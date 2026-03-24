(in-package :beadwork)

;;; ============================================================================
;;; SQLite Storage Layer
;;;
;;; CRUD operations against the SQLite database, matching br (beads_rust)
;;; behavior for .beads/beads.db interoperability.
;;; ============================================================================

;;; ---------------------------------------------------------------------------
;;; Timestamp helpers
;;; ---------------------------------------------------------------------------

(defparameter *rfc3339-format*
  '((:year 4) #\- (:month 2) #\- (:day 2)
    #\T (:hour 2) #\: (:min 2) #\: (:sec 2)
    #\. (:nsec 9) :gmt-offset-hhmm)
  "local-time format list producing RFC 3339 timestamps for SQLite storage.")

(defun format-timestamp (timestamp)
  "Format a local-time TIMESTAMP as an RFC 3339 string for SQLite storage."
  (local-time:format-timestring nil timestamp :format *rfc3339-format*))

(defun format-timestamp-utc (timestamp)
  "Format a local-time TIMESTAMP as an RFC 3339 string in UTC."
  (local-time:format-timestring nil timestamp
                                :format *rfc3339-format*
                                :timezone local-time:+utc-zone+))

(defun format-timestamp-or-null (timestamp)
  "Format TIMESTAMP as RFC 3339, or return NIL if TIMESTAMP is NIL."
  (when timestamp (format-timestamp timestamp)))

(defun parse-timestamp (string)
  "Parse an RFC 3339 string from SQLite into a local-time timestamp.
Returns NIL if STRING is NIL or empty."
  (when (and string (plusp (length string)))
    (local-time:parse-timestring string :fail-on-error nil)))

;;; ---------------------------------------------------------------------------
;;; Store class
;;; ---------------------------------------------------------------------------

(defclass store ()
  ((%db :accessor store-db :initform nil)
   (%db-path :initarg :db-path :accessor store-db-path)
   (%prefix :initarg :prefix :accessor store-prefix :initform "bd")))

;;; ---------------------------------------------------------------------------
;;; Open / Close / with-store
;;; ---------------------------------------------------------------------------

(defun open-store (path &key (prefix "bd"))
  "Create a store, connect to SQLite at PATH, apply schema. Returns store instance."
  (let* ((store (make-instance 'store :db-path path :prefix prefix))
         (db (sqlite:connect path)))
    (setf (store-db store) db)
    (apply-schema db)
    store))

(defun close-store (store)
  "Disconnect from the database held by STORE."
  (when (store-db store)
    (sqlite:disconnect (store-db store))
    (setf (store-db store) nil)))

(defmacro with-store ((var path &key (prefix "bd")) &body body)
  "Open a store bound to VAR for the duration of BODY, ensuring close on exit."
  `(let ((,var (open-store ,path :prefix ,prefix)))
     (unwind-protect (progn ,@body)
       (close-store ,var))))

;;; ---------------------------------------------------------------------------
;;; Row → Issue
;;; ---------------------------------------------------------------------------

(defun row-to-issue (row)
  "Convert a row list (from execute-to-list) into an issue instance.
Column order must match the canonical SELECT order used throughout this file."
  (destructuring-bind (id content-hash title description design
                       acceptance-criteria notes status priority issue-type
                       assignee owner estimated-minutes created-at created-by
                       updated-at closed-at close-reason closed-by-session
                       due-at defer-until external-ref source-system source-repo
                       deleted-at deleted-by delete-reason original-type
                       compaction-level compacted-at compacted-at-commit
                       original-size sender ephemeral pinned is-template)
      row
    (declare (ignore design acceptance-criteria notes closed-by-session
                    due-at defer-until source-system
                    deleted-at deleted-by delete-reason original-type
                    compaction-level compacted-at compacted-at-commit
                    original-size sender ephemeral pinned is-template))
    (make-instance 'issue
      :id id
      :content-hash content-hash
      :title title
      :description (or description "")
      :status (parse-status (or status "open"))
      :priority (or priority 2)
      :issue-type (parse-issue-type (or issue-type "task"))
      :assignee assignee
      :owner (or owner "")
      :estimated-minutes estimated-minutes
      :created-at (or (parse-timestamp created-at) (local-time:now))
      :created-by (or created-by "")
      :updated-at (or (parse-timestamp updated-at) (local-time:now))
      :closed-at (parse-timestamp closed-at)
      :close-reason (or close-reason "")
      :source-repo (or source-repo ".")
      :external-ref external-ref)))

(defparameter *issue-select-columns*
  "id, content_hash, title, description, design, acceptance_criteria, notes,
   status, priority, issue_type, assignee, owner, estimated_minutes,
   created_at, created_by, updated_at, closed_at, close_reason, closed_by_session,
   due_at, defer_until, external_ref, source_system, source_repo,
   deleted_at, deleted_by, delete_reason, original_type,
   compaction_level, compacted_at, compacted_at_commit, original_size,
   sender, ephemeral, pinned, is_template"
  "Canonical column list for SELECT on the issues table.")

;;; ---------------------------------------------------------------------------
;;; Create Issue
;;; ---------------------------------------------------------------------------

(defun next-child-number (store parent-id)
  "Get and increment the child counter for PARENT-ID. Returns the next child number."
  (let* ((db (store-db store))
         (current (sqlite:execute-single
                   db
                   "SELECT last_child FROM child_counters WHERE parent_id = ?"
                   parent-id))
         (next (1+ (or current 0))))
    (sqlite:execute-non-query
     db
     "INSERT OR REPLACE INTO child_counters (parent_id, last_child) VALUES (?, ?)"
     parent-id next)
    next))

(defun create-issue (store &key title (type :task) (priority 2) description
                              parent assignee owner)
  "Create a new issue in the database. Returns the created issue object.

If PARENT is given, generates a child ID and adds a parent-child dependency."
  (let* ((db (store-db store))
         (prefix (store-prefix store))
         (now (local-time:now))
         (now-str (format-timestamp now))
         (id (if parent
                 (generate-child-id parent (next-child-number store parent))
                 (generate-id title :prefix prefix)))
         (issue (make-instance 'issue
                  :id id
                  :title title
                  :description (or description "")
                  :status :open
                  :priority priority
                  :issue-type type
                  :assignee assignee
                  :owner (or owner "")
                  :created-at now
                  :created-by ""
                  :updated-at now))
         (content-hash (compute-content-hash issue)))
    (setf (issue-content-hash issue) content-hash)
    (sqlite:execute-non-query
     db
     "INSERT INTO issues (
        id, content_hash, title, description, design, acceptance_criteria, notes,
        status, priority, issue_type, assignee, owner, estimated_minutes,
        created_at, created_by, updated_at, closed_at, close_reason,
        closed_by_session, due_at, defer_until, external_ref, source_system,
        source_repo, deleted_at, deleted_by, delete_reason, original_type,
        compaction_level, compacted_at, compacted_at_commit, original_size,
        sender, ephemeral, pinned, is_template
      ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
     id content-hash title (or description "") "" "" ""
     (status-string :open) priority (issue-type-string type)
     assignee (or owner "") nil
     now-str "" now-str nil "" ""
     nil nil nil "" "."
     nil "" "" ""
     0 nil nil nil
     "" 0 0 0)
    ;; Parent-child dependency
    (when parent
      (sqlite:execute-non-query
       db
       "INSERT INTO dependencies (issue_id, depends_on_id, type, created_at, created_by)
        VALUES (?, ?, 'parent-child', ?, '')"
       id parent now-str))
    (mark-dirty store id)
    issue))

;;; ---------------------------------------------------------------------------
;;; Get Issue
;;; ---------------------------------------------------------------------------

(defun get-issue (store id)
  "Retrieve an issue by ID. Signals ISSUE-NOT-FOUND if not present."
  (let ((rows (sqlite:execute-to-list
               (store-db store)
               (format nil "SELECT ~A FROM issues WHERE id = ?" *issue-select-columns*)
               id)))
    (unless rows
      (error 'issue-not-found :issue-id id :message (format nil "Issue ~A not found" id)))
    (row-to-issue (first rows))))

;;; ---------------------------------------------------------------------------
;;; Update Issue
;;; ---------------------------------------------------------------------------

(defun update-issue (store id &key title status priority description assignee
                                   owner issue-type close-reason)
  "Update specified fields of issue ID. Returns the updated issue.
Only non-NIL keyword arguments cause updates."
  (let* ((db (store-db store))
         (now-str (format-timestamp (local-time:now)))
         (clauses nil)
         (params nil))
    (macrolet ((when-field (key column value)
                 `(when ,key
                    (push ,column clauses)
                    (push ,value params))))
      (when-field title "title = ?" title)
      (when-field status "status = ?" (status-string status))
      (when-field priority "priority = ?" priority)
      (when-field description "description = ?" description)
      (when-field assignee "assignee = ?" assignee)
      (when-field owner "owner = ?" owner)
      (when-field issue-type "issue_type = ?" (issue-type-string issue-type))
      (when-field close-reason "close_reason = ?" close-reason))
    (unless clauses
      (return-from update-issue (get-issue store id)))
    ;; Always update updated_at and content_hash
    (push "updated_at = ?" clauses)
    (push now-str params)
    ;; Build and execute
    (let ((sql (format nil "UPDATE issues SET ~{~A~^, ~} WHERE id = ?"
                       (nreverse clauses))))
      (setf params (nreverse params))
      (apply #'sqlite:execute-non-query db sql (append params (list id))))
    ;; Recompute content hash
    (let* ((issue (get-issue store id))
           (new-hash (compute-content-hash issue)))
      (sqlite:execute-non-query
       db "UPDATE issues SET content_hash = ? WHERE id = ?" new-hash id)
      (setf (issue-content-hash issue) new-hash)
      (mark-dirty store id)
      issue)))

;;; ---------------------------------------------------------------------------
;;; Close Issue
;;; ---------------------------------------------------------------------------

(defun close-issue (store id &key reason)
  "Set issue status to closed, set closed_at to now. Mark dirty."
  (let* ((db (store-db store))
         (now-str (format-timestamp (local-time:now))))
    (sqlite:execute-non-query
     db
     "UPDATE issues SET status = 'closed', closed_at = ?, close_reason = ?, updated_at = ? WHERE id = ?"
     now-str (or reason "") now-str id)
    ;; Recompute content hash
    (let* ((issue (get-issue store id))
           (new-hash (compute-content-hash issue)))
      (sqlite:execute-non-query
       db "UPDATE issues SET content_hash = ? WHERE id = ?" new-hash id)
      (setf (issue-content-hash issue) new-hash)
      (mark-dirty store id)
      issue)))

;;; ---------------------------------------------------------------------------
;;; Reopen Issue
;;; ---------------------------------------------------------------------------

(defun reopen-issue (store id)
  "Set issue status to open, clear closed_at. Mark dirty."
  (let* ((db (store-db store))
         (now-str (format-timestamp (local-time:now))))
    (sqlite:execute-non-query
     db
     "UPDATE issues SET status = 'open', closed_at = NULL, updated_at = ? WHERE id = ?"
     now-str id)
    (let* ((issue (get-issue store id))
           (new-hash (compute-content-hash issue)))
      (sqlite:execute-non-query
       db "UPDATE issues SET content_hash = ? WHERE id = ?" new-hash id)
      (setf (issue-content-hash issue) new-hash)
      (mark-dirty store id)
      issue)))

;;; ---------------------------------------------------------------------------
;;; List Issues
;;; ---------------------------------------------------------------------------

(defun list-issues (store &key status type priority assignee limit offset)
  "List issues with optional filters. Returns a list of issue objects.
ORDER BY priority ASC, created_at DESC."
  (let ((clauses (list "1=1"))
        (params nil))
    (when status
      (push "status = ?" clauses)
      (push (status-string status) params))
    (when type
      (push "issue_type = ?" clauses)
      (push (issue-type-string type) params))
    (when priority
      (push "priority = ?" clauses)
      (push priority params))
    (when assignee
      (push "assignee = ?" clauses)
      (push assignee params))
    (let ((sql (format nil "SELECT ~A FROM issues WHERE ~{~A~^ AND ~} ORDER BY priority ASC, created_at DESC"
                       *issue-select-columns* (nreverse clauses))))
      (when limit
        (setf sql (format nil "~A LIMIT ?" sql))
        (push limit params))
      (when (and offset limit)
        (setf sql (format nil "~A OFFSET ?" sql))
        (push offset params))
      (setf params (nreverse params))
      (let ((rows (apply #'sqlite:execute-to-list (store-db store) sql params)))
        (mapcar #'row-to-issue rows)))))

;;; ---------------------------------------------------------------------------
;;; Ready Issues
;;; ---------------------------------------------------------------------------

(defun ready-issues (store)
  "Return issues that are open/in_progress and not blocked by any unclosed
blocking dependency. Uses NOT EXISTS subquery against dependencies table."
  (let* ((sql (format nil
                "SELECT ~A FROM issues i
                 WHERE i.status IN ('open', 'in_progress')
                 AND i.ephemeral = 0
                 AND (i.is_template = 0 OR i.is_template IS NULL)
                 AND NOT EXISTS (
                   SELECT 1 FROM dependencies d
                   JOIN issues blocker ON blocker.id = d.depends_on_id
                   WHERE d.issue_id = i.id
                   AND d.type IN ('blocks', 'parent-child', 'conditional-blocks', 'waits-for')
                   AND blocker.status NOT IN ('closed', 'tombstone')
                 )
                 ORDER BY i.priority ASC, i.created_at DESC"
                 *issue-select-columns*))
          (rows (sqlite:execute-to-list (store-db store) sql)))
    (mapcar #'row-to-issue rows)))

;;; ---------------------------------------------------------------------------
;;; Blocked Issues
;;; ---------------------------------------------------------------------------

(defun blocked-issues (store)
  "Return issues that are blocked by unclosed dependencies."
  (let* ((sql (format nil
                 "SELECT ~A FROM issues i
                  WHERE i.status IN ('open', 'in_progress')
                  AND EXISTS (
                    SELECT 1 FROM dependencies d
                    JOIN issues blocker ON blocker.id = d.depends_on_id
                    WHERE d.issue_id = i.id
                    AND d.type IN ('blocks', 'parent-child', 'conditional-blocks', 'waits-for')
                    AND blocker.status NOT IN ('closed', 'tombstone')
                  )
                  ORDER BY i.priority ASC, i.created_at DESC"
                 *issue-select-columns*))
         (rows (sqlite:execute-to-list (store-db store) sql)))
    (mapcar #'row-to-issue rows)))

;;; ---------------------------------------------------------------------------
;;; Search Issues
;;; ---------------------------------------------------------------------------

(defun search-issues (store query)
  "Simple LIKE search on title and description. Returns matching issues."
  (let* ((pattern (format nil "%~A%" query))
         (sql (format nil
                "SELECT ~A FROM issues
                 WHERE (title LIKE ? OR description LIKE ?)
                 ORDER BY priority ASC, created_at DESC"
                *issue-select-columns*))
         (rows (sqlite:execute-to-list (store-db store) sql pattern pattern)))
    (mapcar #'row-to-issue rows)))

;;; ---------------------------------------------------------------------------
;;; Dependencies
;;; ---------------------------------------------------------------------------

(defun add-dependency (store issue-id depends-on-id &key (type :blocks))
  "Insert a dependency relationship between two issues."
  (let ((now-str (format-timestamp (local-time:now))))
    (sqlite:execute-non-query
     (store-db store)
     "INSERT INTO dependencies (issue_id, depends_on_id, type, created_at, created_by)
      VALUES (?, ?, ?, ?, '')"
     issue-id depends-on-id (dependency-type-string type) now-str)
    (mark-dirty store issue-id)))

(defun remove-dependency (store issue-id depends-on-id)
  "Delete a dependency relationship."
  (sqlite:execute-non-query
   (store-db store)
   "DELETE FROM dependencies WHERE issue_id = ? AND depends_on_id = ?"
   issue-id depends-on-id)
  (mark-dirty store issue-id))

(defun list-dependencies (store issue-id)
  "Return a list of dependency objects for ISSUE-ID."
  (let ((rows (sqlite:execute-to-list
               (store-db store)
               "SELECT issue_id, depends_on_id, type, created_at, created_by, metadata, thread_id
                FROM dependencies WHERE issue_id = ?"
               issue-id)))
    (mapcar (lambda (row)
              (destructuring-bind (iid did dtype created-at created-by metadata thread-id) row
                (make-instance 'dependency
                  :issue-id iid
                  :depends-on-id did
                  :dep-type (parse-dependency-type dtype)
                  :created-at (or (parse-timestamp created-at) (local-time:now))
                  :created-by created-by
                  :metadata metadata
                  :thread-id thread-id)))
            rows)))

;;; ---------------------------------------------------------------------------
;;; Labels
;;; ---------------------------------------------------------------------------

(defun add-label (store issue-id label)
  "Add a label to an issue."
  (sqlite:execute-non-query
   (store-db store)
   "INSERT OR IGNORE INTO labels (issue_id, label) VALUES (?, ?)"
   issue-id label)
  (mark-dirty store issue-id))

(defun remove-label (store issue-id label)
  "Remove a label from an issue."
  (sqlite:execute-non-query
   (store-db store)
   "DELETE FROM labels WHERE issue_id = ? AND label = ?"
   issue-id label)
  (mark-dirty store issue-id))

(defun get-labels (store issue-id)
  "Return a list of label strings for ISSUE-ID."
  (let ((rows (sqlite:execute-to-list
               (store-db store)
               "SELECT label FROM labels WHERE issue_id = ?"
               issue-id)))
    (mapcar #'first rows)))

(defun list-all-labels (store)
  "Return a sorted list of all unique labels in the database."
  (let ((rows (sqlite:execute-to-list
               (store-db store)
               "SELECT DISTINCT label FROM labels ORDER BY label")))
    (mapcar #'first rows)))

;;; ---------------------------------------------------------------------------
;;; Comments
;;; ---------------------------------------------------------------------------

(defun add-comment (store issue-id author text)
  "Add a comment to an issue."
  (let ((now-str (format-timestamp (local-time:now))))
    (sqlite:execute-non-query
     (store-db store)
     "INSERT INTO comments (issue_id, author, text, created_at) VALUES (?, ?, ?, ?)"
     issue-id author text now-str)
    (mark-dirty store issue-id)))

(defun list-comments (store issue-id)
  "Return a list of comment objects for ISSUE-ID, ordered by created_at."
  (let ((rows (sqlite:execute-to-list
               (store-db store)
               "SELECT id, issue_id, author, text, created_at
                FROM comments WHERE issue_id = ? ORDER BY created_at ASC"
               issue-id)))
    (mapcar (lambda (row)
              (destructuring-bind (cid iid author body created-at) row
                (make-instance 'comment
                  :id cid
                  :issue-id iid
                  :author author
                  :body body
                  :created-at (or (parse-timestamp created-at) (local-time:now)))))
            rows)))

;;; ---------------------------------------------------------------------------
;;; Dirty Issues Tracking
;;; ---------------------------------------------------------------------------

(defun mark-dirty (store issue-id)
  "Mark an issue as dirty (needs sync export)."
  (sqlite:execute-non-query
   (store-db store)
   "INSERT OR REPLACE INTO dirty_issues (issue_id, marked_at) VALUES (?, ?)"
   issue-id (format-timestamp (local-time:now))))

(defun get-dirty-issues (store)
  "Return a list of issue-id strings that are marked dirty."
  (let ((rows (sqlite:execute-to-list
               (store-db store)
               "SELECT issue_id FROM dirty_issues")))
    (mapcar #'first rows)))

(defun clear-dirty (store &optional issue-id)
  "Clear dirty marks. If ISSUE-ID is given, clear only that issue; otherwise clear all."
  (if issue-id
      (sqlite:execute-non-query
       (store-db store)
       "DELETE FROM dirty_issues WHERE issue_id = ?"
       issue-id)
      (sqlite:execute-non-query
       (store-db store)
       "DELETE FROM dirty_issues")))
