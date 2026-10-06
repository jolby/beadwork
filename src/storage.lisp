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
    #\. (:nsec 6) :gmt-offset-hhmm)
  "local-time format list producing RFC 3339 timestamps for SQLite storage.")

(defun %clip-nsec (ts-str)
  "Clip nanosecond precision (9+ digits) to microsecond (6 digits)
and normalize timezone offset to ±HH:MM format with colon for RFC 3339.
local-time always emits 9+ fractional digits regardless of (:nsec N)."
  (let* ((dot-pos (position #\. ts-str))
         (sign-pos (when dot-pos
                     (or (position #\+ ts-str :start dot-pos)
                         (position #\- ts-str :start dot-pos)))))
    (if (not dot-pos)
        ts-str
        ;; Build: YYYY-MM-DDTHH:MM:SS + 6 fractional digits + normalized offset
        (let* ((prefix (subseq ts-str 0 (1+ dot-pos)))
               (fraction (subseq ts-str (1+ dot-pos)
                                 (min (+ dot-pos 7) (or sign-pos (length ts-str)))))
               (offset (when sign-pos (subseq ts-str sign-pos))))
          ;; Pad fraction to 6 digits if needed
          (let ((fraction-padded (if (< (length fraction) 6)
                                     (concatenate 'string fraction
                                                  (make-string (- 6 (length fraction))
                                                               :initial-element #\0))
                                     fraction)))
            (if offset
                ;; Insert colon: +0000 → +00:00, -0700 → -07:00
                (if (= 5 (length offset))
                    (concatenate 'string prefix fraction-padded
                                 (subseq offset 0 3) ":" (subseq offset 3))
                    (concatenate 'string prefix fraction-padded offset))
                (concatenate 'string prefix fraction-padded)))))))

(defun format-timestamp (timestamp)
  "Format a local-time TIMESTAMP as an RFC 3339 string with microsecond precision."
  (%clip-nsec (local-time:format-timestring nil timestamp :format *rfc3339-format*)))

(defun format-timestamp-utc (timestamp)
  "Format a local-time TIMESTAMP as an RFC 3339 string in UTC with microsecond precision."
  (%clip-nsec (local-time:format-timestring nil timestamp
                                             :format *rfc3339-format*
                                             :timezone local-time:+utc-zone+)))

(defun format-timestamp-or-null (timestamp)
  "Format TIMESTAMP as RFC 3339, or return NIL if TIMESTAMP is NIL."
  (when timestamp (format-timestamp timestamp)))

(defun %parse-legacy-space-timestamp (string)
  "Parse br's legacy UTC format \"YYYY-MM-DD HH:MM:SS[.ffffff]\" — space
separator, no timezone offset, optional fractional seconds (as written by the
old beads_rust bd tool). Returns a local-time timestamp, or NIL if STRING does
not match or is an invalid date."
  (multiple-value-bind (match groups)
      (cl-ppcre:scan-to-strings
       "^(\\d{4})-(\\d{2})-(\\d{2}) (\\d{2}):(\\d{2}):(\\d{2})(?:\\.(\\d{1,6}))?$"
       (or string ""))
    (declare (ignore match))
    (when groups
      (handler-case
          (local-time:encode-timestamp
           (if (aref groups 6)
               (* 1000 (parse-integer (aref groups 6))) ; fraction is microseconds
               0)
           (parse-integer (aref groups 5)) ; sec
           (parse-integer (aref groups 4)) ; min
           (parse-integer (aref groups 3)) ; hour
           (parse-integer (aref groups 2)) ; day
           (parse-integer (aref groups 1)) ; month
           (parse-integer (aref groups 0)) ; year
           :timezone local-time:+utc-zone+)
        (error () nil)))))

(defun parse-timestamp (string)
  "Parse an RFC 3339 string from SQLite into a local-time timestamp.
Falls back to br's legacy space-separated UTC format (see
%PARSE-LEGACY-SPACE-TIMESTAMP) which LOCAL-TIME's parser rejects. Without the
fallback, readers substitute wall-clock now for such rows, making JSONL export
non-idempotent. Returns NIL if STRING is NIL, empty, or unparsable."
  (when (and string (plusp (length string)))
    (or (local-time:parse-timestring string :fail-on-error nil)
        (%parse-legacy-space-timestamp string))))

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

(defun %normalize-timestamps (db)
  "Normalize all datetime columns to microsecond-precision RFC 3339 UTC.
Fixes historical timestamps that have nanosecond precision or ±HHMM offsets
which br's Rust chrono parser cannot handle."
  (handler-case
      (let ((rows (sqlite:execute-to-list db
                    "SELECT id, created_at, updated_at, closed_at,
                            due_at, defer_until, deleted_at, compacted_at
                     FROM issues")))
        (dolist (row rows)
          (destructuring-bind (id created-at updated-at closed-at
                               due-at defer-until deleted-at compacted-at)
              row
            (let ((updates nil))
              (flet ((needs-normalize (val label)
                       (let ((ts (when (and val (plusp (length val)))
                                   (parse-timestamp val))))
                         (when ts
                           (let ((formatted (format-timestamp-utc ts)))
                             (unless (string= val formatted)
                               (push (list label formatted) updates)))))))
                (needs-normalize created-at "created_at")
                (needs-normalize updated-at "updated_at")
                (needs-normalize closed-at "closed_at")
                (needs-normalize due-at "due_at")
                (needs-normalize defer-until "defer_until")
                (needs-normalize deleted-at "deleted_at")
                (needs-normalize compacted-at "compacted_at"))
              (when updates
                (let* ((set-clauses
                         (format nil "~{~A = ?~^, ~}"
                                 (mapcar #'first updates)))
                       (vals (mapcar #'second updates)))
                  (apply #'sqlite:execute-non-query db
                         (format nil "UPDATE issues SET ~A WHERE id = ?"
                                 set-clauses)
                         (append vals (list id)))))))))
    (error () nil)))

(defun %normalize-dependency-timestamps (db)
  "Normalize the dependencies.created_at column to microsecond-precision
RFC 3339 UTC, matching the issues-table handling in %NORMALIZE-TIMESTAMPS.
Br's legacy bd tool stored space-separated timestamps without an offset here;
leaving them unparsed makes JSONL export substitute wall-clock now (bd-e84)."
  (handler-case
      (let ((rows (sqlite:execute-to-list db
                    "SELECT issue_id, depends_on_id, created_at
                     FROM dependencies")))
        (dolist (row rows)
          (destructuring-bind (issue-id depends-on-id created-at) row
            (let ((ts (when (and created-at (plusp (length created-at)))
                        (parse-timestamp created-at))))
              (when ts
                (let ((formatted (format-timestamp-utc ts)))
                  (unless (string= created-at formatted)
                    (sqlite:execute-non-query db
                      "UPDATE dependencies SET created_at = ?
                       WHERE issue_id = ? AND depends_on_id = ?"
                      formatted issue-id depends-on-id))))))))
    (error () nil)))

(defun open-store (path &key (prefix "bd"))
  "Create a store, connect to SQLite at PATH, apply schema. Returns store instance.
The connection uses cl-sqlite-deep's default busy timeout (5000ms)."
  (let* ((store (make-instance 'store :db-path path :prefix prefix))
         (db (sqlite:connect path)))
    (setf (store-db store) db)
    (apply-schema db)
    ;; Normalize historical timestamps for br interop
    (%normalize-timestamps db)
    ;; br's legacy timestamp format also lives in dependencies.created_at
    (%normalize-dependency-timestamps db)
    store))

(defun close-store (store)
  "Disconnect from the database held by STORE."
  (when (store-db store)
    (sqlite:disconnect (store-db store))
    (setf (store-db store) nil)))

(defmacro with-store ((var path &key (prefix "bd")) &body body)
  "Open a store bound to VAR for the duration of BODY, ensuring close on exit.
The connection uses cl-sqlite-deep's default busy timeout (5000ms)."
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
    (declare (ignore design acceptance-criteria closed-by-session
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
      :notes (or notes "")
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

(defun issue-id-exists-p (store id)
  "Return true when ID is already used by an issue row."
  (and (sqlite:execute-single (store-db store)
                              "SELECT 1 FROM issues WHERE id = ?" id)
       t))

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

(defun next-free-child-id (store parent-id)
  "Return the next free dotted child id for PARENT-ID.

Advances CHILD-COUNTERS, but skips candidate ids that already exist -- e.g.
children imported with explicit ids whose counter was never reconciled
(spec 11.2) -- so the INSERT cannot fail with a raw UNIQUE constraint (bd-pvz)."
  (loop for number = (next-child-number store parent-id)
        for candidate = (generate-child-id parent-id number)
        unless (issue-id-exists-p store candidate)
          do (return candidate)))

(defun create-issue (store &key title (type :task) (priority 2) description
                              parent assignee owner source-repo)
  "Create a new issue in the database. Returns the created issue object.

If PARENT is given, generates a child ID and adds a parent-child dependency."
  (let* ((db (store-db store))
         (prefix (store-prefix store))
         (now (local-time:now))
         (now-str (format-timestamp now))
         (id (if parent
                 (next-free-child-id store parent)
                 ;; Collision-proof: retry the random hash instead of letting
                 ;; an id clash surface as a raw UNIQUE-constraint error
                 ;; (bd-pvz).
                 (generate-unique-id
                  title :prefix prefix
                  :exists-p (lambda (candidate)
                              (issue-id-exists-p store candidate)))))
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
                  :updated-at now
                  ;; source-repo is written by the INSERT below; set it on the
                  ;; returned object too so it matches the DB row (bd-x50).
                  ;; It is not part of compute-content-hash (matches br).
                  :source-repo (or source-repo ".")))
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
     nil nil nil "" (or source-repo ".")
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

(defun find-issue (store id)
  "Like GET-ISSUE, but return NIL instead of signalling when ID is unknown.

Use when walking graph edges: a dangling endpoint (legacy data, or a row
written before validation existed) must not abort a listing or render (bd-wux)."
  (handler-case (get-issue store id)
    (issue-not-found () nil)))

;;; ---------------------------------------------------------------------------
;;; Update Issue
;;; ---------------------------------------------------------------------------

(defun update-issue (store id &key title status priority description assignee
                                   notes owner issue-type close-reason source-repo)
  "Update specified fields of issue ID. Returns the updated issue.
Only non-NIL keyword arguments cause updates. Status transitions into a
terminal state (:closed or :tombstone) stamp closed_at, and transitions out of
one clear it, satisfying the schema CHECK constraint (bd-lu5)."
  (let* ((db (store-db store))
         (now-str (format-timestamp (local-time:now)))
         (current-status (issue-status (get-issue store id)))
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
      (when-field notes "notes = ?" notes)
      (when-field owner "owner = ?" owner)
      (when-field issue-type "issue_type = ?" (issue-type-string issue-type))
      (when-field close-reason "close_reason = ?" close-reason)
      (when-field source-repo "source_repo = ?" source-repo))
    (unless clauses
      (return-from update-issue (get-issue store id)))
    ;; Maintain the closed_at invariant on status transitions (bd-lu5):
    ;; terminal statuses require closed_at, non-terminal statuses forbid it.
    (when (and status (not (eq status current-status)))
      (cond
        ((status-terminal-p status)
         (push "closed_at = ?" clauses)
         (push now-str params))
        ((status-terminal-p current-status)
         (push "closed_at = NULL" clauses))))
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

(defun list-issues (store &key status type priority assignee limit offset source-repo)
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
    (when source-repo
      (let ((repos (if (listp source-repo) source-repo (list source-repo))))
        (push (format nil "source_repo IN (~{~A~^,~})"
                      (mapcar (constantly "?") repos))
              clauses)
        (dolist (r (reverse repos))
          (push r params))))
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

(defun ready-issues (store &key source-repo)
  "Return issues that are open/in_progress and not blocked by any unclosed
blocking dependency. Uses NOT EXISTS subquery against dependencies table.
SOURCE-REPO may be a string or a list of strings."
  (let* ((repos (when source-repo
                  (if (listp source-repo) source-repo (list source-repo))))
         (repo-clause (if repos
                          (format nil "AND i.source_repo IN (~{~A~^,~})"
                                  (mapcar (constantly "?") repos))
                          ""))
         (sql (format nil
                "SELECT ~A FROM issues i
                 WHERE i.status IN ('open', 'in_progress')
                 AND i.ephemeral = 0
                 AND (i.is_template = 0 OR i.is_template IS NULL)
                 ~A
                 AND NOT EXISTS (
                   SELECT 1 FROM dependencies d
                   JOIN issues blocker ON blocker.id = d.depends_on_id
                   WHERE d.issue_id = i.id
                   AND d.type IN ('blocks', 'conditional-blocks', 'waits-for')
                   AND blocker.status NOT IN ('closed', 'tombstone')
                 )
                 ORDER BY i.priority ASC, i.created_at DESC"
                 *issue-select-columns*
                 repo-clause))
         (rows (if repos
                   (apply #'sqlite:execute-to-list (store-db store) sql repos)
                   (sqlite:execute-to-list (store-db store) sql))))
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
                    AND d.type IN ('blocks', 'conditional-blocks', 'waits-for')
                    AND blocker.status NOT IN ('closed', 'tombstone')
                  )
                  ORDER BY i.priority ASC, i.created_at DESC"
                 *issue-select-columns*))
         (rows (sqlite:execute-to-list (store-db store) sql)))
    (mapcar #'row-to-issue rows)))

;;; ---------------------------------------------------------------------------
;;; Delete Issue
;;; ---------------------------------------------------------------------------

(defun delete-issue (store id)
  "Delete an issue by ID. Returns the deleted issue's ID on success.
Signals ISSUE-NOT-FOUND if the issue doesn't exist."
  (get-issue store id)                    ; validate existence
  (sqlite:execute-non-query
   (store-db store)
   "DELETE FROM issues WHERE id = ?"
   id)
  id)

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
  "Insert a dependency relationship between two issues.

Both endpoints must be existing issues: signals ISSUE-NOT-FOUND otherwise, so a
typo or a pasted description cannot become a dangling edge (bd-wux)."
  (get-issue store issue-id)
  (get-issue store depends-on-id)
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

(defun %dependency-from-row (row)
  "Build a DEPENDENCY instance from a dependencies-table ROW."
  (destructuring-bind (iid did dtype created-at created-by metadata thread-id) row
    (make-instance 'dependency
      :issue-id iid
      :depends-on-id did
      :dep-type (parse-dependency-type dtype)
      :created-at (or (parse-timestamp created-at) (local-time:now))
      :created-by created-by
      :metadata metadata
      :thread-id thread-id)))

(defun list-dependencies (store issue-id)
  "Return a list of dependency objects for ISSUE-ID (outgoing edges: this
issue depends on the returned depends-on ids)."
  (mapcar #'%dependency-from-row
          (sqlite:execute-to-list
           (store-db store)
           "SELECT issue_id, depends_on_id, type, created_at, created_by, metadata, thread_id
            FROM dependencies WHERE issue_id = ?"
           issue-id)))

(defun list-dependents (store issue-id)
  "Return a list of dependency objects for edges pointing AT ISSUE-ID
(incoming edges: the returned issue-ids depend on ISSUE-ID). Mirrors
LIST-DEPENDENCIES."
  (mapcar #'%dependency-from-row
          (sqlite:execute-to-list
           (store-db store)
           "SELECT issue_id, depends_on_id, type, created_at, created_by, metadata, thread_id
            FROM dependencies WHERE depends_on_id = ?"
           issue-id)))

(defun get-parent-id (store issue-id)
  "Return the parent id of ISSUE-ID from a parent-child dependency, or NIL."
  (sqlite:execute-single
   (store-db store)
   "SELECT depends_on_id FROM dependencies
    WHERE issue_id = ? AND type = 'parent-child'
    LIMIT 1"
   issue-id))

(defun list-children (store parent-id)
  "Return the direct children of PARENT-ID as issue objects, ordered by id.
Walks parent-child dependencies whose depends_on_id is PARENT-ID. A dangling
child id (legacy data) is skipped rather than signalling (bd-wux)."
  (loop for row in (sqlite:execute-to-list
                    (store-db store)
                    "SELECT issue_id FROM dependencies
                     WHERE depends_on_id = ? AND type = 'parent-child'
                     ORDER BY issue_id"
                    parent-id)
        for child = (find-issue store (first row))
        when child collect child))

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

(defun edit-comment (store comment-id text)
  "Update the text of comment COMMENT-ID. Signals an error if not found."
  (let* ((db (store-db store))
         (row (sqlite:execute-to-list
               db
               "SELECT issue_id FROM comments WHERE id = ?"
               comment-id)))
    (unless row
      (error 'beadwork-error :message
             (format nil "Comment ~A not found" comment-id)))
    (sqlite:execute-non-query
     db
     "UPDATE comments SET text = ? WHERE id = ?"
     text comment-id)
    (mark-dirty store (first (first row)))
    (values)))

(defun delete-comment (store comment-id)
  "Delete comment COMMENT-ID. No-op if not found."
  (let* ((db (store-db store))
         (row (sqlite:execute-to-list
               db
               "SELECT issue_id FROM comments WHERE id = ?"
               comment-id)))
    (when row
      (sqlite:execute-non-query
       db
       "DELETE FROM comments WHERE id = ?"
       comment-id)
      (mark-dirty store (first (first row)))))
  (values))

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

;;; ============================================================================
;;; Session Management
;;; ============================================================================

(defparameter *session-stale-hours* 4
  "Hours of inactivity after which a session is considered stale.")

(defun start-session (store &key agent-id agent-session-id)
  "Create a new session. Automatically ends any stale sessions first.
Returns a plist with session data or NIL if a session is already active."
  (let ((db (store-db store)))
    (auto-end-stale-sessions store)
    (let ((current (get-current-session store)))
      (when current
        (return-from start-session nil)))
    (let* ((id (generate-unique-id
                "session" :prefix "S"
                ;; Sessions use their own id space; check the sessions table
                ;; rather than issues (bd-pvz).
                :exists-p (lambda (candidate)
                            (and (sqlite:execute-single
                                  db "SELECT 1 FROM sessions WHERE id = ?" candidate)
                                 t))))
           (now-str (format-timestamp (local-time:now))))
      (sqlite:execute-non-query
       db
       "INSERT INTO sessions (id, started_at, agent_id, agent_session_id)
        VALUES (?, ?, ?, ?)"
       id now-str (or agent-id "") (or agent-session-id ""))
      (list :id id
            :started-at (parse-timestamp now-str)
            :ended-at nil
            :active-issue-id nil
            :handoff-notes ""
            :last-action ""
            :agent-id (or agent-id "")
            :agent-session-id (or agent-session-id "")))))

(defun end-session (store session-id &key notes)
  "End session SESSION-ID, setting ended_at and handoff notes."
  (let ((db (store-db store))
        (now-str (format-timestamp (local-time:now))))
    (sqlite:execute-non-query
     db
     "UPDATE sessions SET ended_at = ?, handoff_notes = ? WHERE id = ?"
     now-str (or notes "") session-id)
    (values)))

(defun get-current-session (store)
  "Return the active session as a plist, or NIL if none."
  (let ((rows (sqlite:execute-to-list
               (store-db store)
               "SELECT id, started_at, ended_at, active_issue_id,
                       handoff_notes, last_action, agent_id, agent_session_id
                FROM sessions WHERE ended_at IS NULL
                ORDER BY started_at DESC LIMIT 1")))
    (when rows
      (destructuring-bind (id started-at ended-at active-issue-id
                           handoff-notes last-action agent-id agent-session-id)
          (first rows)
        (list :id id
              :started-at (parse-timestamp started-at)
              :ended-at (when ended-at (parse-timestamp ended-at))
              :active-issue-id active-issue-id
              :handoff-notes (or handoff-notes "")
              :last-action (or last-action "")
              :agent-id (or agent-id "")
              :agent-session-id (or agent-session-id ""))))))

(defun get-last-session (store)
  "Return the most recently ended session as a plist, or NIL if none."
  (let ((rows (sqlite:execute-to-list
               (store-db store)
               "SELECT id, started_at, ended_at, active_issue_id,
                       handoff_notes, last_action, agent_id, agent_session_id
                FROM sessions WHERE ended_at IS NOT NULL
                ORDER BY ended_at DESC LIMIT 1")))
    (when rows
      (destructuring-bind (id started-at ended-at active-issue-id
                           handoff-notes last-action agent-id agent-session-id)
          (first rows)
        (list :id id
              :started-at (parse-timestamp started-at)
              :ended-at (parse-timestamp ended-at)
              :active-issue-id active-issue-id
              :handoff-notes (or handoff-notes "")
              :last-action (or last-action "")
              :agent-id (or agent-id "")
              :agent-session-id (or agent-session-id ""))))))

(defun set-session-work (store session-id issue-id)
  "Set the active issue for SESSION-ID."
  (sqlite:execute-non-query
   (store-db store)
   "UPDATE sessions SET active_issue_id = ? WHERE id = ?"
   issue-id session-id)
  (values))

(defun record-session-action (store session-id action-text)
  "Record a breadcrumb action for SESSION-ID and auto-comment on the active
issue if one is set."
  (let ((db (store-db store)))
    (sqlite:execute-non-query
     db
     "UPDATE sessions SET last_action = ? WHERE id = ?"
     action-text session-id)
    ;; Auto-comment on the active issue
    (let ((rows (sqlite:execute-to-list
                 db
                 "SELECT active_issue_id FROM sessions WHERE id = ?"
                 session-id)))
      (when (and rows (first rows) (first (first rows)))
        (add-comment store (first (first rows))
                     "[session]"
                     action-text)))
    (values)))

(defun auto-end-stale-sessions (store)
  "End any active sessions that have been idle for more than
*session-stale-hours*.  Marks them with an abandonment note."
  (let* ((db (store-db store))
         (threshold (local-time:timestamp-
                     (local-time:now)
                     *session-stale-hours*
                     :hour))
         (threshold-str (format-timestamp threshold))
         (rows (sqlite:execute-to-list
                db
                "SELECT id FROM sessions
                 WHERE ended_at IS NULL AND started_at < ?"
                threshold-str)))
    (dolist (row rows)
      (end-session store (first row)
                   :notes "[auto-ended] Session abandoned (stale >4h)"))
    (values)))

;;; ============================================================================
;;; Stats
;;; ============================================================================

(defun issue-stats (store)
  "Return aggregate statistics as a plist with keys :total,
:counts-by-status, :counts-by-priority, :counts-by-type, :ready-count."
  (let ((db (store-db store)))
    (flet ((qcount (sql &rest params)
             (or (apply #'sqlite:execute-single db sql params) 0)))
      (list
       :total (qcount "SELECT COUNT(*) FROM issues WHERE status != 'tombstone'")
       :counts-by-status
       (list :open (qcount "SELECT COUNT(*) FROM issues WHERE status = 'open'")
             :in-progress (qcount "SELECT COUNT(*) FROM issues WHERE status = 'in_progress'")
             :blocked (qcount "SELECT COUNT(*) FROM issues WHERE status = 'blocked'")
             :deferred (qcount "SELECT COUNT(*) FROM issues WHERE status = 'deferred'")
             :closed (qcount "SELECT COUNT(*) FROM issues WHERE status = 'closed'"))
       :counts-by-priority
       (loop for p from 0 to 4
             collect (intern (format nil "P~D" p) :keyword)
             collect (qcount "SELECT COUNT(*) FROM issues WHERE priority = ? AND status NOT IN ('closed', 'tombstone')" p))
       :counts-by-type
       (list :bug (qcount "SELECT COUNT(*) FROM issues WHERE issue_type = 'bug' AND status NOT IN ('closed', 'tombstone')")
             :feature (qcount "SELECT COUNT(*) FROM issues WHERE issue_type = 'feature' AND status NOT IN ('closed', 'tombstone')")
             :task (qcount "SELECT COUNT(*) FROM issues WHERE issue_type = 'task' AND status NOT IN ('closed', 'tombstone')")
             :epic (qcount "SELECT COUNT(*) FROM issues WHERE issue_type = 'epic' AND status NOT IN ('closed', 'tombstone')")
             :chore (qcount "SELECT COUNT(*) FROM issues WHERE issue_type = 'chore' AND status NOT IN ('closed', 'tombstone')")
             :docs (qcount "SELECT COUNT(*) FROM issues WHERE issue_type = 'docs' AND status NOT IN ('closed', 'tombstone')"))
       :ready-count (length (ready-issues store))))))
