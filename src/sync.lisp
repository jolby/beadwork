(in-package :beadwork)

;;; ============================================================================
;;; JSONL Import/Export — br (beads_rust) interoperability
;;;
;;; Each line of issues.jsonl is a JSON object representing a single issue with
;;; its dependencies embedded inline. This matches br's sync format exactly.
;;; ============================================================================

;;; ----------------------------------------------------------------------------
;;; Omit-empty helper
;;; ----------------------------------------------------------------------------

(defun set-when-present (ht key value)
  "Set KEY in hash-table HT to VALUE only if VALUE is non-nil and non-empty.
Matches br's serde omitempty behavior."
  (when (and value
             (not (and (stringp value) (zerop (length value)))))
    (setf (gethash key ht) value)))

;;; ----------------------------------------------------------------------------
;;; Issue → JSON hash-table
;;; ----------------------------------------------------------------------------

(defun dependency-to-json-object (dep)
  "Convert a dependency object to a hash-table matching br's JSON format."
  (let ((ht (make-hash-table :test 'equal)))
    (setf (gethash "issue_id" ht) (dependency-issue-id dep))
    (setf (gethash "depends_on_id" ht) (dependency-depends-on-id dep))
    (setf (gethash "type" ht) (dependency-type-string (dependency-dep-type dep)))
    (setf (gethash "created_at" ht) (format-timestamp-utc (dependency-created-at dep)))
    (setf (gethash "created_by" ht) (or (dependency-created-by dep) ""))
    (setf (gethash "metadata" ht) (or (dependency-metadata dep) "{}"))
    (setf (gethash "thread_id" ht) (or (dependency-thread-id dep) ""))
    ht))

(defun issue-to-json-object (issue &key labels dependencies)
  "Convert an issue CLOS object to a hash-table matching br's JSONL format.
LABELS is a list of label strings. DEPENDENCIES is a list of dependency objects.
Fields with nil/empty values are omitted (serde omitempty behavior)."
  (let ((ht (make-hash-table :test 'equal)))
    ;; Required fields — always present
    (setf (gethash "id" ht) (issue-id issue))
    (setf (gethash "title" ht) (issue-title issue))
    (setf (gethash "status" ht) (status-string (issue-status issue)))
    (setf (gethash "priority" ht) (issue-priority issue))
    (setf (gethash "issue_type" ht) (issue-type-string (issue-type issue)))
    (setf (gethash "created_at" ht) (format-timestamp-utc (issue-created-at issue)))
    (setf (gethash "updated_at" ht) (format-timestamp-utc (issue-updated-at issue)))
    (setf (gethash "source_repo" ht) (or (issue-source-repo issue) "."))
    (setf (gethash "compaction_level" ht) 0)
    (setf (gethash "original_size" ht) 0)

    ;; created_by — br always emits this
    (setf (gethash "created_by" ht) (or (issue-created-by issue) ""))

    ;; Optional string fields — omit when empty
    (set-when-present ht "description" (issue-description issue))
    (set-when-present ht "design" (issue-design issue))
    (set-when-present ht "acceptance_criteria" (issue-acceptance-criteria issue))
    (set-when-present ht "notes" (issue-notes issue))
    (set-when-present ht "assignee" (issue-assignee issue))
    (set-when-present ht "owner" (issue-owner issue))
    (set-when-present ht "close_reason" (issue-close-reason issue))
    (set-when-present ht "external_ref" (issue-external-ref issue))
    (set-when-present ht "content_hash" (issue-content-hash issue))

    ;; Optional timestamp fields
    (let ((closed-at (format-timestamp-utc (issue-closed-at issue))))
      (set-when-present ht "closed_at" closed-at))

    ;; Optional integer fields
    (when (issue-estimated-minutes issue)
      (setf (gethash "estimated_minutes" ht) (issue-estimated-minutes issue)))

    ;; Labels — omit if empty
    (when labels
      (setf (gethash "labels" ht) (coerce labels 'vector)))

    ;; Dependencies — omit if empty (matching br behavior)
    (when dependencies
      (setf (gethash "dependencies" ht)
            (coerce (mapcar #'dependency-to-json-object dependencies) 'vector)))

    ht))

;;; ----------------------------------------------------------------------------
;;; JSON hash-table → Issue
;;; ----------------------------------------------------------------------------

(defun json-object-to-dependency (ht)
  "Convert a parsed JSON hash-table to a dependency object."
  (make-instance 'dependency
    :issue-id      (gethash "issue_id" ht)
    :depends-on-id (gethash "depends_on_id" ht)
    :dep-type      (parse-dependency-type (gethash "type" ht "blocks"))
    :created-at    (or (parse-timestamp (gethash "created_at" ht))
                       (local-time:now))
    :created-by    (gethash "created_by" ht "")
    :metadata      (gethash "metadata" ht "{}")
    :thread-id     (gethash "thread_id" ht "")))

(defun json-object-to-issue (ht)
  "Convert a parsed JSON hash-table back to an issue CLOS object.
Dependencies are stored in the issue's dependencies slot."
  (let* ((deps-raw (gethash "dependencies" ht))
         (deps (when deps-raw
                 (map 'list #'json-object-to-dependency deps-raw)))
         (issue (make-instance 'issue
                  :id                  (gethash "id" ht)
                  :title               (gethash "title" ht)
                  :description         (gethash "description" ht "")
                  :design              (gethash "design" ht "")
                  :acceptance-criteria (gethash "acceptance_criteria" ht "")
                  :notes               (gethash "notes" ht "")
                  :status              (parse-status (gethash "status" ht "open"))
                  :priority            (gethash "priority" ht 2)
                  :issue-type          (parse-issue-type
                                        (gethash "issue_type" ht "task"))
                  :assignee            (gethash "assignee" ht nil)
                  :owner               (gethash "owner" ht "")
                  :estimated-minutes   (gethash "estimated_minutes" ht nil)
                  :created-at          (or (parse-timestamp
                                            (gethash "created_at" ht))
                                           (local-time:now))
                  :created-by          (gethash "created_by" ht "")
                  :updated-at          (or (parse-timestamp
                                            (gethash "updated_at" ht))
                                           (local-time:now))
                  :closed-at           (parse-timestamp
                                        (gethash "closed_at" ht))
                  :close-reason        (gethash "close_reason" ht "")
                  :source-repo         (gethash "source_repo" ht ".")
                  :external-ref        (gethash "external_ref" ht nil)
                  :content-hash        (gethash "content_hash" ht nil)
                  :dependencies        deps)))
    issue))

;;; ----------------------------------------------------------------------------
;;; Export JSONL
;;; ----------------------------------------------------------------------------

(defun %row-to-issue (row)
  "Convert a SQLite result row (list) from the issues table into an issue object.
Column order matches the SELECT in export-jsonl."
  (destructuring-bind (id content-hash title description design
                       acceptance-criteria notes status priority issue-type
                       assignee owner estimated-minutes created-at created-by
                       updated-at closed-at close-reason source-repo
                       external-ref)
      row
    (make-instance 'issue
      :id                  id
      :content-hash        content-hash
      :title               title
      :description         (or description "")
      :design              (or design "")
      :acceptance-criteria (or acceptance-criteria "")
      :notes               (or notes "")
      :status              (parse-status status)
      :priority            (or priority 2)
      :issue-type          (parse-issue-type (or issue-type "task"))
      :assignee            assignee
      :owner               (or owner "")
      :estimated-minutes   estimated-minutes
      :created-at          (or (parse-timestamp created-at) (local-time:now))
      :created-by          (or created-by "")
      :updated-at          (or (parse-timestamp updated-at) (local-time:now))
      :closed-at           (parse-timestamp closed-at)
      :close-reason        (or close-reason "")
      :source-repo         (or source-repo ".")
      :external-ref        external-ref)))

(defun %row-to-dependency (row)
  "Convert a SQLite result row from the dependencies table into a dependency object."
  (destructuring-bind (issue-id depends-on-id dep-type created-at
                       created-by metadata thread-id)
      row
    (make-instance 'dependency
      :issue-id      issue-id
      :depends-on-id depends-on-id
      :dep-type      (parse-dependency-type dep-type)
      :created-at    (or (parse-timestamp created-at) (local-time:now))
      :created-by    (or created-by "")
      :metadata      (or metadata "{}")
      :thread-id     (or thread-id ""))))

(defun %fetch-labels (db issue-id)
  "Fetch all labels for ISSUE-ID from DB."
  (mapcar #'first
          (sqlite:execute-to-list db
            "SELECT label FROM labels WHERE issue_id = ? ORDER BY label"
            issue-id)))

(defun %fetch-dependencies (db issue-id)
  "Fetch all dependencies for ISSUE-ID from DB."
  (mapcar #'%row-to-dependency
          (sqlite:execute-to-list db
            "SELECT issue_id, depends_on_id, type, created_at,
                    created_by, metadata, thread_id
             FROM dependencies WHERE issue_id = ? ORDER BY depends_on_id"
            issue-id)))

(defun export-jsonl (store path)
  "Export all non-tombstone issues from STORE to a JSONL file at PATH.
Each line is a JSON object with embedded dependencies. Issues are sorted by
created_at. Clears dirty flags after successful export."
  (let ((db (store-db store))
        (path (pathname path)))
    (ensure-directories-exist path)
    (let ((rows (sqlite:execute-to-list db
                  "SELECT id, content_hash, title, description, design,
                          acceptance_criteria, notes, status, priority,
                          issue_type, assignee, owner, estimated_minutes,
                          created_at, created_by, updated_at, closed_at,
                          close_reason, source_repo, external_ref
                   FROM issues
                   WHERE status != 'tombstone'
                   ORDER BY created_at ASC")))
      (with-open-file (out path :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create
                                :external-format :utf-8)
        (dolist (row rows)
          (let* ((issue (%row-to-issue row))
                 (labels (%fetch-labels db (issue-id issue)))
                 (deps (%fetch-dependencies db (issue-id issue)))
                 (json-ht (issue-to-json-object issue
                                                :labels labels
                                                :dependencies deps)))
            (jzon:stringify json-ht :stream out :pretty nil)
            (terpri out))))
      ;; Clear dirty flags
      (sqlite:execute-non-query db "DELETE FROM dirty_issues")
      path)))

;;; ----------------------------------------------------------------------------
;;; Import JSONL
;;; ----------------------------------------------------------------------------

(defun %upsert-issue (db issue)
  "Insert or replace an issue into the database."
  (sqlite:execute-non-query db
    "INSERT OR REPLACE INTO issues
       (id, content_hash, title, description, design, acceptance_criteria,
        notes, status, priority, issue_type, assignee, owner,
        estimated_minutes, created_at, created_by, updated_at, closed_at,
        close_reason, source_repo, external_ref)
     VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
    (issue-id issue)
    (issue-content-hash issue)
    (issue-title issue)
    (issue-description issue)
    (issue-design issue)
    (issue-acceptance-criteria issue)
    (issue-notes issue)
    (status-string (issue-status issue))
    (issue-priority issue)
    (issue-type-string (issue-type issue))
    (issue-assignee issue)
    (issue-owner issue)
    (issue-estimated-minutes issue)
    (format-timestamp-utc (issue-created-at issue))
    (issue-created-by issue)
    (format-timestamp-utc (issue-updated-at issue))
    (format-timestamp-utc (issue-closed-at issue))
    (issue-close-reason issue)
    (issue-source-repo issue)
    (issue-external-ref issue)))

(defun %upsert-dependency (db dep)
  "Insert or replace a dependency into the database."
  (sqlite:execute-non-query db
    "INSERT OR REPLACE INTO dependencies
       (issue_id, depends_on_id, type, created_at, created_by, metadata, thread_id)
     VALUES (?, ?, ?, ?, ?, ?, ?)"
    (dependency-issue-id dep)
    (dependency-depends-on-id dep)
    (dependency-type-string (dependency-dep-type dep))
    (format-timestamp-utc (dependency-created-at dep))
    (or (dependency-created-by dep) "")
    (or (dependency-metadata dep) "{}")
    (or (dependency-thread-id dep) "")))

(defun import-jsonl (store path)
  "Import issues from a JSONL file at PATH into STORE.
Each line is parsed as a JSON object and upserted (INSERT OR REPLACE).
Embedded dependencies are also imported."
  (let ((db (store-db store))
        (path (pathname path))
        (count 0))
    (unless (probe-file path)
      (error 'sync-error
             :path path
             :message "JSONL file does not exist"))
    (sqlite:execute-non-query db "BEGIN TRANSACTION")
    (handler-bind
        ((error (lambda (c)
                  (declare (ignore c))
                  (sqlite:execute-non-query db "ROLLBACK"))))
      (with-open-file (in path :direction :input
                               :external-format :utf-8)
        (loop for line = (read-line in nil nil)
              while line
              when (plusp (length (string-trim '(#\Space #\Tab) line)))
                do (let* ((ht (jzon:parse line))
                          (issue (json-object-to-issue ht)))
                     (%upsert-issue db issue)
                     ;; Import embedded dependencies
                     (dolist (dep (issue-dependencies issue))
                       (%upsert-dependency db dep))
                     (incf count))))
      (sqlite:execute-non-query db "COMMIT"))
    count))

;;; ----------------------------------------------------------------------------
;;; High-level sync
;;; ----------------------------------------------------------------------------

(defun sync-store (store &key (beads-dir ".beads"))
  "Export all issues from STORE to {BEADS-DIR}/issues.jsonl."
  (let* ((dir (uiop:ensure-directory-pathname beads-dir))
         (jsonl-path (merge-pathnames "issues.jsonl" dir)))
    (export-jsonl store jsonl-path)))
