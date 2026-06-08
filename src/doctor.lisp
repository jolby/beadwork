(in-package :beadwork)

(defun extract-bw-id (text)
  "Extract a bw issue ID from TEXT. Returns the ID string if TEXT is exactly
a valid bw ID (bd-<alphanumeric>), or NIL if not.
Examples: \"bd-0lg\" → \"bd-0lg\", \"—\" → NIL, \"note f9efe052\" → NIL."
  (when text
    (let ((trimmed (string-trim " " text)))
      (when (ppcre:scan "^bd-[a-z0-9]+$" trimmed)
        trimmed))))

(defun classify-row (row store)
  "Classify a parsed What's Next ROW against the beadwork STORE.
ROW is a list (priority task id-text).
Returns a plist with keys :status (:ok, :stale, :missing, :orphan),
:id (string or nil), :task (string), and :issue-status (keyword or nil)."
  (destructuring-bind (priority task id-text) row
    (declare (ignore priority))
    (let ((bw-id (extract-bw-id id-text)))
      (if bw-id
          ;; Has a valid bw ID — check existence and status
          (handler-case
              (let ((issue (get-issue store bw-id)))
                (if (member (issue-status issue) '(:closed :tombstone))
                    (list :status :stale
                          :id bw-id
                          :task task
                          :issue-status (issue-status issue))
                    (list :status :ok
                          :id bw-id
                          :task task
                          :issue-status (issue-status issue))))
            (issue-not-found ()
              (list :status :missing
                    :id bw-id
                    :task task
                    :issue-status nil)))
          ;; No valid bw ID — orphan
          (list :status :orphan
                :id nil
                :task task
                :issue-status nil)))))

(defun parse-whats-next-table (lines)
  "Parse the What's Next markdown table from LINES (list of strings).
Returns a list of rows, each a list of (priority task beadwork-id).
Skips the header and separator rows. Stops at blank line or next heading
only after at least one data row has been collected."
  (let ((in-table nil)
        (past-separator nil)
        (started nil)
        (rows nil))
    (dolist (line lines)
      (cond
        ;; Detect What's Next heading
        ((ppcre:scan "^#+\\s*What'?s Next" line)
         (setf in-table t))
        ;; Detect separator row (dashes and pipes only) — mark past it
        ((and in-table
              (ppcre:scan "^\\|" line)
              (ppcre:scan "^\\|[- :|]*\\|$" line))
         (setf past-separator t))
        ;; Exit table on blank line or next heading (only after rows collected)
        ((and in-table started
              (or (string= "" (string-trim " " line))
                  (ppcre:scan "^#" line)))
         (return))
        ;; Parse data row (pipe-delimited, skip header/separator)
        ((and in-table past-separator
              (ppcre:scan "^\\|" line))
         (let ((cells (mapcar (lambda (c) (string-trim " " c))
                              (cdr (ppcre:split "\\|" line)))))
           ;; Remove trailing pipe artifact (empty string after last |)
           (when (string= "" (car (last cells)))
             (setf cells (butlast cells)))
           ;; Require at least 2 cells (priority + task); ID is optional
           (when (>= (length cells) 2)
             (let ((priority (first cells))
                   (task (second cells))
                   (id (if (>= (length cells) 3) (third cells) "")))
               (when (plusp (length (string-trim " " task)))
                 (push (list priority task id) rows)
                 (setf started t))))))))
    (nreverse rows)))

(defun find-latest-status-doc (status-dir)
  "Return the pathname of the latest status doc in STATUS-DIR.
Latest is determined by highest timestamp in filename (YYYYMMDDTHHMM-*.md).
Returns NIL if no matching files found."
  (let ((files (uiop:directory-files status-dir "*.md")))
    (first (sort files #'string>
                 :key (lambda (f) (pathname-name f))))))

(defun find-status-docs-dir ()
  "Resolve the status docs directory relative to current working directory.
Returns the pathname if it exists, or NIL."
  (let ((dir (merge-pathnames "resources/project/status/" (uiop:getcwd))))
    (when (probe-file dir)
      dir)))
