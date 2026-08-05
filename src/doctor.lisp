(in-package :beadwork)

(defun extract-bw-id (text)
  "Extract a bw issue ID from TEXT.
If TEXT is exactly a valid bw ID (bd-<alphanumeric>), returns it.
If TEXT contains an embedded bw ID (e.g., '(bd-szn.5)'), extracts it.
Otherwise returns NIL.
Examples: \"bd-0lg\" → \"bd-0lg\", \"(bd-szn.5)\" → \"bd-szn.5\", \"—\" → NIL."
  (when text
    (let ((trimmed (string-trim " " text)))
      (or (ppcre:register-groups-bind (id)
              ("^(bd-[a-z0-9]+)$" trimmed)
            id)
          (ppcre:register-groups-bind (id)
              (".*?(bd-[a-z0-9]+)" trimmed)
            id)))))

(defun orphan-id (doc-namestring task)
  "Generate a stable pseudo-ID for an orphan task.
Uses a hash of the document path + task text, truncated to 7 hex chars."
  (let ((hash (format nil "~36R" (sxhash (concatenate 'string doc-namestring task)))))
    (format nil "orph-~A" (subseq hash (max 0 (- (length hash) 7))))))

(defun classify-row (row store &key doc-path)
  "Classify a parsed What's Next ROW against the beadwork STORE.
ROW is a list (priority task id-text).
DOC-PATH is the status document pathname (for generating orphan IDs).
Returns a plist with keys :status (:ok, :stale, :missing, :orphan),
:id (string or nil), :task (string), :source (string), and
:issue-status (keyword or nil)."
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
          ;; No valid bw ID — orphan: generate a stable pseudo-ID
          (let* ((doc-str (if doc-path (file-namestring doc-path) "unknown"))
                 (oid (orphan-id doc-str task)))
            (list :status :orphan
                  :id oid
                  :task task
                  :source doc-str
                  :issue-status nil))))))

(defun parse-whats-next-table (lines)
  "Parse the What's Next section from LINES (list of strings).

Detection priority:
  1. Explicit marker: <!-- bw:whats-next --> (most reliable)
  2. Heading variants: 'What's Next', 'Next Steps', 'Action Items', etc.

Supports two table formats:
  1. Pipe table: heading + | Priority | Task | ID | separator + data rows
  2. Numbered list: heading + 1. Task text (possibly with embedded bw ID)
Also accepts both markdown (#) and org-mode (*) headings."
  (let ((in-section nil)
        (past-separator nil)
        (started nil)
        (rows nil))
    (dolist (line lines)
      (cond
        ;; Explicit marker: <!-- bw:whats-next -->
        ((search "<!-- bw:whats-next -->" line)
         (setf in-section t))
        ;; Heading variants: What's Next, Next Steps, Action Items, Proposed Next Steps
        ((ppcre:scan "^(?:#+|\\*+)\\s*(?:What'?s\\s+Next|Next\\s+Steps?|Action\\s+Items?|Proposed\\s+Next\\s+Steps?|Remaining\\s+Work)" line)
         (setf in-section t))
        ;; Table: detect separator row (dashes and pipes only)
        ((and in-section
              (ppcre:scan "^\\|" line)
              (ppcre:scan "^\\|[- :|]*\\|$" line))
         (setf past-separator t))
        ;; Table: parse pipe-delimited data row
        ((and in-section past-separator
              (ppcre:scan "^\\|" line))
         (let ((cells (mapcar (lambda (c) (string-trim " " c))
                              (cdr (ppcre:split "\\|" line)))))
           (when (string= "" (car (last cells)))
             (setf cells (butlast cells)))
           (when (>= (length cells) 2)
             (let ((priority (first cells))
                   (task (second cells))
                   (id (if (>= (length cells) 3) (third cells) "")))
               (when (plusp (length (string-trim " " task)))
                 (push (list priority task id) rows)
                 (setf started t))))))
        ;; List: numbered item (1. 2. etc.) or bullet (- *)
        ((and in-section (not past-separator)
              (ppcre:scan "^\\s*(?:\\d+\\.\\s+|[-*]\\s+)" line))
         (let* ((task (string-trim " "
                         (ppcre:regex-replace
                          "^\\s*(?:\\d+\\.\\s+|[-*]\\s+)" line "")))
                (bw-id (or (extract-bw-id task) "")))
           (when (plusp (length task))
             (push (list "—" task bw-id) rows)
             (setf started t))))
        ;; Exit on blank line or next heading (only after rows collected)
        ((and in-section started
              (or (string= "" (string-trim " " line))
                  (ppcre:scan "^(?:#+|\\*+)\\s" line)))
         (return))))
    (nreverse rows)))

(defun find-latest-status-doc (status-dir)
  "Return the pathname of the latest status doc in STATUS-DIR.
Latest is determined by highest timestamp in filename (YYYYMMDDTHHMM-*.md).
Only considers files with timestamp-prefixed names (starting with a digit).
Returns NIL if no matching files found."
  (let ((files (remove-if-not (lambda (f)
                                (ppcre:scan "^\\d" (pathname-name f)))
                              (uiop:directory-files status-dir "*.md"))))
    (first (sort files #'string>
                 :key (lambda (f) (pathname-name f))))))

(defun find-status-docs-dir ()
  "Resolve the status docs directory relative to current working directory.
Returns the pathname if it exists, or NIL."
  (let ((dir (merge-pathnames "resources/project/status/" (uiop:getcwd))))
    (when (probe-file dir)
      dir)))

(defun run-doctor-status-docs (store status-dir)
  "Run the status-docs check. Reads the latest status doc, parses the
What's Next section, and classifies each row against STORE.
Returns a plist with :status-doc (pathname), :findings (list of classification
plists), and :summary (plist with :ok, :stale, :missing, :orphan counts).
If no What's Next section is found, returns a soft-info result (not an error)."
  (let ((doc (find-latest-status-doc status-dir)))
    (unless doc
      (return-from run-doctor-status-docs
        (list :status-doc nil :findings nil :summary nil :error "No status docs found")))
    (let* ((lines (uiop:read-file-lines doc))
           (rows (parse-whats-next-table lines)))
      (if rows
          (let* ((findings (mapcar (lambda (row) (classify-row row store :doc-path doc)) rows))
                 (ok (count :ok findings :key (lambda (f) (getf f :status))))
                 (stale (count :stale findings :key (lambda (f) (getf f :status))))
                 (missing (count :missing findings :key (lambda (f) (getf f :status))))
                 (orphan (count :orphan findings :key (lambda (f) (getf f :status)))))
            (list :status-doc doc
                  :findings findings
                  :summary (list :ok ok :stale stale :missing missing :orphan orphan)))
          ;; No What's Next section — not an error, just a soft-info
          (list :status-doc doc
                :findings nil
                :summary (list :ok 0 :stale 0 :missing 0 :orphan 0)
                :info (format nil "No What's Next section found in ~A. Add <!-- bw:whats-next --> before the section to enable cross-referencing."
                              (file-namestring doc)))))))

(defun format-finding-line (finding)
  "Format a single finding as a human-readable line.
For orphans, includes the source document name."
  (let* ((status (getf finding :status))
         (id (getf finding :id))
         (task (getf finding :task))
         (source (getf finding :source))
         (marker (ecase status
                   (:ok "OK    ")
                   (:stale "STALE ")
                   (:missing "MISS  ")
                   (:orphan "ORPHAN"))))
    (if (and (eq status :orphan) source)
        (format nil "~A ~A  ~A  [~A]"
                marker id (string-trim " " task) source)
        (format nil "~A ~A  ~A"
                marker
                (if id (format nil "~A  " id) "—        ")
                (string-trim " " task)))))

(defun report-doctor-findings (result format)
  "Print the doctor findings in FORMAT (:human or :json).
Returns the appropriate exit code: 0 if healthy, 1 if problems found."
  (ecase format
    (:human
     (let* ((doc (getf result :status-doc))
            (findings (getf result :findings))
            (summary (getf result :summary))
            (error (getf result :error))
            (info (getf result :info))
            (ok (getf summary :ok 0))
            (stale (getf summary :stale 0))
            (missing (getf summary :missing 0))
            (orphan (getf summary :orphan 0)))
       (cond
         (error
          (format t "bw doctor: ~A~%" error)
          2)
         (info
          (format t "bw doctor: ~A~%" info)
          0)
         (t
          (format t "📋 bw doctor: ~A~%~%" (file-namestring doc))
          (dolist (f findings)
            (format t "  ~A~%" (format-finding-line f)))
          (format t "~%  ~D OK, ~D STALE, ~D MISSING, ~D ORPHAN~%"
                  ok stale missing orphan)
          (if (and (zerop stale) (zerop missing) (zerop orphan))
              0
              1)))))
    (:json
     (let* ((doc (getf result :status-doc))
            (findings (getf result :findings))
            (summary (getf result :summary))
            (healthy (and (zerop (getf summary :stale 0))
                          (zerop (getf summary :missing 0))
                          (zerop (getf summary :orphan 0)))))
       (format t "~A"
               (jzon:stringify
                (list (list "status-doc" (when doc (namestring doc)))
                      (list "findings"
                            (mapcar (lambda (f)
                                      (list (list "id" (getf f :id))
                                            (list "task" (getf f :task))
                                            (list "status" (symbol-name (getf f :status)))
                                            (list "source" (getf f :source))
                                            (list "issue-status"
                                                  (when (getf f :issue-status)
                                                    (string-downcase
                                                     (symbol-name (getf f :issue-status)))))))
                                    findings))
                      (list "summary" (list (list "ok" (getf summary :ok 0))
                                            (list "stale" (getf summary :stale 0))
                                            (list "missing" (getf summary :missing 0))
                                            (list "orphan" (getf summary :orphan 0))))
                      (list "healthy" healthy)
                      (list "info" (getf result :info)))
                :pretty t))
       (if healthy 0 1)))))
