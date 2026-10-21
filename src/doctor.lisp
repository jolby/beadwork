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

(defparameter *whats-next-heading*
  "^(?:#+|\\*+)\\s*(?:What'?s\\s+Next|Next\\s+Steps?|Action\\s+Items?|Proposed\\s+Next\\s+Steps?|Remaining\\s+Work)"
  "Heading text that opens a What's Next section without the explicit marker.")

(defun parse-whats-next-table (lines)
  "Parse the What's Next section from LINES (list of strings).

Returns (values ROWS FOUND-P).
  ROWS    - list of (priority task id-text); may be empty.
  FOUND-P - true when a marker or heading opened a section, even if no row
            could be parsed from it.

Detection priority:
  1. Explicit marker: <!-- bw:whats-next --> (most reliable)
  2. Heading variants: 'What's Next', 'Next Steps', 'Action Items', etc.

The section ends at the next heading of the same style (markdown or org) or at
end of file, so a postscript cannot become false rows (bd-sa3.11.5).  Rows
come from a pipe table, else a numbered/bulleted list, else prose lines that
name a bd id; prose lines without an id are ignored rather than reported as
orphans (bd-fs9)."
  (let ((state :before)        ; :before, :armed (marker seen), :in-section
        (style nil)            ; :md or :org -- heading style that opened it
        (past-separator nil)
        (table-rows nil)
        (list-rows nil)
        (prose-rows nil)
        (found-p nil))
    (labels ((ends-section-p (line)
               (or (ppcre:scan "^#+\\s" line)
                   (and (eq style :org) (ppcre:scan "^\\*+\\s+\\S" line))))
             (active-p ()
               (member state '(:armed :in-section))))
      (dolist (line lines)
        (cond
          ;; Explicit marker
          ((search "<!-- bw:whats-next -->" line)
           (setf state :armed found-p t))
          ;; End of the section: next heading of the same style
          ((and (eq state :in-section) (ends-section-p line))
           (return))
          ;; Markdown heading: titles the section when armed, may open it by name
          ((ppcre:scan "^#+\\s" line)
           (cond
             ((eq state :armed) (setf state :in-section style :md))
             ((and (eq state :before) (ppcre:scan *whats-next-heading* line))
              (setf state :in-section style :md found-p t))))
          ;; Org heading: same, but only when it names the section
          ((ppcre:scan "^\\*+\\s+\\S" line)
           (cond
             ((and (eq state :armed) (ppcre:scan *whats-next-heading* line))
              (setf state :in-section style :org))
             ((and (eq state :before) (ppcre:scan *whats-next-heading* line))
              (setf state :in-section style :org found-p t))))
          ;; Table separator
          ((and (active-p)
                (ppcre:scan "^\\|" line)
                (ppcre:scan "^\\|[- :|]*\\|$" line))
           (setf state :in-section past-separator t))
          ;; Table data row
          ((and (active-p) past-separator (ppcre:scan "^\\|" line))
           (setf state :in-section)
           (let ((cells (mapcar (lambda (c) (string-trim " " c))
                                (cdr (ppcre:split "\\|" line)))))
             (when (string= "" (car (last cells)))
               (setf cells (butlast cells)))
             (when (>= (length cells) 2)
               (let ((priority (first cells))
                     (task (second cells))
                     (id (if (>= (length cells) 3) (third cells) "")))
                 (when (plusp (length (string-trim " " task)))
                   (push (list priority task id) table-rows))))))
          ;; Numbered or bulleted list item (only before a table starts)
          ((and (active-p) (not past-separator)
                (ppcre:scan "^\\s*(?:\\d+\\.\\s+|[-*]\\s+)" line))
           (setf state :in-section)
           (let* ((task (string-trim " "
                           (ppcre:regex-replace
                            "^\\s*(?:\\d+\\.\\s+|[-*]\\s+)" line "")))
                  (bw-id (or (extract-bw-id task) "")))
             (when (plusp (length task))
               (push (list "—" task bw-id) list-rows))))
          ;; Prose: keep only lines that name an issue
          ((and (active-p) (not (string= "" (string-trim " " line))))
           (setf state :in-section)
           (let ((bw-id (extract-bw-id line)))
             (when bw-id
               (push (list "—" (string-trim " " line) bw-id) prose-rows)))))))
    (values (cond (table-rows (nreverse table-rows))
                  (list-rows (nreverse list-rows))
                  (t (nreverse prose-rows)))
            found-p)))

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

Returns a plist with :status-doc (pathname), :checked (true when at least one
row was classified), :findings (list of classification plists), :summary (plist
with :ok, :stale, :missing, :orphan counts), and :info (why nothing was checked,
when :checked is NIL).  A doc with no parseable What's Next is not an error, but
it must be recognizable as *not checked* (bd-fs9)."
  (let ((doc (find-latest-status-doc status-dir)))
    (unless doc
      (return-from run-doctor-status-docs
        (list :status-doc nil :findings nil :summary nil
              :error "No status docs found")))
    (let ((lines (uiop:read-file-lines doc)))
      (multiple-value-bind (rows found-p)
          (parse-whats-next-table lines)
        (if rows
            (let* ((findings (mapcar (lambda (row) (classify-row row store :doc-path doc)) rows))
                   (ok (count :ok findings :key (lambda (f) (getf f :status))))
                   (stale (count :stale findings :key (lambda (f) (getf f :status))))
                   (missing (count :missing findings :key (lambda (f) (getf f :status))))
                   (orphan (count :orphan findings :key (lambda (f) (getf f :status)))))
              (list :status-doc doc
                    :checked t
                    :findings findings
                    :summary (list :ok ok :stale stale :missing missing :orphan orphan)))
            (list :status-doc doc
                  :checked nil
                  :findings nil
                  :summary (list :ok 0 :stale 0 :missing 0 :orphan 0)
                  :info (if found-p
                            (format nil "What's Next section found in ~A but no issue references were parsed -- expected a table, a list, or lines naming bd-XXXX."
                                    (file-namestring doc))
                            (format nil "No What's Next section found in ~A. Add <!-- bw:whats-next --> before the section, or name issues inline so they can be checked."
                                    (file-namestring doc)))))))))

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
          (format t "  NOT CHECKED -- nothing was cross-referenced.~%")
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
            (checked (getf result :checked))
            (healthy (and checked
                          (zerop (getf summary :stale 0))
                          (zerop (getf summary :missing 0))
                          (zerop (getf summary :orphan 0))))
            (counts (make-hash-table :test #'equal))
            (ht (make-hash-table :test #'equal)))
       ;; jzon writes hash-tables as objects and lists as arrays; the old code
       ;; produced "[[\"status-doc\", ...]]" instead of an object (bd-7b6).
       (setf (gethash "ok" counts) (getf summary :ok 0))
       (setf (gethash "stale" counts) (getf summary :stale 0))
       (setf (gethash "missing" counts) (getf summary :missing 0))
       (setf (gethash "orphan" counts) (getf summary :orphan 0))
       (setf (gethash "status-doc" ht) (if doc (namestring doc) 'null))
       (setf (gethash "checked" ht) (if checked t nil))
       (setf (gethash "findings" ht)
             (coerce (mapcar (lambda (f)
                               (let ((fht (make-hash-table :test #'equal)))
                                 (setf (gethash "id" fht) (or (getf f :id) 'null))
                                 (setf (gethash "task" fht) (or (getf f :task) 'null))
                                 (setf (gethash "status" fht) (symbol-name (getf f :status)))
                                 (setf (gethash "source" fht) (or (getf f :source) 'null))
                                 (setf (gethash "issue-status" fht)
                                       (if (getf f :issue-status)
                                           (string-downcase
                                            (symbol-name (getf f :issue-status)))
                                           'null))
                                 fht))
                             findings)
                     'vector))
       (setf (gethash "summary" ht) counts)
       (setf (gethash "healthy" ht) (if checked (and healthy t) 'null))
       (setf (gethash "info" ht) (or (getf result :info) 'null))
       (format t "~A" (jzon:stringify ht :pretty t))
       (if healthy 0 1)))))
