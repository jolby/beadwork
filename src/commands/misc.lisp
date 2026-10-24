(in-package :beadwork)

;;; ============================================================================
;;; Commands: misc (search, stats, init, sync, doctor, batch)
;;; ============================================================================

;;; ---------------------------------------------------------------------------
;;; Command: search
;;; ---------------------------------------------------------------------------

(defun search/handler (cmd)
  (let* ((format-val (clingon:getopt cmd :format))
         (*format* (parse-format format-val))
       (*no-color* (clingon:getopt* cmd :no-color))
         (store (ensure-store cmd))
         (query (first (clingon:command-arguments cmd))))
    (unless query
      (format *error-output* "Usage: bw search <query>~%")
      (clingon:exit 1))
    (let ((issues (search-issues store query)))
      (print-issues issues store))))

(defun search/command ()
  (clingon:make-command
   :name "search"
   :description "Search issues by title and description (LIKE match)"
   :aliases '("q")
   :options (global-options)
   :handler #'search/handler
   :usage "<query>"))

;;; ---------------------------------------------------------------------------
;;; Command: stats
;;; ---------------------------------------------------------------------------

(defun counts->json (counts)
  "Convert a keyword plist of counts to a JSON object hash-table."
  (let ((ht (make-hash-table :test #'equal)))
    (loop for (key value) on counts by #'cddr
          do (setf (gethash (string-downcase (symbol-name key)) ht) value))
    ht))

(defun stats->json (stats)
  "Convert ISSUE-STATS output to a jzon JSON object (bd-7b6).

A plist stringifies to a JSON *array* in jzon, so consumers doing `jq .total`
failed; build hash-tables instead."
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash "total" ht) (getf stats :total))
    (setf (gethash "ready-count" ht) (getf stats :ready-count))
    (setf (gethash "counts-by-status" ht)
          (counts->json (getf stats :counts-by-status)))
    (setf (gethash "counts-by-priority" ht)
          (counts->json (getf stats :counts-by-priority)))
    (setf (gethash "counts-by-type" ht)
          (counts->json (getf stats :counts-by-type)))
    ht))

(defun stats/handler (cmd)
  (let* ((format-val (clingon:getopt cmd :format))
         (*format* (parse-format format-val))
       (*no-color* (clingon:getopt* cmd :no-color))
         (store (ensure-store cmd))
         (stats (issue-stats store))
         (counts-status (getf stats :counts-by-status))
         (counts-pri (getf stats :counts-by-priority))
         (counts-type (getf stats :counts-by-type)))
    (if (eq *format* :json)
        (format t "~A" (jzon:stringify (stats->json stats) :pretty t))
        (progn
          (format t "Issues: ~D total, ~D ready~%"
                  (getf stats :total) (getf stats :ready-count))
          (format t "Status:  ~D open, ~D in-progress, ~D blocked, ~D deferred, ~D closed~%"
                  (getf counts-status :open)
                  (getf counts-status :in-progress)
                  (getf counts-status :blocked)
                  (getf counts-status :deferred)
                  (getf counts-status :closed))
          (format t "Priority: P0:~D P1:~D P2:~D P3:~D P4:~D~%"
                  (getf counts-pri :p0)
                  (getf counts-pri :p1)
                  (getf counts-pri :p2)
                  (getf counts-pri :p3)
                  (getf counts-pri :p4))
          (format t "Type:    bug:~D feature:~D task:~D epic:~D chore:~D docs:~D question:~D~%"
                  (getf counts-type :bug)
                  (getf counts-type :feature)
                  (getf counts-type :task)
                  (getf counts-type :epic)
                  (getf counts-type :chore)
                  (getf counts-type :docs)
                  (getf counts-type :question))))))

(defun stats/command ()
  (clingon:make-command
   :name "stats"
   :description "Show aggregate issue statistics"
   :options (global-options)
   :handler #'stats/handler))

;;; ---------------------------------------------------------------------------
;;; Command: init
;;; ---------------------------------------------------------------------------

(defun init/handler (cmd)
  (let* ((path (or (clingon:getopt cmd :path)
                   (uiop:getcwd)))
         ;; --path may arrive without a trailing slash.  Normalize it first so
         ;; merge-pathnames treats it as a directory instead of a file name
         ;; (bd-5t0; resolve-db-path fixed the same class of bug for --db).
         (dir (uiop:ensure-directory-pathname path)))
    (let ((beads-dir (merge-pathnames ".beads/" dir)))
      (when (probe-file beads-dir)
        (format *error-output* "Error: .beads/ already exists at ~A~%" beads-dir)
        (clingon:exit 1))
      (ensure-directories-exist beads-dir)
      (let ((db-path (merge-pathnames "beads.db" beads-dir))
            (config-path (merge-pathnames "config.yaml" beads-dir))
            (gitignore-path (merge-pathnames ".gitignore" beads-dir)))
        (open-store (namestring db-path))
        (with-open-file (out config-path :direction :output)
          (format out "# Beads Project Configuration~%"))
        (with-open-file (out gitignore-path :direction :output)
          (format out "*.db~%*.db-shm~%*.db-wal~%*.lock~%"))
        (format t "Initialized .beads/ at ~A~%" beads-dir)))))

(defun init/options ()
  (list
   (clingon:make-option
    :filepath
    :short-name #\p
    :long-name "path"
    :description "Path to initialize"
    :key :path)))

(defun init/command ()
  (clingon:make-command
   :name "init"
   :description "Initialize a new .beads/ directory"
   :options (append (init/options) (global-options))
   :handler #'init/handler))

;;; ---------------------------------------------------------------------------
;;; Command: sync
;;; ---------------------------------------------------------------------------

(defun sync/handler (cmd)
  (let* ((store (ensure-store cmd))
         ;; --db may arrive without a trailing slash.  Normalize it before
         ;; merging the JSONL filename, or merge-pathnames treats ".beads" as
         ;; a file name and writes into its parent (bd-a5u, same class as
         ;; bd-5t0).
         (beads-dir (uiop:ensure-directory-pathname
                     (or (clingon:getopt* cmd :db-path) (find-beads-dir))))
         (direction (clingon:getopt cmd :direction)))
    (cond
      ((string-equal direction "export")
       (let ((path (merge-pathnames "issues.jsonl" beads-dir)))
         (multiple-value-bind (exported problems)
             (export-jsonl store (namestring path))
           (declare (ignore exported))
           (dolist (problem problems)
             (destructuring-bind (id column raw) problem
               (format *error-output*
                       "warning: ~A ~A is not a parseable timestamp (~S); exported without a value~%"
                       id column raw)))
           (format t "Exported to ~A~%" path)
           (when problems
             (format *error-output*
                     "~D timestamp(s) have no exported value -- fix the rows, then re-export~%"
                     (length problems))
             (clingon:exit 1)))))
      ((string-equal direction "import")
       (let ((path (merge-pathnames "issues.jsonl" beads-dir)))
         (import-jsonl store (namestring path))
         (format t "Imported from ~A~%" path)))
      (t
       (error 'beadwork-error :message (format nil "Invalid sync direction: ~A" direction))))))

(defun sync/options ()
  (list
   (clingon:make-option
    :choice
    :long-name "direction"
    :description "Sync direction: export or import"
    :key :direction
    :items '("export" "import")
    :initial-value "export")))

(defun sync/command ()
  (clingon:make-command
   :name "sync"
   :description "Sync with JSONL file for git"
   :options (append (sync/options) (global-options))
   :handler #'sync/handler))

;;; ---------------------------------------------------------------------------
;;; Command: doctor
;;; ---------------------------------------------------------------------------

(defun doctor/options ()
  (list
   (clingon:make-option
    :boolean
    :long-name "all"
    :description "Scan all status docs, not just latest"
    :key :all)
   (clingon:make-option
    :string
    :long-name "status-dir"
    :description "Path to status docs directory"
    :key :status-dir)))

(defun doctor-status-docs/handler (cmd)
  (let* ((format-val (clingon:getopt cmd :format))
         (*format* (parse-format format-val))
       (*no-color* (clingon:getopt* cmd :no-color))
         (store (ensure-store cmd))
         (status-dir (or (clingon:getopt cmd :status-dir)
                         (find-status-docs-dir))))
    (unless status-dir
      (format *error-output* "Error: No status docs directory found.~%")
      (format *error-output* "Run from project root or use --status-dir <path>~%")
      (clingon:exit 2))
    (let ((result (run-doctor-status-docs store status-dir)))
      (when (getf result :error)
        (unless (eq *format* :json)
          (format t "bw doctor: ~A~%" (getf result :error)))
        (clingon:exit 2))
      (let ((exit-code (report-doctor-findings
                        result
                        (if (eq *format* :json) :json :human))))
        (clingon:exit exit-code)))))

(defun doctor-status-docs/command ()
  (clingon:make-command
   :name "status-docs"
   :description "Cross-reference status doc What's Next against beadwork issues"
   :options (global-options)
   :handler #'doctor-status-docs/handler))

(defun doctor-health/handler (cmd)
  (let* ((format-val (clingon:getopt cmd :format))
         (*format* (parse-format format-val))
       (*no-color* (clingon:getopt* cmd :no-color))
         (store (ensure-store cmd))
         (status-dir (or (clingon:getopt cmd :status-dir)
                         (find-status-docs-dir))))
    (unless status-dir
      (format *error-output* "Error: No status docs directory found.~%")
      (clingon:exit 2))
    (let ((result (run-doctor-status-docs store status-dir)))
      (when (getf result :error)
        (unless (eq *format* :json)
          (format t "bw doctor: ~A~%" (getf result :error)))
        (clingon:exit 2))
      (let* ((checked (getf result :checked))
             (summary (getf result :summary))
             (ok (getf summary :ok 0))
             (stale (getf summary :stale 0))
             (missing (getf summary :missing 0))
             (orphan (getf summary :orphan 0))
             (unscoped (getf (run-doctor-unscoped store) :count))
             (healthy (and checked
                           (zerop stale) (zerop missing) (zerop orphan))))
        (if (eq *format* :json)
            (let ((ht (make-hash-table :test #'equal)))
              (setf (gethash "checked" ht) (if checked t nil))
              (setf (gethash "ok" ht) ok)
              (setf (gethash "stale" ht) stale)
              (setf (gethash "missing" ht) missing)
              (setf (gethash "orphan" ht) orphan)
              (setf (gethash "unscoped" ht) unscoped)
              (setf (gethash "healthy" ht) (if checked (and healthy t) 'null))
              (format t "~A" (jzon:stringify ht)))
            (if checked
                (format t "~D OK, ~D STALE, ~D MISSING, ~D ORPHAN, ~D unscoped -- exit ~D~%"
                        ok stale missing orphan unscoped (if healthy 0 1))
                (format t "NOT CHECKED (~D unscoped) -- ~A~%" unscoped (getf result :info))))
        ;; Not checked exits 0 by design (a doc with no next steps is legal);
        ;; the output line is what makes the state visible (bd-fs9).
        (clingon:exit (if checked (if healthy 0 1) 0))))))

(defun doctor-health/command ()
  (clingon:make-command
   :name "health"
   :description "One-line doctor summary for cold-start integration"
   :options (append (doctor/options) (global-options))
   :handler #'doctor-health/handler))

(defun doctor-unscoped/handler (cmd)
  (let* ((format-val (clingon:getopt cmd :format))
         (*format* (parse-format format-val))
         (store (ensure-store cmd)))
    (clingon:exit (report-doctor-unscoped
                   (run-doctor-unscoped store)
                   (if (eq *format* :json) :json :human)))))

(defun doctor-unscoped/command ()
  (clingon:make-command
   :name "unscoped"
   :description "List open issues with no source repo (the \".\" bucket)"
   :options (append (doctor/options) (global-options))
   :handler #'doctor-unscoped/handler))

(defun doctor/handler (cmd)
  "Flat 'bw doctor' -- runs status-docs check by default."
  (doctor-status-docs/handler cmd))

(defun doctor/command ()
  (clingon:make-command
   :name "doctor"
   :description "Run project-health diagnostics (status doc <-> issue sync)"
   :options (append (doctor/options) (global-options))
   :handler #'doctor/handler
   :sub-commands (list (doctor-status-docs/command)
                       (doctor-health/command)
                       (doctor-unscoped/command))))

;;; ---------------------------------------------------------------------------
;;; Command: batch
;;; ---------------------------------------------------------------------------

(defun batch/handler (cmd)
  "Handle bw batch -- read JSON from stdin or --file, process, print result."
  ;; --example: print sample JSON payload and exit
  (when (clingon:getopt cmd :example)
    (princ *batch-example-json*)
    (terpri)
    (return-from batch/handler))
  (let* ((format-val (clingon:getopt cmd :format))
         (*format* (parse-format format-val))
       (*no-color* (clingon:getopt* cmd :no-color))
         (store (ensure-store cmd))
         (file-path (clingon:getopt cmd :file))
         (idempotency-key (clingon:getopt cmd :idempotency-key))
         (repo-val (clingon:getopt cmd :source-repo))
         (source-repo (or (when repo-val (first repo-val))
                          (detect-source-repo)))
         (dry-run (clingon:getopt cmd :dry-run))
         (json-string
           (if file-path
               (uiop:read-file-string file-path)
               (let ((lines '()))
                 (loop for line = (read-line *standard-input* nil nil)
                       while line
                       do (push line lines))
                 (format nil "~{~A~%~}" (nreverse lines))))))
    (let ((result (process-batch store json-string
                                 :idempotency-key idempotency-key
                                 :source-repo source-repo
                                 :dry-run dry-run)))
      (format t "~A~%" result)
      (let* ((parsed (com.inuoe.jzon:parse result))
             (warnings (gethash "warnings" parsed)))
        (when warnings
          (dolist (w (coerce warnings 'list))
            (format *error-output* "warning: ~A~%" w)))
        ;; Exit non-zero if batch failed
        (unless (gethash "ok" parsed)
          (clingon:exit 1))))))

(defparameter *batch-example-json*
  "{
  \"idempotency_key\": \"h_abc123\",
  \"operations\": [
    {
      \"op\": \"create\",
      \"ref\": \"epic\",
      \"title\": \"widget system overhaul\",
      \"type\": \"epic\",
      \"priority\": \"P1\",
      \"description\": \"Complete rewrite of the widget subsystem.\\n\\nGoals:\\n- Faster rendering\\n- Better API\",
      \"children\": [
        { \"op\": \"create\", \"ref\": \"core\", \"title\": \"widget core engine\", \"type\": \"feature\", \"priority\": \"P2\" },
        { \"op\": \"create\", \"ref\": \"ui\",   \"title\": \"widget UI layer\",    \"type\": \"feature\", \"priority\": \"P2\" }
      ]
    },
    { \"op\": \"link\", \"source\": { \"ref\": \"core\" }, \"target\": { \"ref\": \"ui\" }, \"relation\": \"blocks\" },
    { \"op\": \"comment\", \"id\": { \"ref\": \"epic\" }, \"text\": \"Batched creation -- edit this template\" },
    { \"op\": \"update\", \"id\": \"bd-xxx\", \"status\": \"in_progress\" }
  ]
}"
  "Example JSON payload for bw batch --example.")

(defun batch/options ()
  (list
   (clingon:make-option
    :string
    :long-name "file"
    :short-name #\f
    :description "Read JSON batch payload from file"
    :key :file)
   (clingon:make-option
    :string
    :long-name "idempotency-key"
    :short-name #\k
    :description "Idempotency key for safe retry"
    :key :idempotency-key)
   (clingon:make-option
    :boolean/true
    :long-name "dry-run"
    :description "Validate and simulate the JSON batch, then roll back (nothing is persisted)"
    :key :dry-run)
   (clingon:make-option
    :boolean/true
    :long-name "example"
    :description "Print an example batch payload and exit"
    :key :example)))

(defun batch/command ()
  (clingon:make-command
   :name "batch"
   :description "Process a batch of operations from JSON payload.

Payload is a JSON object with an 'operations' array. Each operation has an 'op' field:
  create: {\"op\":\"create\", \"title\":\"...\", \"type\":\"task|bug|feature|epic|chore|docs\",
           \"ref\":\"label\", \"priority\":\"P1-P4\", \"description\":\"...\", \"repo\":\"...\", \"children\":[...]}
  update: {\"op\":\"update\", \"id\":\"bd-xxx\", \"title\":\"...\", \"status\":\"...\", ...}
  link:   {\"op\":\"link\", \"source\":{\"ref\":\"x\"},\"target\":{\"ref\":\"y\"|\"id\":\"bd-xxx\"}, \"relation\":\"blocks|...\"}
          DIRECTION: source depends on target. A blocks link means source is
          BLOCKED BY target; parent-child makes source a child of target.
          e.g. {source:{ref:child}, target:{ref:parent}, relation:\"parent-child\"}
  comment:{\"op\":\"comment\", \"id\":{\"ref\":\"x\"|\"id\":\"bd-xxx\"}, \"text\":\"...\"}
Anywhere an issue is named (update/comment id, link source/target) both shapes
work: a bare id string (\"bd-xxx\") or an object ({\"ref\":\"label\"} / {\"id\":\"bd-xxx\"}).
A failing op is reported as 'operation N (op-type): ...' so a mixed batch is
debuggable without bisecting the operations array.
Use --example to print a complete example payload."
   :options (append (batch/options) (global-options))
   :handler #'batch/handler))
