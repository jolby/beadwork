(in-package :beadwork)

;;; ============================================================================
;;; Beadwork CLI (bw) - Command-line interface using Clingon
;;; ============================================================================

(import '(clingon:make-option
          clingon:make-command
          clingon:getopt
          clingon:getopt*
          clingon:run
          clingon:print-usage-and-exit
          clingon:exit))

;;; ---------------------------------------------------------------------------
;;; Global state
;;; ---------------------------------------------------------------------------

(defvar *store* nil
  "Current database store, bound during command execution.")

(defvar *format* :rich
  "Current output format: :rich, :plain, :json.")

(defvar *verbose* 0
  "Verbosity level counter.")

(defun find-beads-dir ()
  "Find .beads/ directory starting from current dir upward."
  (let ((dir (uiop:getcwd)))
    (loop
      (let ((beads (merge-pathnames ".beads/" dir)))
        (when (probe-file beads)
          (return beads))
        (let ((parent (uiop:pathname-directory-pathname dir)))
          (when (equal parent dir)
            (return nil))
          (setf dir parent))))))

(defun find-db-path ()
  "Find the SQLite database path."
  (let ((beads-dir (find-beads-dir)))
    (unless beads-dir
      (error 'beadwork-error :message "No .beads/ directory found. Run 'bw init' first."))
    (merge-pathnames "beads.db" beads-dir)))

(defun resolve-store ()
  "Get or create the current store."
  (unless *store*
    (setf *store* (open-store (namestring (find-db-path)))))
  *store*)

(defun ensure-store ()
  "Ensure store is open, or exit with error."
  (handler-case
      (resolve-store)
    (error (c)
      (format *error-output* "Error: ~A~%" c)
      (clingon:exit 1))))

;;; ---------------------------------------------------------------------------
;;; Output Formatting
;;; ---------------------------------------------------------------------------

(defun format-issue-plain (issue)
  "Format ISSUE as plain text line."
  (format nil "[~A] [~A] [~A] ~A  ~A"
          (issue-id issue)
          (format-priority (issue-priority issue))
          (issue-type-string (issue-type issue))
          (or (issue-source-repo issue) ".")
          (issue-title issue)))

(defun format-issue-rich (issue)
  "Format ISSUE for terminal display."
  (let* ((status (issue-status issue))
         (status-str (string-upcase (symbol-name status))))
    (format nil "[~A] ~A ~A  ~A"
            status-str
            (format-priority (issue-priority issue))
            (or (issue-source-repo issue) ".")
            (issue-title issue))))

(defun print-issues (issues)
  "Print a list of issues according to *format*."
  (ecase *format*
    (:json
     (format t "~A" (jzon:stringify issues :pretty t)))
    (:plain
     (dolist (issue issues)
       (format t "~A~%" (format-issue-plain issue))))
    (:rich
     (dolist (issue issues)
       (format t "~A~%" (format-issue-rich issue))))))

(defun print-issue-single (issue)
  "Print a single issue in detail."
  (ecase *format*
    (:json
     (format t "~A" (jzon:stringify issue :pretty t)))
    (:plain
     (format t "ID: ~A~%" (issue-id issue))
     (format t "Title: ~A~%" (issue-title issue))
     (format t "Status: ~A~%" (issue-status issue))
     (format t "Priority: ~A~%" (format-priority (issue-priority issue)))
     (format t "Type: ~A~%" (issue-type-string (issue-type issue)))
     (when (issue-assignee issue)
       (format t "Assignee: ~A~%" (issue-assignee issue)))
     (format t "Created: ~A~%" (format-timestamp (issue-created-at issue)))
     (format t "Updated: ~A~%" (format-timestamp (issue-updated-at issue)))
     (when (issue-description issue)
       (format t "~%~A~%" (issue-description issue))))
    (:rich
     (format t "ID: ~A~%" (issue-id issue))
     (format t "Title: ~A~%" (issue-title issue))
     (format t "Status: ~A~%" (issue-status issue))
     (format t "Priority: ~A~%" (format-priority (issue-priority issue)))
     (format t "Type: ~A~%" (issue-type-string (issue-type issue)))
     (when (issue-assignee issue)
       (format t "Assignee: ~A~%" (issue-assignee issue)))
     (format t "Created: ~A~%" (format-timestamp (issue-created-at issue)))
     (format t "Updated: ~A~%" (format-timestamp (issue-updated-at issue)))
     (when (issue-description issue)
       (format t "~%~A~%" (issue-description issue))))))

;;; ---------------------------------------------------------------------------
;;; Global Options
;;; ---------------------------------------------------------------------------

(defun global-options ()
  "Global options available to all commands."
  (list
   (clingon:make-option
    :choice
    :description "Output format"
    :long-name "format"
    :key :format
    :items '("rich" "plain" "json")
    :initial-value "rich")
   (clingon:make-option
    :counter
    :short-name #\v
    :description "Increase verbosity"
    :key :verbose)
   (clingon:make-option
    :string
    :long-name "db"
    :description "Path to .beads/ directory"
    :key :db-path)
   (clingon:make-option
    :string
    :long-name "repo"
    :description "Filter by source repository (e.g. beadwork, cogen-ai)"
    :key :source-repo)))

(defun parse-format (value)
  "Convert format string to keyword."
  (cond ((string= value "json") :json)
        ((string= value "plain") :plain)
        (t :rich)))

;;; ---------------------------------------------------------------------------
;;; Command: list
;;; ---------------------------------------------------------------------------

(defun list/options ()
  (list
   (clingon:make-option
    :choice
    :description "Filter by status"
    :long-name "status"
    :key :status
    :items '("open" "in_progress" "blocked" "deferred" "closed"))
   (clingon:make-option
    :choice
    :description "Filter by priority (P0-P4)"
    :long-name "priority"
    :key :priority
    :items '("P0" "P1" "P2" "P3" "P4"))
   (clingon:make-option
    :choice
    :description "Filter by type"
    :long-name "type"
    :key :type
    :items '("task" "bug" "feature" "epic" "chore" "docs"))
   (clingon:make-option
    :string
    :short-name #\a
    :long-name "assignee"
    :description "Filter by assignee"
    :key :assignee)
   (clingon:make-option
    :integer
    :short-name #\n
    :long-name "limit"
    :description "Limit number of results"
    :key :limit)))

(defun list/handler (cmd)
  (let* ((format-val (clingon:getopt cmd :format))
         (*format* (parse-format format-val))
         (store (ensure-store))
         (status (when-let (s (clingon:getopt cmd :status))
                   (parse-status s)))
         (priority (when-let (p (clingon:getopt cmd :priority))
                    (parse-priority p)))
         (itype (when-let (type-val (clingon:getopt cmd :type))
                  (parse-issue-type type-val)))
         (assignee (clingon:getopt cmd :assignee))
         (limit (clingon:getopt cmd :limit))
         (source-repo (clingon:getopt cmd :source-repo)))
    (let ((issues (list-issues store
                               :status status
                               :priority priority
                               :type itype
                               :assignee assignee
                               :limit limit
                               :source-repo source-repo)))
      (print-issues issues))))

(defun list/command ()
  (clingon:make-command
   :name "list"
   :description "List issues with optional filters"
   :aliases '("ls" "l")
   :options (append (list/options) (global-options))
   :handler #'list/handler))

;;; ---------------------------------------------------------------------------
;;; Command: ready
;;; ---------------------------------------------------------------------------

(defun ready/handler (cmd)
  (let* ((format-val (clingon:getopt cmd :format))
         (*format* (parse-format format-val))
         (store (ensure-store)))
    (let* ((source-repo (clingon:getopt cmd :source-repo))
           (issues (ready-issues store :source-repo source-repo)))
      (unless (eq *format* :json)
        (if source-repo
            (format t "📋 Ready work for ~A (~D issues):~%~%" source-repo (length issues))
            (format t "📋 Ready work (~D issues):~%~%" (length issues))))
      (print-issues issues))))

(defun ready/command ()
  (clingon:make-command
   :name "ready"
   :description "Show issues ready to work on (unblocked)"
   :options (global-options)
   :handler #'ready/handler))

;;; ---------------------------------------------------------------------------
;;; Command: create
;;; ---------------------------------------------------------------------------

(defun create/options ()
  (list
   (clingon:make-option
    :string
    :short-name #\t
    :long-name "title"
    :description "Issue title"
    :key :title
    :required t)
   (clingon:make-option
    :string
    :short-name #\d
    :long-name "description"
    :description "Issue description"
    :key :description)
   (clingon:make-option
    :choice
    :short-name #\T
    :long-name "type"
    :description "Issue type"
    :key :type
    :items '("task" "bug" "feature" "epic" "chore" "docs")
    :initial-value "task")
   (clingon:make-option
    :choice
    :short-name #\p
    :long-name "priority"
    :description "Priority (P0-P4)"
    :key :priority
    :items '("P0" "P1" "P2" "P3" "P4")
    :initial-value "P2")
   (clingon:make-option
    :string
    :short-name #\a
    :long-name "assignee"
    :description "Assignee username"
    :key :assignee)
   (clingon:make-option
    :string
    :long-name "parent"
    :description "Parent issue ID for child issues"
    :key :parent)
   (clingon:make-option
    :string
    :long-name "blocks-on"
    :description "Issue ID that the new issue blocks on (adds dependency)"
    :key :blocks-on)))

(defun create/handler (cmd)
  (let* ((format-val (clingon:getopt cmd :format))
         (*format* (parse-format format-val))
         (store (ensure-store))
         (title (clingon:getopt cmd :title))
         (description (clingon:getopt cmd :description))
         (itype (parse-issue-type (clingon:getopt cmd :type)))
         (priority (parse-priority (clingon:getopt cmd :priority)))
         (assignee (clingon:getopt cmd :assignee))
         (parent (clingon:getopt cmd :parent))
         (blocks-on (clingon:getopt cmd :blocks-on))
         (source-repo (clingon:getopt cmd :source-repo)))
    (let ((issue (create-issue store
                               :title title
                               :description description
                               :type itype
                               :priority priority
                               :assignee assignee
                               :parent parent
                               :source-repo source-repo)))
      ;; Add blocking dependency if --blocks-on specified
      (when blocks-on
        (add-dependency store (issue-id issue) blocks-on))
      (unless (eq *format* :json)
        (format t "Created ~A~%" (issue-id issue)))
      (print-issue-single issue))))

(defun create/command ()
  (clingon:make-command
   :name "create"
   :description "Create a new issue"
   :aliases '("new" "c")
   :options (append (create/options) (global-options))
   :handler #'create/handler))

;;; ---------------------------------------------------------------------------
;;; Command: show
;;; ---------------------------------------------------------------------------

(defun show/handler (cmd)
  (let* ((format-val (clingon:getopt cmd :format))
         (*format* (parse-format format-val))
         (store (ensure-store))
         (id (first (clingon:command-arguments cmd))))
    (unless id
      (format *error-output* "Error: Issue ID required~%")
      (clingon:exit 1))
    (let ((issue (get-issue store id)))
      (print-issue-single issue))))

(defun show/command ()
  (clingon:make-command
   :name "show"
   :description "Show issue details"
   :aliases '("s")
   :options (global-options)
   :handler #'show/handler
   :usage "<issue-id>"))

;;; ---------------------------------------------------------------------------
;;; Command: update
;;; ---------------------------------------------------------------------------

(defun update/options ()
  (list
   (clingon:make-option
    :string
    :short-name #\t
    :long-name "title"
    :description "New title"
    :key :title)
   (clingon:make-option
    :string
    :short-name #\d
    :long-name "description"
    :description "New description"
    :key :description)
   (clingon:make-option
    :choice
    :short-name #\s
    :long-name "status"
    :description "New status"
    :key :status
    :items '("open" "in_progress" "blocked" "deferred" "closed"))
   (clingon:make-option
    :choice
    :short-name #\p
    :long-name "priority"
    :description "New priority"
    :key :priority
    :items '("P0" "P1" "P2" "P3" "P4"))
   (clingon:make-option
    :string
    :short-name #\a
    :long-name "assignee"
    :description "New assignee"
    :key :assignee)
   (clingon:make-option
    :string
    :short-name #\n
    :long-name "notes"
    :description "New notes"
    :key :notes)))

(defun update/handler (cmd)
  (let* ((format-val (clingon:getopt cmd :format))
         (*format* (parse-format format-val))
         (store (ensure-store))
         (id (first (clingon:command-arguments cmd))))
    (unless id
      (format *error-output* "Error: Issue ID required~%")
      (clingon:exit 1))
    (let* ((title (clingon:getopt cmd :title))
           (description (clingon:getopt cmd :description))
           (status (when-let (s (clingon:getopt cmd :status))
                     (parse-status s)))
           (priority (when-let (p (clingon:getopt cmd :priority))
                      (parse-priority p)))
            (assignee (clingon:getopt cmd :assignee))
            (notes (clingon:getopt cmd :notes)))
      (let ((issue (update-issue store id
                                :title title
                                :description description
                                :status status
                                :priority priority
                                :assignee assignee
                                :notes notes)))
        (unless (eq *format* :json)
          (format t "Updated ~A~%" (issue-id issue)))
        (print-issue-single issue)))))

(defun update/command ()
  (clingon:make-command
   :name "update"
   :description "Update an issue"
   :aliases '("u")
   :options (append (update/options) (global-options))
   :handler #'update/handler
   :usage "<issue-id>"))

;;; ---------------------------------------------------------------------------
;;; Command: close
;;; ---------------------------------------------------------------------------

(defun close/options ()
  (list
   (clingon:make-option
    :string
    :short-name #\r
    :long-name "reason"
    :description "Close reason (required)"
    :key :reason
    :required t)))

(defun close/handler (cmd)
  (let* ((format-val (clingon:getopt cmd :format))
         (*format* (parse-format format-val))
         (store (ensure-store))
         (id (first (clingon:command-arguments cmd)))
         (reason (clingon:getopt cmd :reason)))
    (unless id
      (format *error-output* "Error: Issue ID required~%")
      (clingon:exit 1))
    (let ((issue (close-issue store id :reason reason)))
      (unless (eq *format* :json)
        (format t "Closed ~A: ~A~%" (issue-id issue) reason))
      (print-issue-single issue))))

(defun close/command ()
  (clingon:make-command
   :name "close"
   :description "Close an issue"
   :options (append (close/options) (global-options))
   :handler #'close/handler
   :usage "<issue-id>"))

;;; ---------------------------------------------------------------------------
;;; Command: reopen
;;; ---------------------------------------------------------------------------

(defun reopen/handler (cmd)
  (let* ((format-val (clingon:getopt cmd :format))
         (*format* (parse-format format-val))
         (store (ensure-store))
         (id (first (clingon:command-arguments cmd))))
    (unless id
      (format *error-output* "Error: Issue ID required~%")
      (clingon:exit 1))
    (let ((issue (reopen-issue store id)))
      (unless (eq *format* :json)
        (format t "Reopened ~A~%" (issue-id issue)))
      (print-issue-single issue))))

(defun reopen/command ()
  (clingon:make-command
   :name "reopen"
   :description "Reopen a closed issue"
   :options (global-options)
   :handler #'reopen/handler
   :usage "<issue-id>"))

;;; ---------------------------------------------------------------------------
;;; Command: dep (subcommands)
;;; ---------------------------------------------------------------------------

(defun dep-add/options ()
  (list
   (clingon:make-option :string
     :long-name "blocks-on"
     :description "Issue ID that the child issue blocks on (parent ID)"
     :key :blocks-on)))

(defun dep-add/handler (cmd)
  (let* ((store (ensure-store))
         (args (clingon:command-arguments cmd))
         (child (first args))
         (parent (or (clingon:getopt cmd :blocks-on) (second args))))
    (unless (and child parent)
      (format *error-output* "Usage: bw dep add <child-id> [--blocks-on <parent-id>]~%")
      (clingon:exit 1))
    (add-dependency store child parent)
    (format t "Added dependency: ~A blocks ~A~%" child parent)))

(defun dep-add/command ()
  (clingon:make-command
   :name "add"
   :description "Add a dependency"
   :options (dep-add/options)
   :handler #'dep-add/handler
   :usage "<child-id> [--blocks-on <parent-id>]"))

(defun dep-remove/handler (cmd)
  (let* ((store (ensure-store))
         (args (clingon:command-arguments cmd))
         (child (first args))
         (parent (second args)))
    (unless (and child parent)
      (format *error-output* "Usage: bw dep remove <child-id> <parent-id>~%")
      (clingon:exit 1))
    (remove-dependency store child parent)
    (format t "Removed dependency: ~A -> ~A~%" child parent)))

(defun dep-remove/command ()
  (clingon:make-command
   :name "remove"
   :description "Remove a dependency"
   :handler #'dep-remove/handler
   :usage "<child-id> <parent-id>"))

(defun dep-list/handler (cmd)
  (let* ((format-val (clingon:getopt cmd :format))
         (*format* (parse-format format-val))
         (store (ensure-store))
         (id (first (clingon:command-arguments cmd))))
    (unless id
      (format *error-output* "Usage: bw dep list <issue-id>~%")
      (clingon:exit 1))
    (let ((deps (list-dependencies store id)))
      (format t "Dependencies for ~A:~%" id)
      (dolist (dep deps)
        (format t "  ~A -> ~A (~A)~%"
                (dependency-issue-id dep)
                (dependency-depends-on-id dep)
                (dependency-type-string (dependency-dep-type dep)))))))

(defun dep-list/command ()
  (clingon:make-command
   :name "list"
   :description "List dependencies for an issue"
   :aliases '("ls")
   :options (global-options)
   :handler #'dep-list/handler
   :usage "<issue-id>"))

(defun dep/command ()
  (clingon:make-command
   :name "dep"
   :description "Manage dependencies"
   :sub-commands (list (dep-add/command)
                       (dep-remove/command)
                       (dep-list/command))))

;;; ---------------------------------------------------------------------------
;;; Command: label (subcommands)
;;; ---------------------------------------------------------------------------

(defun label-add/handler (cmd)
  (let* ((store (ensure-store))
         (args (clingon:command-arguments cmd))
         (issue-id (first args))
         (label (second args)))
    (unless (and issue-id label)
      (format *error-output* "Usage: bw label add <issue-id> <label>~%")
      (clingon:exit 1))
    (add-label store issue-id label)
    (format t "Added label '~A' to ~A~%" label issue-id)))

(defun label-add/command ()
  (clingon:make-command
   :name "add"
   :description "Add a label to an issue"
   :handler #'label-add/handler
   :usage "<issue-id> <label>"))

(defun label-remove/handler (cmd)
  (let* ((store (ensure-store))
         (args (clingon:command-arguments cmd))
         (issue-id (first args))
         (label (second args)))
    (unless (and issue-id label)
      (format *error-output* "Usage: bw label remove <issue-id> <label>~%")
      (clingon:exit 1))
    (remove-label store issue-id label)
    (format t "Removed label '~A' from ~A~%" label issue-id)))

(defun label-remove/command ()
  (clingon:make-command
   :name "remove"
   :description "Remove a label from an issue"
   :handler #'label-remove/handler
   :usage "<issue-id> <label>"))

(defun label-list/handler (cmd)
  (let* ((store (ensure-store))
         (issue-id (first (clingon:command-arguments cmd))))
    (if issue-id
        (let ((labels (get-labels store issue-id)))
          (format t "Labels for ~A:~%" issue-id)
          (dolist (l labels)
            (format t "  ~A~%" l)))
        (let ((labels (list-all-labels store)))
          (format t "All labels (~D):~%" (length labels))
          (dolist (l labels)
            (format t "  ~A~%" l))))))

(defun label-list/command ()
  (clingon:make-command
   :name "list"
   :description "List labels for an issue or all labels"
   :aliases '("ls")
   :handler #'label-list/handler
   :usage "[issue-id]"))

(defun label/command ()
  (clingon:make-command
   :name "label"
   :description "Manage labels"
   :sub-commands (list (label-add/command)
                       (label-remove/command)
                       (label-list/command))))

;;; ---------------------------------------------------------------------------
;;; Command: init
;;; ---------------------------------------------------------------------------

(defun init/handler (cmd)
  (let ((path (or (clingon:getopt cmd :path)
                  (uiop:getcwd))))
    (let ((beads-dir (merge-pathnames ".beads/" path)))
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
  (let* ((store (ensure-store))
         (direction (clingon:getopt cmd :direction)))
    (cond
      ((string-equal direction "export")
       (let ((path (merge-pathnames "issues.jsonl" (find-beads-dir))))
         (export-jsonl store (namestring path))
         (format t "Exported to ~A~%" path)))
      ((string-equal direction "import")
       (let ((path (merge-pathnames "issues.jsonl" (find-beads-dir))))
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
         (store (ensure-store))
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
         (store (ensure-store))
         (status-dir (or (clingon:getopt cmd :status-dir)
                         (find-status-docs-dir))))
    (unless status-dir
      (format *error-output* "Error: No status docs directory found.~%")
      (clingon:exit 2))
    (let ((result (run-doctor-status-docs store status-dir)))
      (when (getf result :error)
        (clingon:exit 2))
      (let* ((summary (getf result :summary))
             (ok (getf summary :ok))
             (stale (getf summary :stale))
             (missing (getf summary :missing))
             (orphan (getf summary :orphan))
             (healthy (and (zerop stale) (zerop missing) (zerop orphan))))
        (if (eq *format* :json)
            (format t "~A"
                    (jzon:stringify
                     (list (list "ok" ok)
                           (list "stale" stale)
                           (list "missing" missing)
                           (list "orphan" orphan)
                           (list "healthy" healthy))))
            (format t "~D OK, ~D STALE, ~D MISSING, ~D ORPHAN — exit ~D~%"
                    ok stale missing orphan (if healthy 0 1)))
        (clingon:exit (if healthy 0 1))))))

(defun doctor-health/command ()
  (clingon:make-command
   :name "health"
   :description "One-line doctor summary for cold-start integration"
   :options (global-options)
   :handler #'doctor-health/handler))

(defun doctor/handler (cmd)
  "Flat 'bw doctor' — runs status-docs check by default."
  (doctor-status-docs/handler cmd))

(defun doctor/command ()
  (clingon:make-command
   :name "doctor"
   :description "Run project-health diagnostics (status doc ↔ issue sync)"
   :options (append (doctor/options) (global-options))
   :handler #'doctor/handler
   :sub-commands (list (doctor-status-docs/command)
                       (doctor-health/command))))

;;; ---------------------------------------------------------------------------
;;; Top-level command
;;; ---------------------------------------------------------------------------

(defun top-level/handler (cmd)
  (clingon:print-usage-and-exit cmd t))

(defun top-level/command ()
  (clingon:make-command
   :name "bw"
   :version "0.1.0"
   :description "Beadwork - Common Lisp issue tracker"
   :authors '("Joel Boehland")
   :license "MIT"
   :options (global-options)
   :handler #'top-level/handler
   :sub-commands (list (list/command)
                       (ready/command)
                       (create/command)
                       (show/command)
                       (update/command)
                       (close/command)
                       (reopen/command)
                       (dep/command)
                       (label/command)
                       (doctor/command)
                       (init/command)
                       (sync/command))))

(defun main ()
  "Entry point for the bw executable."
  (let ((app (top-level/command)))
    (clingon:run app)))
