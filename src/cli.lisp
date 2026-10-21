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

(defvar *no-color* nil
  "When T, suppress all ANSI color/formatting codes in output.
Set by the global --plain/--no-color flag.")

(defvar *verbose* 0
  "Verbosity level counter.")

(defun find-beads-dir ()
  "Find .beads/ directory starting from current dir upward."
  (let ((dir (uiop:getcwd)))
    (loop
      (let ((beads (merge-pathnames ".beads/" dir)))
        (when (probe-file beads)
          (return beads))
        (let ((parent (uiop:pathname-parent-directory-pathname dir)))
          (when (or (null parent) (equal parent dir))
            (return nil))
          (setf dir parent))))))

(defun detect-source-repo ()
  "Walk up from cwd to find the nearest .git/ directory (stopping at the
project root where .beads/ lives).  Return that directory's name as the
source-repo.  Returns \".\" when no git repo is found before the project root.

Examples:
  repos/beadwork/src/ -> finds .git at repos/beadwork/ -> \"beadwork\"
  worktrees/cogen-kb/feature/ -> finds .git at worktrees/cogen-kb/ -> \"cogen-kb\"
  project root (cogen-meta/) -> no .git before .beads/ -> \".\""
  (let* ((beads-dir (find-beads-dir))
         (project-root (when beads-dir
                         (uiop:pathname-parent-directory-pathname beads-dir)))
         (dir (uiop:getcwd)))
    (unless project-root
      (return-from detect-source-repo "."))
    (loop
      (let ((git-dir (merge-pathnames ".git/" dir)))
        (when (probe-file git-dir)
          (let ((name (car (last (pathname-directory dir)))))
            (return-from detect-source-repo
              (if name (string-downcase name) ".")))))
      (when (or (null dir)
                (equal dir project-root)
                (not (uiop:subpathp dir project-root)))
        (return "."))
      (setf dir (uiop:pathname-parent-directory-pathname dir)))))

(defun find-db-path ()
  "Find the SQLite database path."
  (let ((beads-dir (find-beads-dir)))
    (unless beads-dir
      (error 'beadwork-error :message "No .beads/ directory found. Run 'bw init' first."))
    (merge-pathnames "beads.db" beads-dir)))

(defun resolve-db-path (db-dir)
  "Resolve the SQLite database path.

If DB-DIR (the --db option value) is given, normalize it as a directory,
ensure it exists and return <DB-DIR>/beads.db.  Otherwise auto-detect
via find-beads-dir."
  (if db-dir
      (let ((dir (uiop:ensure-directory-pathname db-dir)))
        (ensure-directories-exist dir)
        (namestring (merge-pathnames "beads.db" dir)))
      (namestring (find-db-path))))

(defun resolve-store (cmd)
  "Get or create the current store, honoring the --db option.

Uses GETOPT* so the flag is honored whether given before or after the
subcommand (e.g. 'bw --db DIR create ...' and 'bw create --db DIR ...')."
  (unless *store*
    (setf *store* (open-store (resolve-db-path (clingon:getopt* cmd :db-path)))))
  *store*)

(defun ensure-store (cmd)
  "Ensure store is open, or exit with error."
  (handler-case
      (resolve-store cmd)
    (error (c)
      (format *error-output* "Error: ~A~%" c)
      (clingon:exit 1))))

;;; ---------------------------------------------------------------------------
;;; Output Formatting
;;; ---------------------------------------------------------------------------

;;; ANSI helpers

(defparameter *ansi-reset*  (format nil "~C[0m" #\Esc))
(defparameter *ansi-bold*   (format nil "~C[1m" #\Esc))
(defparameter *ansi-red*    (format nil "~C[31m" #\Esc))
(defparameter *ansi-green*  (format nil "~C[32m" #\Esc))
(defparameter *ansi-yellow* (format nil "~C[33m" #\Esc))
(defparameter *ansi-cyan*   (format nil "~C[36m" #\Esc))
(defparameter *ansi-dim*    (format nil "~C[2m" #\Esc))

(defun visible-length (s)
  "Return the visible character length, stripping ANSI escape sequences."
  (length (cl-ppcre:regex-replace-all "\\e\\[[0-9;]*m" s "")))

(defun pad-right (s width)
  "Right-pad S with spaces to at least WIDTH visible characters."
  (let* ((vlen (visible-length s))
         (pad (max 0 (- width vlen))))
    (concatenate 'string s (make-string pad :initial-element #\Space))))

(defun color-status (status)
  "Return ANSI-colored status string, or plain when *no-color* is T."
  (let ((s (string-upcase (symbol-name status))))
    (if *no-color*
        s
        (cond
          ((eq status :in-progress) (concatenate 'string *ansi-green* s *ansi-reset*))
          ((eq status :blocked)    (concatenate 'string *ansi-red* s *ansi-reset*))
          ((eq status :deferred)   (concatenate 'string *ansi-yellow* s *ansi-reset*))
          (t s)))))

(defun color-priority (p)
  "Return ANSI-colored priority string, or plain when *no-color* is T."
  (let ((s (format-priority p)))
    (if *no-color*
        s
        (cond
          ((= p 1) (concatenate 'string *ansi-bold* *ansi-red* s *ansi-reset*))
          ((= p 2) (concatenate 'string *ansi-yellow* s *ansi-reset*))
          ((= p 0) (concatenate 'string *ansi-bold* *ansi-red* "!!" s *ansi-reset*))
          (t s)))))

(defun format-issue-plain (issue)
  "Format ISSUE as plain text line."
  (format nil "[~A] [~A] [~A] ~A  ~A"
          (issue-id issue)
          (format-priority (issue-priority issue))
          (issue-type-string (issue-type issue))
          (or (issue-source-repo issue) ".")
          (issue-title issue)))

(defun format-issue-rich (issue id-w status-w pri-w type-w repo-w)
  "Format ISSUE for terminal display with aligned columns and color."
  (let* ((status (issue-status issue))
         (repo (or (issue-source-repo issue) "."))
         (id-str (issue-id issue))
         (status-str (color-status status))
         (pri-str (color-priority (issue-priority issue)))
         (type-str (string-downcase (symbol-name (issue-type issue))))
         (repo-str repo)
         (title-str (issue-title issue)))
    (format nil "~A  ~A  ~A  ~A  ~A  ~A"
            (pad-right id-str id-w)
            (pad-right status-str status-w)
            (pad-right pri-str pri-w)
            (pad-right type-str type-w)
            (pad-right repo-str repo-w)
            title-str)))

;;; ---------------------------------------------------------------------------
;;; Graph neighbors (bd-uz3)
;;; ---------------------------------------------------------------------------

(defun %issue-brief-json (issue &optional relation)
  "Build a compact JSON object for ISSUE, optionally tagged with RELATION."
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash "id" ht) (issue-id issue))
    (setf (gethash "title" ht) (issue-title issue))
    (setf (gethash "status" ht)
          (string-downcase (symbol-name (issue-status issue))))
    (setf (gethash "priority" ht) (issue-priority issue))
    (setf (gethash "issue-type" ht) (issue-type-string (issue-type issue)))
    (when relation
      (setf (gethash "relation" ht) (dependency-type-string relation)))
    ht))

(defun %issue-neighbors (store issue)
  "Return a plist describing ISSUE's graph neighborhood.
:PARENT is a parent id or NIL; :CHILDREN is a list of child issues;
:DEPENDENCIES and :DEPENDENTS are lists of (relation . issue) pairs for
the outgoing and incoming non parent-child edges."
  (let* ((id (issue-id issue))
         (parent (get-parent-id store id))
         (children (list-children store id))
         (dependencies
           ;; Skip a dangling endpoint rather than signalling (bd-wux).
           (loop for dep in (list-dependencies store id)
                 unless (eq (dependency-dep-type dep) :parent-child)
                   append (let ((target (find-issue store
                                                    (dependency-depends-on-id dep))))
                            (when target
                              (list (cons (dependency-dep-type dep) target))))))
         (dependents
           (loop for dep in (list-dependents store id)
                 unless (eq (dependency-dep-type dep) :parent-child)
                   append (let ((source (find-issue store (dependency-issue-id dep))))
                            (when source
                              (list (cons (dependency-dep-type dep) source)))))))
    (list :parent parent :children children
          :dependencies dependencies :dependents dependents)))

(defun issue->cli-json (issue &optional store)
  "Build the CLI JSON object for ISSUE.  When STORE is given, include the
graph neighborhood: parent, children, dependencies (outgoing) and
dependents (incoming)."
  (let ((ht (make-hash-table :test #'equal)))
    (dolist (field (jzon:coerced-fields issue))
      (destructuring-bind (name value &optional type) field
        (declare (ignore type))
        (setf (gethash (string name) ht) value)))
    (when store
      (let ((neighbors (%issue-neighbors store issue)))
        (setf (gethash "parent" ht) (or (getf neighbors :parent) 'null))
        (setf (gethash "children" ht)
              (coerce (mapcar #'%issue-brief-json
                              (getf neighbors :children))
                      'vector))
        (setf (gethash "dependencies" ht)
              (coerce (mapcar (lambda (pair)
                                (%issue-brief-json (cdr pair) (car pair)))
                              (getf neighbors :dependencies))
                      'vector))
        (setf (gethash "dependents" ht)
              (coerce (mapcar (lambda (pair)
                                (%issue-brief-json (cdr pair) (car pair)))
                              (getf neighbors :dependents))
                      'vector))))
    ht))

(defun %print-issue-brief (issue relation)
  "Print one compact relationship line for ISSUE."
  (format t "    ~A  [~A]  ~A  ~A~@[  (~A)~]  ~A~%"
          (issue-id issue)
          (string-downcase (symbol-name (issue-status issue)))
          (format-priority (issue-priority issue))
          (string-downcase (symbol-name (issue-type issue)))
          (when relation (dependency-type-string relation))
          (issue-title issue)))

(defun print-issue-neighbors (store issue)
  "Print ISSUE's graph neighborhood (parent, children, dependencies,
dependents) with one line per neighbor."
  (let ((neighbors (%issue-neighbors store issue)))
    (let ((parent (getf neighbors :parent))
          (children (getf neighbors :children))
          (dependencies (getf neighbors :dependencies))
          (dependents (getf neighbors :dependents)))
      (when (or parent children dependencies dependents)
        (format t "~%Relationships:~%")
        (when parent
          (let ((parent-issue (find-issue store parent)))
            (when parent-issue
              (format t "  Parent:~%")
              (%print-issue-brief parent-issue nil))))
        (when children
          (format t "  Children:~%")
          (dolist (child children)
            (%print-issue-brief child nil)))
        (when dependencies
          (format t "  Depends on (blocked by):~%")
          (dolist (pair dependencies)
            (%print-issue-brief (cdr pair) (car pair))))
        (when dependents
          (format t "  Required by (blocks):~%")
          (dolist (pair dependents)
            (%print-issue-brief (cdr pair) (car pair))))))))

(defun print-issues (issues &optional store)
  "Print a list of issues according to *format*.  When STORE is given,
the JSON form includes each issue's graph neighborhood."
  (ecase *format*
    (:json
     (format t "~A" (jzon:stringify
                     (or (mapcar (lambda (i) (issue->cli-json i store)) issues)
                         #())
                     :pretty t)))
    (:plain
     (dolist (issue issues)
       (format t "~A~%" (format-issue-plain issue))))
    (:rich
     (let* ((id-w (max 12 (reduce #'max (mapcar (lambda (i) (length (issue-id i)))
                                                 issues)
                                   :initial-value 0)))
            (status-w (max 6 (reduce #'max (mapcar (lambda (i)
                                                      (visible-length
                                                       (color-status (issue-status i))))
                                                    issues)
                                      :initial-value 0)))
            (pri-w (max 3 (reduce #'max (mapcar (lambda (i)
                                                   (visible-length
                                                    (color-priority (issue-priority i))))
                                                 issues)
                                   :initial-value 0)))
            (type-w (max 4 (reduce #'max (mapcar (lambda (i)
                                                    (length (string-downcase
                                                             (symbol-name (issue-type i)))))
                                                  issues)
                                    :initial-value 0)))
            (repo-w (max 4 (reduce #'max (mapcar (lambda (i)
                                                    (length (or (issue-source-repo i) ".")))
                                                  issues)
                                    :initial-value 0))))
       ;; Header
       (format t "~A  ~A  ~A  ~A  ~A  ~A~%"
               (pad-right "ID" id-w)
               (pad-right "STATUS" status-w)
               (pad-right "PRI" pri-w)
               (pad-right "TYPE" type-w)
               (pad-right "REPO" repo-w)
               "TITLE")
       (let ((id-underline (make-string id-w :initial-element #\-))
             (status-underline (make-string status-w :initial-element #\-))
             (pri-underline (make-string pri-w :initial-element #\-))
             (type-underline (make-string type-w :initial-element #\-))
             (repo-underline (make-string repo-w :initial-element #\-)))
         (format t "~A  ~A  ~A  ~A  ~A  ~A~%"
                 id-underline status-underline pri-underline
                 type-underline repo-underline "-----"))
       ;; Rows
       (loop for issue in issues
             do (format t "~A~%"
                        (format-issue-rich issue
                                           id-w status-w pri-w type-w repo-w)))))))

(defun print-issue-single (issue &optional store)
  "Print a single issue in detail.  When STORE is given, the plain/rich
forms append the issue's graph neighborhood and the JSON form includes it."
  (ecase *format*
    (:json
     (format t "~A" (jzon:stringify (issue->cli-json issue store) :pretty t)))
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
       (format t "~%~A~%" (issue-description issue)))
     (when (and (issue-notes issue)
                (plusp (length (issue-notes issue))))
       (format t "~%Notes:~%  ~A~%" (issue-notes issue)))
     (when store
       (print-issue-neighbors store issue)))
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
       (format t "~%~A~%" (issue-description issue)))
     (when (and (issue-notes issue)
                (plusp (length (issue-notes issue))))
       (format t "~%Notes:~%  ~A~%" (issue-notes issue)))
     (when store
       (print-issue-neighbors store issue)))))

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
    :list
    :long-name "repo"
    :description "Filter by source repository. May be specified multiple times (e.g. --repo csct --repo beadwork)"
    :key :source-repo)
   (clingon:make-option
    :boolean/true
    :long-name "all-repos"
    :description "Show issues from all repositories (disable source-repo filtering)"
    :key :all-repos)
   (clingon:make-option
    :boolean/true
    :long-name "plain"
    :short-name #\P
    :description "Suppress ANSI color/formatting codes in output"
    :key :no-color)))

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
    :items '("task" "bug" "feature" "epic" "chore" "docs" "question"))
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

(defun list-scope-line (source-repo count)
  "Human header for `bw list`, naming the repo filter in effect.

Without it a scoped list looks like the whole board, so work tagged in a
sibling repo appears not to exist (friction report, 2026-10-21)."
  (if source-repo
      (format nil "Listing ~D issue(s) for ~{~A~^, ~} (use --all-repos for the whole project)"
              count source-repo)
      (format nil "Listing ~D issue(s) across all repos" count)))

(defun list/handler (cmd)
  (let* ((format-val (clingon:getopt cmd :format))
         (*format* (parse-format format-val))
       (*no-color* (clingon:getopt* cmd :no-color))
         (store (ensure-store cmd))
         (status (when-let (s (clingon:getopt cmd :status))
                   (parse-status s)))
         (priority (when-let (p (clingon:getopt cmd :priority))
                    (parse-priority p)))
         (itype (when-let (type-val (clingon:getopt cmd :type))
                  (parse-issue-type type-val)))
         (assignee (clingon:getopt cmd :assignee))
         (limit (clingon:getopt cmd :limit))
         (all-repos (clingon:getopt cmd :all-repos))
         (explicit-repo (clingon:getopt cmd :source-repo))
         (auto-repo (detect-source-repo))
         (source-repo (cond
                        (all-repos nil)
                        (explicit-repo explicit-repo)
                        ((string= auto-repo ".") nil)
                        (t (list auto-repo)))))
    (let ((issues (list-issues store
                               :status status
                               :priority priority
                               :type itype
                               :assignee assignee
                               :limit limit
                               :source-repo source-repo)))
      (unless (eq *format* :json)
        (format t "~A~%~%" (list-scope-line source-repo (length issues))))
      (print-issues issues store))))

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
       (*no-color* (clingon:getopt* cmd :no-color))
         (store (ensure-store cmd)))
    (let* ((all-repos (clingon:getopt cmd :all-repos))
           (explicit-repo (clingon:getopt cmd :source-repo))
           (auto-repo (detect-source-repo))
           (source-repo (cond
                          (all-repos nil)
                          (explicit-repo explicit-repo)
                          ((string= auto-repo ".") nil)
                          (t (list auto-repo))))
           (issues (ready-issues store :source-repo source-repo)))
      (unless (eq *format* :json)
        (if source-repo
            (format t "Ready work for ~{~A~^, ~} (~D issues):~%~%" source-repo (length issues))
            (format t "Ready work (~D issues):~%~%" (length issues))))
      (print-issues issues store))))

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
    :items '("task" "bug" "feature" "epic" "chore" "docs" "question")
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

(defun resolve-create-repo (store parent explicit-repo)
  "Return the source repo for a new issue created under PARENT.

EXPLICIT-REPO always wins. Otherwise a child inherits its parent's repo -- a
child filed under an epic should stay on the same board unless told otherwise
(asking the agent to retype the epic's repo is how children get misfiled).
A top-level issue has no default: `--repo` is mandatory so nothing lands in the
unscoped \".\" bucket by accident (bd-ogg). Returns NIL when nothing could be
resolved.

Signals ISSUE-NOT-FOUND when PARENT does not exist -- even when EXPLICIT-REPO
is given -- instead of letting a nonexistent parent reach the foreign-key
constraint (bd-193)."
  (let ((parent-repo (when parent (issue-source-repo (get-issue store parent)))))
    (cond
      (explicit-repo (if (listp explicit-repo) (first explicit-repo) explicit-repo))
      (parent parent-repo)
      (t nil))))

(defun create/handler (cmd)
  (let* ((format-val (clingon:getopt cmd :format))
         (*format* (parse-format format-val))
       (*no-color* (clingon:getopt* cmd :no-color))
         (store (ensure-store cmd))
         (title (clingon:getopt cmd :title))
         (description (clingon:getopt cmd :description))
         (itype (parse-issue-type (clingon:getopt cmd :type)))
         (priority (parse-priority (clingon:getopt cmd :priority)))
         (assignee (clingon:getopt cmd :assignee))
         (parent (clingon:getopt cmd :parent))
         (blocks-on (clingon:getopt cmd :blocks-on))
         (explicit-repo (clingon:getopt cmd :source-repo))
         (source-repo (resolve-create-repo store parent explicit-repo)))
    (unless source-repo
      (format *error-output* "Error: --repo is required. Pass --repo <name> to assign the issue to a repository.~%")
      (clingon:exit 1))
    (when (member source-repo '("." "") :test #'equal)
      (format *error-output*
              "warning: parent ~A has no source repo; the child inherits \".\" and will be unscoped~%"
              parent))
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
       (*no-color* (clingon:getopt* cmd :no-color))
         (store (ensure-store cmd))
         (id (first (clingon:command-arguments cmd))))
    (unless id
      (format *error-output* "Error: Issue ID required~%")
      (clingon:exit 1))
    (let ((issue (get-issue store id)))
      (print-issue-single issue store))))

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
       (*no-color* (clingon:getopt* cmd :no-color))
         (store (ensure-store cmd))
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
            (notes (clingon:getopt cmd :notes))
            (repo-val (clingon:getopt cmd :source-repo))
            (source-repo (when repo-val (first repo-val))))
      (let ((issue (update-issue store id
                                :title title
                                :description description
                                :status status
                                :priority priority
                                :assignee assignee
                                :notes notes
                                :source-repo source-repo)))
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
    :description "Close reason"
    :key :reason)
   (clingon:make-option
    :string
    :short-name #\m
    :long-name "message"
    :description "Close message (alias for --reason)"
    :key :message)))

(defun close/handler (cmd)
  (let* ((format-val (clingon:getopt cmd :format))
         (*format* (parse-format format-val))
       (*no-color* (clingon:getopt* cmd :no-color))
         (store (ensure-store cmd))
         (id (first (clingon:command-arguments cmd)))
         (reason (or (clingon:getopt cmd :reason)
                     (clingon:getopt cmd :message))))
    (unless id
      (format *error-output* "Error: Issue ID required~%")
      (clingon:exit 1))
    (unless reason
      (format *error-output* "Error: close reason required (-r/--reason or -m/--message)~%")
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
       (*no-color* (clingon:getopt* cmd :no-color))
         (store (ensure-store cmd))
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
;;; Command: delete
;;; ---------------------------------------------------------------------------

(defun delete/handler (cmd)
  (let* ((store (ensure-store cmd))
         (id (first (clingon:command-arguments cmd)))
         (force (clingon:getopt cmd :force)))
    (unless id
      (format *error-output* "Usage: bw delete <id> [-f]~%")
      (clingon:exit 1))
    (unless force
      (format t "Delete ~A? [y/N] " id)
      (finish-output)
      (let ((response (string-downcase (read-line))))
        (unless (string= response "y")
          (format t "Cancelled.~%")
          (clingon:exit 0))))
    (handler-case
        (progn
          (delete-issue store id)
          (format t "Deleted ~A~%" id))
      (issue-not-found ()
        (format *error-output* "Issue ~A not found.~%" id)
        (clingon:exit 1)))))

(defun delete/options ()
  (list
   (clingon:make-option
    :boolean/true
    :short-name #\f
    :long-name "force"
    :description "Skip confirmation prompt"
    :key :force)))

(defun delete/command ()
  (clingon:make-command
   :name "delete"
   :description "Delete an issue (requires confirmation)"
   :aliases '("rm")
   :options (append (delete/options) (global-options))
   :handler #'delete/handler
   :usage "<id>"))

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
  (let* ((store (ensure-store cmd))
         (beads-dir (or (clingon:getopt* cmd :db-path) (find-beads-dir)))
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

