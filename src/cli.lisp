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

(defun comment->cli-json (comment)
  "Build the CLI JSON hash-table for COMMENT.

A plist of pairs stringifies to a JSON array of arrays in jzon; consumers
parse objects."
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash "id" ht) (comment-id comment))
    (setf (gethash "author" ht) (comment-author comment))
    (setf (gethash "body" ht) (comment-body comment))
    (setf (gethash "created-at" ht) (format-timestamp (comment-created-at comment)))
    ht))

(defun issue->cli-json (issue &optional store &key include-comments)
  "Build the CLI JSON object for ISSUE.  When STORE is given, include the
graph neighborhood: parent, children, dependencies (outgoing) and
dependents (incoming). When INCLUDE-COMMENTS is true, also include the
issue's comments (single-issue views only; list views stay compact)."
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
                      'vector)))
      (when include-comments
        (setf (gethash "comments" ht)
              (coerce (mapcar #'comment->cli-json
                              (list-comments store (issue-id issue)))
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

(defun print-issue-comments (store issue)
  "Print ISSUE's comments when it has any."
  (let ((comments (list-comments store (issue-id issue))))
    (when comments
      (format t "~%Comments (~D):~%" (length comments))
      (dolist (comment comments)
        (format t "  [~A] ~A~%  ~A~%"
                (format-timestamp (comment-created-at comment))
                (comment-author comment)
                (comment-body comment))))))

(defun print-issue-single (issue &optional store)
  "Print a single issue in detail.  When STORE is given, the plain/rich
forms append the issue's comments and graph neighborhood, and the JSON form
includes them."
  (ecase *format*
    (:json
     (format t "~A" (jzon:stringify (issue->cli-json issue store
                                                   :include-comments t)
                                     :pretty t)))
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
       (print-issue-comments store issue)
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
       (print-issue-comments store issue)
       (print-issue-neighbors store issue)))))

;;; ---------------------------------------------------------------------------
;;; Global Options
;;; ---------------------------------------------------------------------------

(defun global-options (&key (repo-role :filter))
  "Global options available to all commands.

REPO-ROLE says what --repo does in this command, because it is one option with
three jobs: :FILTER (the default) narrows list/ready/show, :SETTER writes the
issue's source repo (create/update), and :DEFAULT supplies the fallback board
for issues a batch creates.  A single 'Filter by source repository' line on
create/update reads as 'a no-op here' and hid the fact that bw can set the
field at all (bd-l3s)."
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
    :description (ecase repo-role
                   (:filter
                    "Filter by source repository. May be specified multiple times (e.g. --repo csct --repo beadwork)")
                   (:setter
                    "Set the issue's source repository (e.g. --repo cogen-cli)")
                   (:default
                    "Default source repository for issues this batch creates (a per-op \"repo\" field wins)"))
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
;;; List scope header (used by the list command in src/commands/issue.lisp)
;;; ---------------------------------------------------------------------------

(defun list-scope-line (source-repo count)
  "Human header for `bw list`, naming the repo filter in effect.

Without it a scoped list looks like the whole board, so work tagged in a
sibling repo appears not to exist (friction report, 2026-10-21)."
  (if source-repo
      (format nil "Listing ~D issue(s) for ~{~A~^, ~} (use --all-repos for the whole project)"
              count source-repo)
      (format nil "Listing ~D issue(s) across all repos" count)))

