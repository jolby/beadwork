(in-package :beadwork)

;;; ============================================================================
;;; Commands: issue (list, ready, create, show, update, close, reopen, delete)
;;; ============================================================================

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
      ;; Pass STORE so the returned object carries the graph neighborhood,
      ;; including the parent link written above; without it `create --parent`
      ;; reported "parent": null while `show` reported the real id (bd-juc).
      (print-issue-single issue store))))

(defun create/command ()
  (clingon:make-command
   :name "create"
   :description "Create a new issue"
   :aliases '("new" "c")
   :options (append (create/options) (global-options :repo-role :setter))
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
    :description "Replace notes"
    :key :notes)
   (clingon:make-option
    :string
    :long-name "append-notes"
    :description "Append to notes instead of replacing them"
    :key :append-notes)))

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
            (append-notes (clingon:getopt cmd :append-notes))
            (repo-val (clingon:getopt cmd :source-repo))
            (source-repo (when repo-val (first repo-val))))
      (let ((issue (update-issue store id
                                :title title
                                :description description
                                :status status
                                :priority priority
                                :assignee assignee
                                :notes notes
                                :append-notes append-notes
                                :source-repo source-repo)))
        (unless (eq *format* :json)
          (format t "Updated ~A~%" (issue-id issue)))
        (print-issue-single issue store)))))

(defun update/command ()
  (clingon:make-command
   :name "update"
   :description "Update an issue"
   :aliases '("u")
   :options (append (update/options) (global-options :repo-role :setter))
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
      (print-issue-single issue store))))

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
      (print-issue-single issue store))))

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
