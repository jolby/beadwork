(in-package :beadwork)

;;; ============================================================================
;;; Commands: graph (dep, label, comment, blocked)
;;; ============================================================================

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
  (let* ((store (ensure-store cmd))
         (args (clingon:command-arguments cmd))
         (child (first args))
         (parent (or (clingon:getopt cmd :blocks-on) (second args))))
    (unless (and child parent)
      (format *error-output* "Usage: bw dep add <child-id> [--blocks-on <parent-id>]~%")
      (clingon:exit 1))
    (add-dependency store child parent)
    (format t "Added dependency: ~A is blocked by ~A~%" child parent)))

(defun dep-add/command ()
  (clingon:make-command
   :name "add"
   :description "Add a dependency"
   :options (append (dep-add/options) (global-options))
   :handler #'dep-add/handler
   :usage "<child-id> [--blocks-on <parent-id>]"))

(defun dep-remove/handler (cmd)
  (let* ((store (ensure-store cmd))
         (args (clingon:command-arguments cmd))
         (child (first args))
         (parent (second args)))
    (unless (and child parent)
      (format *error-output* "Usage: bw dep remove <child-id> <parent-id>~%")
      (clingon:exit 1))
    (remove-dependency store child parent)
    (format t "Removed dependency: ~A no longer depends on ~A~%" child parent)))

(defun dep-remove/command ()
  (clingon:make-command
   :name "remove"
   :description "Remove a dependency"
   :options (global-options)
   :handler #'dep-remove/handler
   :usage "<child-id> <parent-id>"))

(defun dep-list/handler (cmd)
  (let* ((format-val (clingon:getopt cmd :format))
         (*format* (parse-format format-val))
       (*no-color* (clingon:getopt* cmd :no-color))
         (store (ensure-store cmd))
         (id (first (clingon:command-arguments cmd))))
    (unless id
      (format *error-output* "Usage: bw dep list <issue-id>~%")
      (clingon:exit 1))
    (let ((deps (list-dependencies store id)))
      (format t "Dependencies for ~A (this issue depends on / is blocked by):~%" id)
      (dolist (dep deps)
        (format t "  ~A  (~A)~%"
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
  (let* ((store (ensure-store cmd))
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
   :options (global-options)
   :handler #'label-add/handler
   :usage "<issue-id> <label>"))

(defun label-remove/handler (cmd)
  (let* ((store (ensure-store cmd))
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
   :options (global-options)
   :handler #'label-remove/handler
   :usage "<issue-id> <label>"))

(defun label-list/handler (cmd)
  (let* ((store (ensure-store cmd))
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
   :options (global-options)
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
;;; Command: comment (subcommands)
;;; ---------------------------------------------------------------------------

(defun comment-add/handler (cmd)
  (let* ((store (ensure-store cmd))
         (args (clingon:command-arguments cmd))
         (issue-id (first args))
         (text (second args)))
    (unless (and issue-id text)
      (format *error-output* "Usage: bw comment add <issue-id> <text>~%")
      (clingon:exit 1))
    (add-comment store issue-id "[cli]" text)
    (format t "Added comment to ~A~%" issue-id)))

(defun comment-add/command ()
  (clingon:make-command
   :name "add"
   :description "Add a comment to an issue"
   :options (global-options)
   :handler #'comment-add/handler
   :usage "<issue-id> <text>"))

(defun comment-list/handler (cmd)
  (let* ((format-val (clingon:getopt cmd :format))
         (*format* (parse-format format-val))
       (*no-color* (clingon:getopt* cmd :no-color))
         (store (ensure-store cmd))
         (issue-id (first (clingon:command-arguments cmd))))
    (unless issue-id
      (format *error-output* "Usage: bw comment list <issue-id>~%")
      (clingon:exit 1))
    (let ((comments (list-comments store issue-id)))
      (if (eq *format* :json)
          (format t "~A" (jzon:stringify
                          (coerce (mapcar #'comment->cli-json comments) 'vector)
                          :pretty t))
          (progn
            (format t "Comments for ~A (~D):~%" issue-id (length comments))
            (dolist (c comments)
              (format t "  [~A] ~A: ~A~%"
                      (format-timestamp (comment-created-at c))
                      (comment-author c)
                      (comment-body c))))))))

(defun comment-list/command ()
  (clingon:make-command
   :name "list"
   :description "List comments for an issue"
   :aliases '("ls")
   :options (global-options)
   :handler #'comment-list/handler
   :usage "<issue-id>"))

(defun comment-edit/handler (cmd)
  (let* ((store (ensure-store cmd))
         (args (clingon:command-arguments cmd))
         (comment-id-str (first args))
         (text (second args)))
    (unless (and comment-id-str text)
      (format *error-output* "Usage: bw comment edit <comment-id> <new-text>~%")
      (clingon:exit 1))
    (let ((comment-id (parse-integer comment-id-str :junk-allowed nil)))
      (unless comment-id
        (format *error-output* "Error: comment-id must be an integer (use 'bw comment list <issue-id>' to find IDs)~%")
        (clingon:exit 1))
      (handler-case
          (progn
            (edit-comment store comment-id text)
            (format t "Updated comment ~D~%" comment-id))
        (beadwork-error (c)
          (format *error-output* "Error: ~A~%" c)
          (clingon:exit 1))))))

(defun comment-edit/command ()
  (clingon:make-command
   :name "edit"
   :description "Edit a comment's text by ID"
   :options (global-options)
   :handler #'comment-edit/handler
   :usage "<comment-id> <new-text>"))

(defun comment-delete/handler (cmd)
  (let* ((store (ensure-store cmd))
         (args (clingon:command-arguments cmd))
         (comment-id-str (first args)))
    (unless comment-id-str
      (format *error-output* "Usage: bw comment rm <comment-id>~%")
      (clingon:exit 1))
    (let ((comment-id (parse-integer comment-id-str :junk-allowed nil)))
      (unless comment-id
        (format *error-output* "Error: comment-id must be an integer (use 'bw comment list <issue-id>' to find IDs)~%")
        (clingon:exit 1))
      (delete-comment store comment-id)
      (format t "Deleted comment ~D~%" comment-id))))

(defun comment-delete/command ()
  (clingon:make-command
   :name "rm"
   :description "Delete a comment by ID"
   :options (global-options)
   :handler #'comment-delete/handler
   :usage "<comment-id>"))

(defun comment/command ()
  (clingon:make-command
   :name "comment"
   :description "Manage issue comments"
   :sub-commands (list (comment-add/command)
                       (comment-list/command)
                       (comment-edit/command)
                       (comment-delete/command))))

;;; ---------------------------------------------------------------------------
;;; Command: blocked
;;; ---------------------------------------------------------------------------

(defun blocked/handler (cmd)
  (let* ((format-val (clingon:getopt cmd :format))
         (*format* (parse-format format-val))
       (*no-color* (clingon:getopt* cmd :no-color))
         (store (ensure-store cmd)))
    (let ((issues (blocked-issues store)))
      (unless (eq *format* :json)
        (format t "Blocked issues (~D):~%~%" (length issues)))
      (print-issues issues store))))

(defun blocked/command ()
  (clingon:make-command
   :name "blocked"
   :description "Show issues blocked by unclosed dependencies"
   :options (global-options)
   :handler #'blocked/handler))
