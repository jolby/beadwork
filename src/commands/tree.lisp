(in-package :beadwork)

;;; ============================================================================
;;; Command tree
;;; ============================================================================
;;;
;;; This file is loaded last: it constructs every command defined in the
;;; sibling modules, so it must compile after them.  It is also loaded after
;;; CLI.LISP, which declares the special variables the handlers bind.

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
                       (blocked/command)
                       (delete/command)
                       (dep/command)
                       (search/command)
                       (stats/command)
                       (label/command)
                       (comment/command)
                       (session/command)
                       (doctor/command)
                       (init/command)
                       (sync/command)
                       (batch/command))))

(defun main ()
  "Entry point for the bw executable.

CL-CLI-RUNTIME takes over the process-level behaviour every dumped binary
needs: death by SIGPIPE when a reader goes away (instead of SBCL retrying a
mid-buffer broken pipe below Lisp, forever -- bd-sso), and an error path that
reports to stderr and exits 1 instead of parking in a debugger reading a dead
stdin.  See ~/work/cl-cli-runtime."
  (cl-cli-runtime:run (lambda ()
                        (clingon:run (top-level/command)))))
