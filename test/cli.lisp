(in-package #:beadwork/tests)

;;; CLI tests — --db flag redirection regression (bd-otg)
;;;
;;; Before the fix, --db was parsed but never consumed: resolve-store
;;; always walked up from cwd for .beads/.  These tests pin the new
;;; behavior: the --db value reaches the store location.

(define-test cli-suite
  :parent beadwork-suite
  :description "Tests for bw CLI helpers (--db flag redirection)")

(define-test db-flag-resolves-to-explicit-dir
  :parent cli-suite
  "resolve-db-path with a --db dir returns <dir>/beads.db, creates the
dir, and a store opened there writes the database into that dir.
Uses a NO-trailing-slash input, matching what the real CLI delivers
(regression: merge-pathnames treated '.beads' as a filename)."
  (let* ((base (string-right-trim "/" (namestring (uiop:temporary-directory))))
         (dir-str (format nil "~A/bw-cli-test-~A/.beads"
                          base (beadwork:generate-id "t" :prefix "t")))
         (dir (uiop:ensure-directory-pathname dir-str))
         (path (beadwork::resolve-db-path dir-str)))
    (unwind-protect
         (progn
           (true (probe-file dir))
           (is equal (namestring (merge-pathnames "beads.db" dir)) path)
           (let ((store (beadwork::open-store path)))
             (unwind-protect
                  (progn
                    (beadwork:create-issue store :title "db-flag regression test")
                    (true (probe-file (merge-pathnames "beads.db" dir))))
               (beadwork::close-store store))))
      ;; Clean up scratch dir (test artifact under /tmp only)
      (handler-case
          (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore)
        (error () nil)))))

(define-test db-flag-before-subcommand-reaches-handler
  :parent cli-suite
  "Parsing 'bw --db DIR create ...' makes DIR visible via GETOPT* on the
subcommand — the exact reproduce from bd-otg"
  (let* ((app (beadwork::top-level/command))
         (parsed (clingon:parse-command-line
                  app '("--db" "/tmp/bw-conc/.beads" "create" "-t" "x" "-d" "y"))))
    (is equal "/tmp/bw-conc/.beads" (clingon:getopt* parsed :db-path))))

(define-test db-flag-after-subcommand-reaches-handler
  :parent cli-suite
  "Parsing 'bw create --db DIR ...' also makes DIR visible via GETOPT*"
  (let* ((app (beadwork::top-level/command))
         (parsed (clingon:parse-command-line
                  app '("create" "-t" "x" "--db" "/tmp/alt/.beads"))))
    (is equal "/tmp/alt/.beads" (clingon:getopt* parsed :db-path))))
