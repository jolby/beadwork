(in-package #:beadwork/tests)

;;; CLI tests -- --db flag redirection regression (bd-otg)
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
subcommand -- the exact reproduce from bd-otg"
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
(define-test ready-table-aligns-long-ids
  :parent cli-suite
  "Regression (bw ready alignment): the rich table must keep all
columns aligned even when issue IDs exceed 12 characters.  The ID
column width is computed from the longest ID in the result set, and
every row (header, underline, and data) must use that same width --
rows must not fall back to a hardcoded 12."
  (let* ((issues (list
                  (make-instance 'beadwork:issue
                                 :id "bd-short.1"
                                 :title "Short id row"
                                 :issue-type :task
                                 :source-repo "repo")
                  (make-instance 'beadwork:issue
                                 :id "bd-1kj7.12.1.10"
                                 :title "Long id row"
                                 :issue-type :bug
                                 :source-repo "repo")))
         (out (make-string-output-stream)))
    (unwind-protect
         (let ((beadwork::*format* :rich)
               (beadwork::*no-color* t)
               (*standard-output* out))
           (beadwork::print-issues issues))
      (close out))
    (let* ((text (get-output-stream-string out))
           (lines (remove "" (uiop:split-string text :separator '(#\Newline))
                          :test #'string=))
           (data-rows (remove-if (lambda (line)
                                   (or (uiop:string-prefix-p "Ready work" line)
                                       (uiop:string-prefix-p "ID" line)
                                       (every (lambda (ch) (member ch '(#\- #\Space)))
                                              line)))
                                 lines))
           (status-starts (mapcar (lambda (row)
                                    (search "OPEN" row))
                                  data-rows)))
      (is equal 2 (length data-rows))
      (true (every #'identity status-starts) "every data row has a STATUS column")
      (true (every (lambda (s) (= (first status-starts) s))
                   (rest status-starts))
            "all rows start STATUS at the same column"))))

;;; ---------------------------------------------------------------------------
;;; Graph neighbors in show/list output (bd-uz3)
;;; ---------------------------------------------------------------------------

(define-test show-json-includes-parent-children-and-dependencies
  :parent cli-suite
  "bd-uz3: the CLI JSON view of an issue exposes its graph neighbors."
  (beadwork:with-store (store ":memory:" :prefix "bd")
    (let* ((epic (beadwork:create-issue store :title "Epic" :type :epic))
           (child (beadwork:create-issue store :title "Child" :type :task
                                         :parent (beadwork:issue-id epic)))
           (blocker (beadwork:create-issue store :title "Blocker" :type :bug))
           (dependent (beadwork:create-issue store :title "Dependent" :type :task)))
      (beadwork:add-dependency store (beadwork:issue-id child)
                               (beadwork:issue-id blocker) :type :blocks)
      (beadwork:add-dependency store (beadwork:issue-id dependent)
                               (beadwork:issue-id child) :type :blocks)
      (let ((child-json (beadwork::issue->cli-json
                         (beadwork:get-issue store (beadwork:issue-id child)) store))
            (epic-json (beadwork::issue->cli-json
                        (beadwork:get-issue store (beadwork:issue-id epic)) store)))
        ;; Full issue fields are still present
        (is equal (beadwork:issue-id child) (gethash "id" child-json))
        (is equal "Child" (gethash "title" child-json))
        ;; Parent
        (is equal (beadwork:issue-id epic) (gethash "parent" child-json))
        (is eq 'null (gethash "parent" epic-json))
        ;; Children
        (is equal 1 (length (gethash "children" epic-json)))
        (is equal (beadwork:issue-id child)
                 (gethash "id" (aref (gethash "children" epic-json) 0)))
        ;; Outgoing dependency
        (is equal 1 (length (gethash "dependencies" child-json)))
        (is equal (beadwork:issue-id blocker)
                 (gethash "id" (aref (gethash "dependencies" child-json) 0)))
        (is equal "blocks"
                 (gethash "relation" (aref (gethash "dependencies" child-json) 0)))
        ;; Incoming dependency
        (is equal 1 (length (gethash "dependents" child-json)))
        (is equal (beadwork:issue-id dependent)
                 (gethash "id" (aref (gethash "dependents" child-json) 0)))))))

(define-test show-rich-lists-graph-neighbors
  :parent cli-suite
  "bd-uz3: rich show output lists children and their ids."
  (beadwork:with-store (store ":memory:" :prefix "bd")
    (let* ((epic (beadwork:create-issue store :title "Epic" :type :epic))
           (child (beadwork:create-issue store :title "Child" :type :task
                                         :parent (beadwork:issue-id epic))))
      (let ((out (make-string-output-stream)))
        (unwind-protect
             (let ((beadwork::*format* :rich)
                   (beadwork::*no-color* t)
                   (*standard-output* out))
               (beadwork::print-issue-single
                (beadwork:get-issue store (beadwork:issue-id epic)) store))
          (close out))
        (let ((text (get-output-stream-string out)))
          (true (search "Children:" text))
          (true (search (beadwork:issue-id child) text)))))))

(define-test list-json-empty-is-array
  :parent cli-suite
  "An empty issue list must serialize as [] rather than JSON false, or
agents parsing `bw list --format json` break when the result set is empty."
  (let ((out (make-string-output-stream)))
    (unwind-protect
         (let ((beadwork::*format* :json)
               (*standard-output* out))
           (beadwork::print-issues nil))
      (close out))
    (is equal "[]"
            (string-trim '(#\Newline #\Space)
                         (get-output-stream-string out)))))
