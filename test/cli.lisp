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

(define-test stats-json-is-an-object
  :parent cli-suite
  "bd-7b6: stats JSON must be an object with named numeric fields."
  (beadwork:with-store (store ":memory:" :prefix "bd")
    (beadwork:create-issue store :title "One" :type :task
                           :source-repo "cogen-meta")
    (let* ((json (com.inuoe.jzon:stringify
                  (beadwork::stats->json (beadwork::issue-stats store))))
           (ht (com.inuoe.jzon:parse json)))
      (true (hash-table-p ht))
      (is = 1 (gethash "total" ht))
      (true (hash-table-p (gethash "counts-by-status" ht)))
      (is = 1 (gethash "open" (gethash "counts-by-status" ht))))))

(define-test session-status-json-is-an-object
  :parent cli-suite
  "bd-7b6: session status JSON must be an object with named fields."
  (let* ((session (list :id "S-test"
                        :started-at (local-time:timestamp- (local-time:now)
                                                           10 :minute)
                        :active-issue-id "bd-abc"
                        :last-action "did a thing"
                        :agent-id "worker"))
         (ht (beadwork::session-status->json session 10)))
    (true (hash-table-p ht))
    (is equal "S-test" (gethash "session-id" ht))
    (is = 10 (gethash "duration-minutes" ht))
    (is equal "bd-abc" (gethash "active-issue-id" ht))))

(define-test cli-create-with-parent-json-reports-parent
  :parent cli-suite
  "bd-juc: the object create returns must name its parent, as show does."
  (let* ((db (bw-case-dir "create-parent"))
         (parent (create-bw-issue "Epic" :type "epic" :db db)))
    (multiple-value-bind (out err code)
        (run-bw (list "create" "-t" "Kid" "--parent" parent
                      "--repo" "beadwork" "--format" "json")
                :db db)
      (declare (ignore err))
      (is = 0 code)
      (is equal parent (gethash "parent" (bw-json out))))))

(define-test create-child-inherits-parent-repo
  :parent cli-suite
  "A child filed with --parent but no --repo stays on the parent's board."
  (beadwork:with-store (store ":memory:" :prefix "bd")
    (let* ((parent (beadwork:create-issue store :title "Epic" :type :epic
                                          :source-repo "render-stack-meta"))
           (repo (beadwork::resolve-create-repo store
                                                (beadwork:issue-id parent) nil)))
      (is equal "render-stack-meta" repo))))

(define-test create-explicit-repo-wins-over-parent
  :parent cli-suite
  (beadwork:with-store (store ":memory:" :prefix "bd")
    (let* ((parent (beadwork:create-issue store :title "Epic" :type :epic
                                          :source-repo "render-stack-meta"))
           (repo (beadwork::resolve-create-repo store
                                                (beadwork:issue-id parent)
                                                "render-parity")))
      (is equal "render-parity" repo))))

(define-test create-without-parent-or-repo-has-no-default
  :parent cli-suite
  "bd-ogg: a top-level create must not silently land in the \".\" bucket."
  (beadwork:with-store (store ":memory:" :prefix "bd")
    (false (beadwork::resolve-create-repo store nil nil))))

(define-test create-rejects-unknown-parent
  :parent cli-suite
  "bd-193: a bogus --parent must be a clean issue-not-found, not an FK error."
  (beadwork:with-store (store ":memory:" :prefix "bd")
    (handler-case
        (progn (beadwork::resolve-create-repo store "bd-nope" nil)
               (false t "expected an issue-not-found condition"))
      (beadwork:issue-not-found () (true t)))))

(define-test list-scope-line-names-the-filter
  :parent cli-suite
  "A scoped list must say so, or it looks like the whole board."
  (let ((scoped (beadwork::list-scope-line '("render-stack-meta") 231))
        (all (beadwork::list-scope-line nil 637)))
    (true (search "render-stack-meta" scoped))
    (true (search "--all-repos" scoped))
    (true (search "all repos" all))))

;;; ============================================================================
;;; End-to-end CLI tests -- the real bin/bw as a subprocess (bd-orw)
;;;
;;; These run the built binary, not the parent image.  Three things only a child
;;; process can show are the point: a real argv, a real exit code, and a clean
;;; image per case.  In-process calls were rejected because CLINGON:EXIT quits
;;; the process, and because the CLI keeps state in dynamic globals (*store*,
;;; *format*, *no-color*) that would leak from one case to the next.  cogen-cli
;;; reached the same conclusion (test/cli-integration-tests.lisp there).
;;;
;;; Isolation: every case gets its own --db directory under /tmp and runs from a
;;; scratch root with no .beads/ and no .git/, so a case can never read the
;;; project database or auto-scope by the checkout's repo name.
;;; ============================================================================

(defvar *bw-binary-path* nil
  "Resolved path to bin/bw, computed once per run.")

(defvar *bw-scratch-root* nil
  "Per-run scratch directory; the working directory of every child.")

(defvar *bw-case-counter* 0
  "Counter for unique per-case directories.")

(defun bw-source-newest-write-date ()
  "Write date of the newest file that goes into the binary."
  (let* ((root (asdf:system-source-directory :beadwork))
         (sources (append (directory (merge-pathnames "src/*.lisp" root))
                          (list (asdf:system-source-file
                                 (asdf:find-system :beadwork))))))
    (reduce #'max sources :key #'file-write-date)))

(defun bw-binary ()
  "Path to the bw executable under test.

Refuses a missing or stale binary.  Grading a build older than the sources is
the same class of false signal as a doctor that reports healthy without
checking anything.  `make test` builds first; a focused `make test-eval` does
not, so this check is what keeps that path honest."
  (or *bw-binary-path*
      (let ((bin (probe-file (merge-pathnames
                              "bin/bw"
                              (asdf:system-source-directory :beadwork)))))
        (unless bin
          (error "bin/bw not found. Run 'make build' (plain 'make test' does it first)."))
        (when (> (bw-source-newest-write-date) (file-write-date bin))
          (error "bin/bw is older than src/. Run 'make build' before the CLI tests."))
        (setf *bw-binary-path* (namestring bin)))))

(defun bw-scratch-root ()
  "Create (once per run) the directory every child runs in."
  (or *bw-scratch-root*
      (setf *bw-scratch-root*
            (ensure-directories-exist
             (merge-pathnames
              (make-pathname :directory (list :relative
                                              (format nil "bw-cli-~D-~D"
                                                      (get-universal-time)
                                                      (random 100000))))
              (uiop:temporary-directory))))))

(defun bw-case-dir (&optional (label "case"))
  "Return a fresh directory for one case, named after LABEL.

The trailing slash matters: the CLI merges \"beads.db\" and \"issues.jsonl\"
onto a directory pathname, so the namestring must already read as a directory."
  (incf *bw-case-counter*)
  (namestring
   (ensure-directories-exist
    (merge-pathnames
     (make-pathname :directory (list :relative
                                     (format nil "~A-~D" label *bw-case-counter*)))
     (bw-scratch-root)))))

(defun run-bw (args &key db input)
  "Run bin/bw with ARGS; return (values STDOUT STDERR EXIT-CODE).

DB, when given, is passed as --db and is the only database the command can
touch.  INPUT is fed to the child's stdin (the delete prompt reads it).

The argv is passed as a list, so UIOP execs the binary directly -- no shell,
no quoting, and an argument with a space stays one argument."
  (let* ((db (or db (bw-case-dir "db")))
         (argv (append (list (bw-binary)) args (list "--db" db))))
    (uiop:run-program argv
                      :output :string
                      :error-output :string
                      :input (make-string-input-stream (or input ""))
                      :directory (bw-scratch-root)
                      :ignore-error-status t)))

(defun bw-json (text)
  "Parse the CLI's JSON output (object -> hash-table, array -> vector)."
  (com.inuoe.jzon:parse text))

(defun json-ids (array)
  "The id of every object in a JSON array, in order."
  (map 'list (lambda (ht) (gethash "id" ht)) array))

(defun create-bw-issue (title &key (repo "beadwork") type priority db)
  "Create an issue through the CLI and return its id, asserting success."
  (multiple-value-bind (out err code)
      (run-bw (append (list "create" "-t" title "--repo" repo "--format" "json")
                      (when type (list "-T" type))
                      (when priority (list "-p" priority)))
              :db db)
    (declare (ignore err))
    (is = 0 code "create ~S should succeed" title)
    (gethash "id" (bw-json out))))

(defun write-status-doc (dir &rest lines)
  "Write a parseable status doc into DIR (a digit-prefixed name is required)."
  (let ((path (merge-pathnames "20260101T0000-doc.md" dir)))
    (with-open-file (out path :direction :output :if-exists :supersede)
      (dolist (line lines) (write-line line out)))
    (namestring path)))

;;; ---------------------------------------------------------------------------
;;; Top level: version, help, unknown command
;;; ---------------------------------------------------------------------------

(define-test cli-version-help-and-unknown-command
  :parent cli-suite
  (multiple-value-bind (out err code) (run-bw '("--version"))
    (declare (ignore err))
    (is = 0 code)
    (true (search "bw version" out)))
  (multiple-value-bind (out err code) (run-bw '("--help"))
    (declare (ignore err))
    (is = 0 code)
    (true (search "COMMANDS:" out))
    (true (search "doctor" out)))
  (multiple-value-bind (out err code) (run-bw '("definitely-not-a-command"))
    (declare (ignore out err))
    (is = 64 code "an unknown command is a usage error, not a silent success")))

;;; ---------------------------------------------------------------------------
;;; create / show
;;; ---------------------------------------------------------------------------

(define-test cli-create-then-show-roundtrips-every-field
  :parent cli-suite
  (let* ((db (bw-case-dir "roundtrip"))
         (id (create-bw-issue "Roundtrip issue" :db db :type "bug" :priority "P1")))
    (true id)
    (multiple-value-bind (out err code)
        (run-bw (list "show" id "--format" "json") :db db)
      (declare (ignore err))
      (is = 0 code)
      (let ((issue (bw-json out)))
        (is equal id (gethash "id" issue))
        (is equal "Roundtrip issue" (gethash "title" issue))
        (is equal "bug" (gethash "issue-type" issue))
        (is = 1 (gethash "priority" issue))
        (is equal "open" (gethash "status" issue))
        (is equal "beadwork" (gethash "source-repo" issue))))))

(define-test cli-create-without-repo-fails-with-a-usable-message
  :parent cli-suite
  (let ((db (bw-case-dir "norepo")))
    (multiple-value-bind (out err code)
        (run-bw '("create" "-t" "Homeless") :db db)
      (declare (ignore out))
      (is = 1 code)
      (true (search "--repo is required" err)))
    (multiple-value-bind (out err code)
        (run-bw '("list" "--format" "json" "--all-repos") :db db)
      (declare (ignore err))
      (is = 0 code)
      (is = 0 (length (bw-json out))))))

(define-test cli-create-child-gets-a-dotted-id-and-the-parent-link
  :parent cli-suite
  (let* ((db (bw-case-dir "child"))
         (parent (create-bw-issue "Epic" :db db :type "epic"))
         (child-id (format nil "~A.1" parent)))
    (multiple-value-bind (out err code)
        (run-bw (list "create" "-t" "Child" "--parent" parent "--format" "json")
                :db db)
      (declare (ignore err))
      (is = 0 code)
      (is equal child-id (gethash "id" (bw-json out))))
    (multiple-value-bind (out err code)
        (run-bw (list "show" child-id "--format" "json") :db db)
      (declare (ignore err))
      (is = 0 code)
      (let ((child (bw-json out)))
        (is equal parent (gethash "parent" child))
        (is equal "beadwork" (gethash "source-repo" child))))))

(define-test cli-show-missing-issue-exits-nonzero
  :parent cli-suite
  (let ((db (bw-case-dir "missing")))
    (multiple-value-bind (out err code)
        (run-bw '("show" "bd-nope" "--format" "json") :db db)
      (declare (ignore out))
      (is = 1 code)
      (true (search "not found" err)))))

;;; ---------------------------------------------------------------------------
;;; list / ready / blocked
;;; ---------------------------------------------------------------------------

(define-test cli-list-filters-by-repo-and-names-its-scope
  :parent cli-suite
  (let ((db (bw-case-dir "list")))
    (create-bw-issue "Alpha work" :db db :repo "alpha-repo")
    (create-bw-issue "Beta work" :db db :repo "beta-repo")
    (multiple-value-bind (out err code)
        (run-bw '("list" "--format" "json" "--all-repos") :db db)
      (declare (ignore err))
      (is = 0 code)
      (is = 2 (length (bw-json out))))
    (multiple-value-bind (out err code)
        (run-bw '("list" "--repo" "alpha-repo" "--format" "json") :db db)
      (declare (ignore err))
      (is = 0 code)
      (is = 1 (length (bw-json out))))
    (multiple-value-bind (out err code)
        (run-bw '("list" "--repo" "alpha-repo" "--plain") :db db)
      (declare (ignore err))
      (is = 0 code)
      (true (search "alpha-repo" out))
      (true (search "1 issue(s)" out))
      (true (search "--all-repos" out))
      (false (search "Beta work" out)))))

(define-test cli-ready-hides-issues-with-an-open-blocker
  :parent cli-suite
  (let* ((db (bw-case-dir "ready"))
         (blocker (create-bw-issue "Blocker" :db db))
         (dependent (create-bw-issue "Dependent" :db db)))
    (multiple-value-bind (out err code)
        (run-bw (list "dep" "add" dependent blocker) :db db)
      (declare (ignore out err))
      (is = 0 code))
    (multiple-value-bind (out err code)
        (run-bw '("ready" "--format" "json") :db db)
      (declare (ignore err))
      (is = 0 code)
      (is equal (list blocker) (json-ids (bw-json out))))
    (multiple-value-bind (out err code)
        (run-bw '("blocked" "--format" "json") :db db)
      (declare (ignore err))
      (is = 0 code)
      (is equal (list dependent) (json-ids (bw-json out))))))

;;; ---------------------------------------------------------------------------
;;; update / close / reopen
;;; ---------------------------------------------------------------------------

(define-test cli-update-changes-title-status-and-priority
  :parent cli-suite
  (let* ((db (bw-case-dir "update"))
         (id (create-bw-issue "Before" :db db)))
    (multiple-value-bind (out err code)
        (run-bw (list "update" id "-t" "After" "--status" "in_progress"
                      "--priority" "P0")
                :db db)
      (declare (ignore err))
      (is = 0 code)
      (true (search "Updated" out)))
    (multiple-value-bind (out err code)
        (run-bw (list "show" id "--format" "json") :db db)
      (declare (ignore err))
      (is = 0 code)
      (let ((issue (bw-json out)))
        (is equal "After" (gethash "title" issue))
        (is equal "in-progress" (gethash "status" issue))
        (is = 0 (gethash "priority" issue))))))

(define-test cli-close-needs-a-reason-and-reopen-undoes-it
  :parent cli-suite
  (let* ((db (bw-case-dir "close"))
         (id (create-bw-issue "Closable" :db db)))
    (multiple-value-bind (out err code)
        (run-bw (list "close" id) :db db)
      (declare (ignore out))
      (is = 1 code "closing without a reason must be refused")
      (true (search "close reason required" err)))
    (multiple-value-bind (out err code)
        (run-bw (list "close" id "-r" "finished it") :db db)
      (declare (ignore err))
      (is = 0 code)
      (true (search "Closed" out)))
    (multiple-value-bind (out err code)
        (run-bw (list "show" id "--format" "json") :db db)
      (declare (ignore err))
      (is = 0 code)
      (let ((issue (bw-json out)))
        (is equal "closed" (gethash "status" issue))
        (is equal "finished it" (gethash "close-reason" issue))
        (true (stringp (gethash "closed-at" issue)))))
    (multiple-value-bind (out err code)
        (run-bw (list "reopen" id) :db db)
      (declare (ignore err))
      (is = 0 code)
      (true (search "Reopened" out)))
    (multiple-value-bind (out err code)
        (run-bw (list "show" id "--format" "json") :db db)
      (declare (ignore err))
      (is = 0 code)
      (let ((issue (bw-json out)))
        (is equal "open" (gethash "status" issue))
        (false (gethash "closed-at" issue))))))

;;; ---------------------------------------------------------------------------
;;; dep / label / comment
;;; ---------------------------------------------------------------------------

(define-test cli-dep-add-list-and-remove
  :parent cli-suite
  (let* ((db (bw-case-dir "dep"))
         (blocker (create-bw-issue "Blocker" :db db))
         (dependent (create-bw-issue "Dependent" :db db)))
    (multiple-value-bind (out err code)
        (run-bw (list "dep" "add" dependent blocker) :db db)
      (declare (ignore err))
      (is = 0 code)
      (true (search "Added dependency" out)))
    (multiple-value-bind (out err code)
        (run-bw (list "dep" "list" dependent) :db db)
      (declare (ignore err))
      (is = 0 code)
      (true (search blocker out))
      (true (search "blocks" out)))
    (multiple-value-bind (out err code)
        (run-bw (list "dep" "remove" dependent blocker) :db db)
      (declare (ignore err))
      (is = 0 code)
      (true (search "Removed dependency" out)))
    (multiple-value-bind (out err code)
        (run-bw (list "dep" "list" dependent) :db db)
      (declare (ignore err))
      (is = 0 code)
      (false (search blocker out)))))

(define-test cli-label-add-list-and-remove
  :parent cli-suite
  (let* ((db (bw-case-dir "label"))
         (id (create-bw-issue "Labelled" :db db)))
    (multiple-value-bind (out err code)
        (run-bw (list "label" "add" id "needs-review") :db db)
      (declare (ignore err))
      (is = 0 code)
      (true (search "needs-review" out)))
    (multiple-value-bind (out err code)
        (run-bw (list "label" "list" id) :db db)
      (declare (ignore err))
      (is = 0 code)
      (true (search "needs-review" out)))
    (multiple-value-bind (out err code)
        (run-bw '("label" "list") :db db)
      (declare (ignore err))
      (is = 0 code)
      (true (search "needs-review" out)))
    (multiple-value-bind (out err code)
        (run-bw (list "label" "remove" id "needs-review") :db db)
      (declare (ignore err))
      (is = 0 code)
      (true (search "Removed label" out)))
    (multiple-value-bind (out err code)
        (run-bw (list "label" "list" id) :db db)
      (declare (ignore err))
      (is = 0 code)
      (false (search "needs-review" out)))))

(define-test cli-comment-add-and-list
  :parent cli-suite
  (let* ((db (bw-case-dir "comment"))
         (id (create-bw-issue "Commented" :db db)))
    (multiple-value-bind (out err code)
        (run-bw (list "comment" "add" id "first thought") :db db)
      (declare (ignore err))
      (is = 0 code)
      (true (search "Added comment" out)))
    ;; Text form.
    (multiple-value-bind (out err code)
        (run-bw (list "comment" "list" id) :db db)
      (declare (ignore err))
      (is = 0 code)
      (true (search "first thought" out))
      (true (search "[cli]" out)))
    ;; JSON form: an array of objects, not an array of pairs (bd-ibg).
    (multiple-value-bind (out err code)
        (run-bw (list "comment" "list" id "--format" "json") :db db)
      (declare (ignore err))
      (is = 0 code)
      (let* ((comments (bw-json out))
             (first (aref comments 0)))
        (is = 1 (length comments))
        (true (hash-table-p first))
        (is equal "[cli]" (gethash "author" first))
        (is equal "first thought" (gethash "body" first))))))

;;; ---------------------------------------------------------------------------
;;; search / stats / delete
;;; ---------------------------------------------------------------------------

(define-test cli-search-matches-title-substring
  :parent cli-suite
  (let ((db (bw-case-dir "search")))
    (let ((hit (create-bw-issue "Needle in a haystack" :db db)))
      (create-bw-issue "Unrelated issue" :db db)
      (multiple-value-bind (out err code)
          (run-bw '("search" "Needle" "--format" "json") :db db)
        (declare (ignore err))
        (is = 0 code)
        (is equal (list hit) (json-ids (bw-json out)))))))

(define-test cli-stats-json-counts
  :parent cli-suite
  (let ((db (bw-case-dir "stats")))
    (create-bw-issue "One" :db db :type "bug")
    (create-bw-issue "Two" :db db)
    (multiple-value-bind (out err code)
        (run-bw '("stats" "--format" "json") :db db)
      (declare (ignore err))
      (is = 0 code)
      (let* ((stats (bw-json out))
             (by-status (gethash "counts-by-status" stats))
             (by-type (gethash "counts-by-type" stats)))
        (is = 2 (gethash "total" stats))
        (is = 2 (gethash "open" by-status))
        (is = 1 (gethash "bug" by-type))
        (is = 1 (gethash "task" by-type))))))

(define-test cli-delete-cancels-without-force-and-deletes-with-it
  :parent cli-suite
  (let* ((db (bw-case-dir "delete"))
         (id (create-bw-issue "Deletable" :db db)))
    (multiple-value-bind (out err code)
        (run-bw (list "delete" id) :db db :input (format nil "n~%"))
      (declare (ignore err))
      (is = 0 code)
      (true (search "Cancelled" out)))
    (multiple-value-bind (out err code)
        (run-bw (list "show" id "--format" "json") :db db)
      (declare (ignore out err))
      (is = 0 code "a cancelled delete must leave the issue in place"))
    (multiple-value-bind (out err code)
        (run-bw (list "delete" id "-f") :db db)
      (declare (ignore err))
      (is = 0 code)
      (true (search "Deleted" out)))
    (multiple-value-bind (out err code)
        (run-bw (list "show" id "--format" "json") :db db)
      (declare (ignore out err))
      (is = 1 code))))

;;; ---------------------------------------------------------------------------
;;; init / sync
;;; ---------------------------------------------------------------------------

(define-test cli-init-creates-a-beads-directory
  :parent cli-suite
  ;; No trailing slash on --path: `bw init --path DIR` must still create
  ;; DIR/.beads/ rather than merging DIR into the parent's .beads/ (bd-5t0).
  (let* ((db (bw-case-dir "init"))
         (target (string-right-trim "/"
                                    (namestring
                                     (ensure-directories-exist
                                      (merge-pathnames "proj/" db))))))
    (multiple-value-bind (out err code)
        (run-bw (list "init" "--path" target) :db db)
      (declare (ignore err))
      (is = 0 code)
      (true (search "Initialized" out)))
    (true (probe-file (merge-pathnames "proj/.beads/beads.db" db)))
    (true (probe-file (merge-pathnames "proj/.beads/config.yaml" db)))
    (true (probe-file (merge-pathnames "proj/.beads/.gitignore" db)))
    (multiple-value-bind (out err code)
        (run-bw (list "init" "--path" target) :db db)
      (declare (ignore out))
      (is = 1 code)
      (true (search "already exists" err)))))

(define-test cli-sync-export-writes-issues-jsonl
  :parent cli-suite
  (let* ((db (bw-case-dir "sync"))
         (id (create-bw-issue "Exported issue" :db db)))
    (multiple-value-bind (out err code)
        (run-bw '("sync" "--direction" "export") :db db)
      (declare (ignore err))
      (is = 0 code)
      (true (search "Exported to" out)))
    (let ((jsonl (merge-pathnames "issues.jsonl" db)))
      (true (probe-file jsonl))
      (true (search id (uiop:read-file-string jsonl))))))

;;; ---------------------------------------------------------------------------
;;; session
;;; ---------------------------------------------------------------------------

(define-test cli-session-lifecycle
  :parent cli-suite
  (let* ((db (bw-case-dir "session"))
         (id (create-bw-issue "Session work" :db db)))
    (multiple-value-bind (out err code)
        (run-bw '("session" "status") :db db)
      (declare (ignore err))
      (is = 0 code)
      (true (search "No active session" out)))
    (multiple-value-bind (out err code)
        (run-bw '("session" "start" "--agent-id" "cli-test") :db db)
      (declare (ignore err))
      (is = 0 code)
      (true (search "started" out))
      (true (search "cli-test" out)))
    (multiple-value-bind (out err code)
        (run-bw (list "session" "work" id) :db db)
      (declare (ignore err))
      (is = 0 code)
      (true (search id out)))
    (multiple-value-bind (out err code)
        (run-bw '("session" "action" "investigating the thing") :db db)
      (declare (ignore err))
      (is = 0 code)
      (true (search "Action recorded" out)))
    (multiple-value-bind (out err code)
        (run-bw '("session" "status" "--format" "json") :db db)
      (declare (ignore err))
      (is = 0 code)
      (let ((status (bw-json out)))
        (is equal id (gethash "active-issue-id" status))
        (is equal "cli-test" (gethash "agent-id" status))
        (is equal "investigating the thing" (gethash "last-action" status))
        (true (stringp (gethash "session-id" status)))))
    (multiple-value-bind (out err code)
        (run-bw '("session" "end" "--notes" "handoff text") :db db)
      (declare (ignore err))
      (is = 0 code)
      (true (search "ended" out)))
    (multiple-value-bind (out err code)
        (run-bw '("session" "list" "--format" "json") :db db)
      (declare (ignore err))
      (is = 0 code)
      (let* ((sessions (bw-json out))
             (latest (aref sessions 0)))
        (is = 1 (length sessions))
        (is equal "ended" (gethash "state" latest))
        (is equal "handoff text" (gethash "handoff-notes" latest))))))

;;; ---------------------------------------------------------------------------
;;; batch / doctor
;;; ---------------------------------------------------------------------------

(define-test cli-batch-creates-a-parent-and-its-child
  :parent cli-suite
  (let* ((db (bw-case-dir "batch"))
         (payload (merge-pathnames "payload.json" db)))
    (with-open-file (out payload :direction :output :if-exists :supersede)
      (write-line "{\"operations\":[{\"op\":\"create\",\"ref\":\"epic\",\"title\":\"Batched epic\",\"type\":\"epic\",\"repo\":\"beadwork\",\"children\":[{\"op\":\"create\",\"ref\":\"kid\",\"title\":\"Batched kid\",\"type\":\"task\",\"repo\":\"beadwork\"}]}]}"
                  out))
    (multiple-value-bind (out err code)
        (run-bw (list "batch" "--file" (namestring payload)) :db db)
      (declare (ignore err))
      (is = 0 code)
      (is eq t (gethash "ok" (bw-json out))))
    (multiple-value-bind (out err code)
        (run-bw '("list" "--format" "json" "--all-repos") :db db)
      (declare (ignore err))
      (is = 0 code)
      (let* ((issues (bw-json out))
             (find-by-title (lambda (title)
                              (find title issues
                                    :test #'equal
                                    :key (lambda (i) (gethash "title" i))))))
        (is = 2 (length issues))
        (let ((epic (funcall find-by-title "Batched epic"))
              (kid (funcall find-by-title "Batched kid")))
          (true epic)
          (true kid)
          (is equal (gethash "id" epic) (gethash "parent" kid)))))))

(define-test cli-doctor-unscoped-exit-code-tracks-the-dot-bucket
  :parent cli-suite
  (let ((db (bw-case-dir "doctor")))
    (create-bw-issue "Scoped issue" :db db)
    (multiple-value-bind (out err code)
        (run-bw '("doctor" "unscoped" "--format" "json") :db db)
      (declare (ignore err))
      (is = 0 code)
      (is = 0 (gethash "count" (bw-json out))))
    (create-bw-issue "Unscoped issue" :db db :repo ".")
    (multiple-value-bind (out err code)
        (run-bw '("doctor" "unscoped" "--format" "json") :db db)
      (declare (ignore err))
      (is = 1 code "a non-empty . bucket is a failure for this subcommand")
      (is = 1 (gethash "count" (bw-json out))))))

(define-test cli-broken-pipe-does-not-hang
  :parent cli-suite
  "A reader that exits early must not leave bw spinning.

`bw list --format json | head -3` used to spin at 100% CPU forever: when the
reader closes mid-buffer, SBCL's fd-stream output path retries without
progress, so no condition reaches Lisp and no handler can run.  The binary
restores the default SIGPIPE action, so it dies like a Unix filter (141).

The payload must exceed one pipe buffer (64 KiB), otherwise the whole write
lands in the pipe and there is nothing to fail."
  (let* ((db (bw-case-dir "sigpipe"))
         (script (format nil "~A list --format json --all-repos --db ~A 2>/dev/null | head -3 >/dev/null; echo ${PIPESTATUS[0]}"
                          (bw-binary) db)))
    ;; Seed in-process: one issue with a 200 KiB description is enough to push
    ;; the JSON well past one pipe buffer, and it costs milliseconds.
    (let ((store (beadwork::open-store
                  (namestring (merge-pathnames "beads.db" db)))))
      (beadwork:create-issue store
                             :title "Broken pipe filler"
                             :type :task
                             :source-repo "beadwork"
                             :description (make-string 200000 :initial-element #\x))
      (beadwork::close-store store))
    (multiple-value-bind (out err code)
        (uiop:run-program (list "timeout" "-s" "KILL" "20" "bash" "-c" script)
                          :output :string
                          :error-output :string
                          :directory (bw-scratch-root)
                          :ignore-error-status t)
      (declare (ignore err))
      (is = 0 code "the pipeline itself must finish, not time out")
      (is equal "141" (string-trim '(#\Newline #\Space) out)
          "bw must die on SIGPIPE instead of spinning"))))

(define-test cli-doctor-classifies-a-closed-dotted-child
  :parent cli-suite
  "bd-r8v end-to-end: the closed child is STALE, not hidden by its open epic."
  (let* ((db (bw-case-dir "doctor-dotted"))
         (epic (create-bw-issue "Open epic" :db db :type "epic"))
         (child (format nil "~A.1" epic)))
    (multiple-value-bind (out err code)
        (run-bw (list "create" "-t" "Child" "--parent" epic) :db db)
      (declare (ignore out err))
      (is = 0 code))
    (multiple-value-bind (out err code)
        (run-bw (list "close" child "-r" "child done") :db db)
      (declare (ignore out err))
      (is = 0 code))
    (write-status-doc db
                      "<!-- bw:whats-next -->"
                      "## What's Next"
                      ""
                      (format nil "1. Continue the plan (~A)" child))
    (multiple-value-bind (out err code)
        (run-bw (list "doctor" "--status-dir" db) :db db)
      (declare (ignore err))
      (is = 1 code "a closed child behind an open epic is not healthy")
      (true (search "STALE" out))
      (true (search child out)))
    (multiple-value-bind (out err code)
        (run-bw (list "doctor" "--status-dir" db "--format" "json") :db db)
      (declare (ignore err))
      (is = 1 code)
      (let* ((result (bw-json out))
             (finding (aref (gethash "findings" result) 0)))
        (is equal "STALE" (gethash "status" finding))
        (is equal child (gethash "id" finding))))))
