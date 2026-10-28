(in-package #:beadwork/tests)

(define-test doctor-suite
  :parent beadwork-suite
  :description "Tests for bw doctor")

(define-test parses-whats-next-table
  :parent doctor-suite
  (let* ((lines '("## What's Next"
                 ""
                 "| Priority | Task | Beadwork ID |"
                 "|----------|------|-------------|"
                 "| P1 | Fix the bug | bd-abc |"
                 "| P2 | Do the thing | — |"
                 "| — | Brainstorm: messaging | note `f9efe052` |"
                 ""))
         (rows (beadwork::parse-whats-next-table lines)))
    (is = 3 (length rows))
    ;; Row 1: full entry
    (is equal "P1" (first (first rows)))
    (is equal "Fix the bug" (second (first rows)))
    (is equal "bd-abc" (third (first rows)))
    ;; Row 2: dash for no ID
    (is equal "P2" (first (second rows)))
    (is equal "Do the thing" (second (second rows)))
    (is equal "—" (third (second rows)))
    ;; Row 3: note reference instead of bw ID
    (is equal "—" (first (third rows)))
    (is equal "Brainstorm: messaging" (second (third rows)))
    (is equal "note `f9efe052`" (third (third rows)))))

(define-test parse-whats-next-table-no-heading
  :parent doctor-suite
  (let* ((lines '("## Summary"
                 "Some content here"
                 ""))
         (rows (beadwork::parse-whats-next-table lines)))
    (is = 0 (length rows))))

(define-test parses-whats-next-numbered-list
  :parent doctor-suite
  (let* ((lines '("## What's Next"
                 ""
                 "1. Signal handlers: SIGWINCH (bd-szn.5)"
                 "2. Dogfood the TUI (bd-szn.6)"
                 "3. Editor cursor polish"
                 ""))
         (rows (beadwork::parse-whats-next-table lines)))
    (is = 3 (length rows))
    ;; Row 1: has embedded bw ID
    (is equal "—" (first (first rows)))
    (is equal "Signal handlers: SIGWINCH (bd-szn.5)" (second (first rows)))
    (is equal "bd-szn.5" (third (first rows)))
    ;; Row 2: has embedded bw ID with parens
    (is equal "—" (first (second rows)))
    (is equal "Dogfood the TUI (bd-szn.6)" (second (second rows)))
    (is equal "bd-szn.6" (third (second rows)))
    ;; Row 3: no ID
    (is equal "—" (first (third rows)))
    (is equal "Editor cursor polish" (second (third rows)))
    (is equal "" (third (third rows)))))

(define-test parses-whats-next-org-mode-heading
  :parent doctor-suite
  (let* ((lines '("* What's Next"
                 ""
                 "1. Do the thing"
                 "2. Fix the other thing (bd-abc)"
                 ""))
         (rows (beadwork::parse-whats-next-table lines)))
    (is = 2 (length rows))
    (is equal "Do the thing" (second (first rows)))
    (is equal "Fix the other thing (bd-abc)" (second (second rows)))
    (is equal "bd-abc" (third (second rows)))))

(define-test parse-whats-next-table-empty
  :parent doctor-suite
  (let* ((lines '("## What's Next"
                 ""
                 "| Priority | Task | Beadwork ID |"
                 "|----------|------|-------------|"
                 ""
                 "## Known Issues"))
         (rows (beadwork::parse-whats-next-table lines)))
    (is = 0 (length rows))))

(define-test classifies-rows-against-db
  :parent doctor-suite
  ;; Create a temp in-memory store with known issues
  (let* ((store (beadwork::open-store ":memory:"))
         (issue (beadwork::create-issue store
                  :title "Test issue"
                  :type :task
                  :priority 1)))
    (let ((issue-id (beadwork::issue-id issue)))
      ;; Row with existing open issue → OK
      (let ((result (beadwork::classify-row
                     (list "P1" "Test issue" issue-id) store)))
        (is equal :ok (getf result :status))
        (is equal issue-id (getf result :id)))
      ;; Close it, then row should be STALE
      (beadwork::close-issue store issue-id :reason "done")
      (let ((result (beadwork::classify-row
                     (list "P1" "Test issue" issue-id) store)))
        (is equal :stale (getf result :status))
        (is equal issue-id (getf result :id)))
      ;; Row with non-existent ID → MISSING
      (let ((result (beadwork::classify-row
                     (list "P1" "Ghost" "bd-zzz") store)))
        (is equal :missing (getf result :status))
        (is equal "bd-zzz" (getf result :id)))
      ;; Row with no valid bw ID → ORPHAN (gets a generated pseudo-ID)
      (let ((result (beadwork::classify-row
                     (list "P1" "No ID" "—") store
                     :doc-path #P"test-status.md")))
        (is equal :orphan (getf result :status))
        (true (getf result :id))
        (true (ppcre:scan "^orph-" (getf result :id)))
        (is equal "test-status.md" (getf result :source))))
    (beadwork::close-store store)))

(define-test extracts-bw-ids
  :parent doctor-suite
  ;; Exact bw IDs
  (is equal "bd-0lg" (beadwork::extract-bw-id "bd-0lg"))
  (is equal "bd-abc" (beadwork::extract-bw-id "bd-abc"))
  (is equal "bd-iq1" (beadwork::extract-bw-id "bd-iq1"))
  ;; Non-bw-ID values return nil
  (is equal nil (beadwork::extract-bw-id "—"))
  (is equal nil (beadwork::extract-bw-id "note `f9efe052`"))
  (is equal nil (beadwork::extract-bw-id ""))
  (is equal nil (beadwork::extract-bw-id "N/A"))
  ;; Embedded IDs in text now extracted
  (is equal "bd-abc" (beadwork::extract-bw-id "see bd-abc for details"))
  (is equal "bd-szn.5" (beadwork::extract-bw-id "Signal handlers: SIGWINCH (bd-szn.5)"))
  (is equal "bd-ayh" (beadwork::extract-bw-id "Fix the bd-ayh issue")))

(define-test extracts-dotted-bw-ids
  :parent doctor-suite
  "bd-r8v: a dotted id is extracted whole, not cut at the first dot."
  (is equal "bd-sa3.11.5" (beadwork::extract-bw-id "bd-sa3.11.5"))
  (is equal "bd-sa3.11.5" (beadwork::extract-bw-id "Landed (bd-sa3.11.5)"))
  (is equal "bd-1kj7.20.7" (beadwork::extract-bw-id "1. Textual MVP (bd-1kj7.20.7)"))
  (is equal "bd-sa3.11" (beadwork::extract-bw-id "then (bd-sa3.11); and more"))
  ;; Trailing sentence punctuation is not part of the id.
  (is equal "bd-sa3" (beadwork::extract-bw-id "see bd-sa3."))
  (is equal "bd-sa3" (beadwork::extract-bw-id "bd-sa3, then bd-abc")))

(define-test finds-latest-status-doc
  :parent doctor-suite
  (let ((status-dir (merge-pathnames
                     "resources/project/status/"
                     (uiop:getcwd))))
    (when (probe-file status-dir)
      (let ((latest (beadwork::find-latest-status-doc status-dir)))
        ;; Should return a pathname
        (true latest)
        ;; Should end with .md
        (true (ppcre:scan "\\.md$" (namestring latest)))))))

(define-test resolves-status-docs-dir
  :parent doctor-suite
  ;; When run from cogen-meta root, it should find the status dir
  (let ((dir (beadwork::find-status-docs-dir)))
    (when dir
      (true (probe-file dir))
      (true (ppcre:scan "status/?$" (namestring dir))))))

(define-test runs-status-docs-check
  :parent doctor-suite
  (let ((status-dir (beadwork::find-status-docs-dir)))
    (when status-dir
      (let* ((store (beadwork::open-store ":memory:"))
             (result (beadwork::run-doctor-status-docs store status-dir)))
        (beadwork::close-store store)
        ;; Should return a plist
        (true (getf result :status-doc))
        (is eq t (getf result :checked)
            "the latest status doc must be parseable")
        (true (getf result :summary))
        (true (listp (getf result :findings)))))))

(define-test reports-doctor-findings-human
  :parent doctor-suite
  (let* ((findings (list (list :status :ok :id "bd-abc" :task "Fix bug" :issue-status :open)
                         (list :status :stale :id "bd-xyz" :task "Done thing" :issue-status :closed)
                         (list :status :missing :id "bd-zzz" :task "Ghost" :issue-status nil)
                         (list :status :orphan :id "orph-1A2B3C4" :task "No ID task" :source "test.md" :issue-status nil)))
         (summary (list :ok 1 :stale 1 :missing 1 :orphan 1))
         (result (list :status-doc #P"test.md"
                       :findings findings
                       :summary summary))
         (exit-code (beadwork::report-doctor-findings result :human)))
    (is = 1 exit-code)))

(define-test reports-doctor-findings-json
  :parent doctor-suite
  (let* ((findings (list (list :status :ok :id "bd-abc" :task "Fix bug" :issue-status :open)))
         (summary (list :ok 1 :stale 0 :missing 0 :orphan 0))
         (result (list :status-doc #P"test.md"
                       :findings findings
                       :summary summary))
         (json-output (with-output-to-string (s)
                        (let ((*standard-output* s))
                          (beadwork::report-doctor-findings result :json)))))
    (true (ppcre:scan "\"status-doc\"" json-output))
    (true (ppcre:scan "\"healthy\"" json-output))))

;;; ============================================================================
;;; What's Next parsing: prose, section end, found-p (bd-fs9, bd-sa3.11.5)
;;; ============================================================================

(define-test parse-whats-next-prose-with-ids
  :parent doctor-suite
  "bd-fs9: a prose paragraph after the marker still names issues; parse the ids."
  (let* ((lines '("<!-- bw:whats-next -->"
                  "## What's Next"
                  ""
                  "Start bd-sa3.2 (P1): split cogen-ai into two systems."
                  "Then re-run the corpus (bd-qsj)."
                  "Nothing to file here."
                  ""))
         (rows (beadwork::parse-whats-next-table lines)))
    (is = 2 (length rows))
    (is equal "bd-sa3.2" (third (first rows)))
    (is equal "bd-qsj" (third (second rows)))
    (is equal "Start bd-sa3.2 (P1): split cogen-ai into two systems."
              (second (first rows)))))

(define-test parse-whats-next-stops-at-next-heading
  :parent doctor-suite
  "bd-sa3.11.5: content after the next heading is not part of What's Next."
  (let* ((lines '("<!-- bw:whats-next -->"
                  "## What's Next"
                  ""
                  "1. Real next step (bd-abc)"
                  ""
                  "## Postscript"
                  ""
                  "1. This is prose, not a task"
                  "2. Neither is this"
                  ""))
         (rows (beadwork::parse-whats-next-table lines)))
    (is = 1 (length rows))
    (is equal "bd-abc" (third (first rows)))))

(define-test parse-whats-next-ignores-a-quoted-marker
  :parent doctor-suite
  "bd-ifb: a marker quoted inside prose does not arm the parser.

The reproduction: a status doc's checklist bullet mentions the marker while
describing the convention, and its own What's Next section sits at the end. The
parser used to arm on the quotation, read the next bullet as a task, and return
at the next heading — so the real section was never read and `bw doctor`
reported an unrelated checklist line as an orphan."
  (let* ((lines '("## Checklist"
                  ""
                  "- Write a status doc ending in a What's Next section marked"
                  "  with <!-- bw:whats-next --> so doctor can read it."
                  "- Close the issue before the doc."
                  ""
                  "## What's Next"
                  ""
                  "<!-- bw:whats-next -->"
                  ""
                  "1. Real next step (bd-abc)"
                  ""))
         (rows (beadwork::parse-whats-next-table lines)))
    (is = 1 (length rows) "only the real section's row")
    (is equal "bd-abc" (third (first rows)))))

(define-test parse-whats-next-reports-found-p
  :parent doctor-suite
  "FOUND-P distinguishes 'no section' from 'section with nothing parseable'."
  (multiple-value-bind (rows found)
      (beadwork::parse-whats-next-table '("## Summary" "text"))
    (is = 0 (length rows))
    (false found))
  (multiple-value-bind (rows found)
      (beadwork::parse-whats-next-table '("<!-- bw:whats-next -->"))
    (is = 0 (length rows))
    (true found)))

(define-test doctor-json-says-not-checked
  :parent doctor-suite
  "bd-fs9: an unchecked run must not claim healthy, and empty findings is []."
  (let* ((result (list :status-doc #P"doc.md"
                       :checked nil
                       :findings nil
                       :summary (list :ok 0 :stale 0 :missing 0 :orphan 0)
                       :info "No What's Next section found in doc.md."))
         (output (with-output-to-string (s)
                   (let ((*standard-output* s))
                     (beadwork::report-doctor-findings result :json))))
         (parsed (com.inuoe.jzon:parse output)))
    (true (hash-table-p parsed))
    (false (gethash "checked" parsed))
    (is eq 'null (gethash "healthy" parsed))
    (true (vectorp (gethash "findings" parsed)))
    (is = 0 (length (gethash "findings" parsed)))
    (is equal "No What's Next section found in doc.md." (gethash "info" parsed))))

(define-test doctor-json-says-checked-and-clean
  :parent doctor-suite
  (let* ((findings (list (list :status :ok :id "bd-abc" :task "Fix bug"
                              :issue-status :open)))
         (result (list :status-doc #P"doc.md"
                       :checked t
                       :findings findings
                       :summary (list :ok 1 :stale 0 :missing 0 :orphan 0)))
         (output (with-output-to-string (s)
                   (let ((*standard-output* s))
                     (beadwork::report-doctor-findings result :json))))
         (parsed (com.inuoe.jzon:parse output)))
    (is eq t (gethash "checked" parsed))
    (is eq t (gethash "healthy" parsed))
    (is = 1 (length (gethash "findings" parsed)))
    (is equal 1 (gethash "ok" (gethash "summary" parsed)))))

(define-test doctor-resolves-prose-whats-next
  :parent doctor-suite
  "End-to-end: a status doc whose What's Next is prose is actually checked."
  ;; The doc lives in a private directory. FIND-LATEST-STATUS-DOC takes the
  ;; highest-sorting *.md whose name starts with a digit, so one stray file in
  ;; the shared temporary directory — a leftover `7m5-close.md` outranks every
  ;; `2026...` name — silently decided which document this test read, and it
  ;; failed with no change to the code.
  (let ((dir (merge-pathnames "bw-doctor-prose-doc/" (uiop:temporary-directory)))
        (doc-name "20260101T0000-prose.md"))
    (ensure-directories-exist (merge-pathnames doc-name dir))
    (unwind-protect
         (progn
           (with-open-file (out (merge-pathnames doc-name dir)
                                :direction :output :if-exists :supersede)
             (format out "<!-- bw:whats-next -->~%## What's Next~%")
             (format out "~%Ship bd-abc next.~%"))
           (let ((store (beadwork::open-store ":memory:")))
             (unwind-protect
                  (let ((result (beadwork::run-doctor-status-docs store dir)))
                    (true (getf result :checked))
                    (is = 1 (length (getf result :findings)))
                    (is eq :missing (getf (first (getf result :findings)) :status)))
               (beadwork::close-store store))))
      (uiop:delete-directory-tree dir :validate t))))

;;; ============================================================================
;;; Dotted ids: exact match, ancestor fallback, missing (bd-r8v)
;;; ============================================================================

(define-test doctor-classifies-exact-dotted-child
  :parent doctor-suite
  "bd-r8v: a closed child is STALE even though its open epic exists."
  (beadwork:with-store (store ":memory:" :prefix "bd")
    (let* ((epic (beadwork:create-issue store :title "Epic" :type :epic))
           (epic-id (beadwork:issue-id epic))
           (child (beadwork:create-issue store :title "Child" :type :task
                                         :parent epic-id))
           (child-id (beadwork:issue-id child)))
      (beadwork:close-issue store child-id :reason "done")
      ;; The epic is still open, so an ancestor-only lookup would say OK.
      (let ((result (beadwork::classify-row
                     (list "P1" "Continue the tail plan" child-id) store)))
        (is eq :stale (getf result :status))
        (is equal child-id (getf result :id))
        (is equal child-id (getf result :requested-id)))
      ;; A still-open child of the same epic classifies OK.
      (let* ((other (beadwork:create-issue store :title "Other" :type :task
                                           :parent epic-id))
             (result (beadwork::classify-row
                      (list "P1" "Keep going" (beadwork:issue-id other)) store)))
        (is eq :ok (getf result :status))
        (is equal (beadwork:issue-id other) (getf result :id))))))

(define-test doctor-falls-back-to-nearest-ancestor
  :parent doctor-suite
  "bd-r8v: an unknown dotted id classifies against the nearest existing ancestor."
  (beadwork:with-store (store ":memory:" :prefix "bd")
    (let* ((epic (beadwork:create-issue store :title "Epic" :type :epic))
           (epic-id (beadwork:issue-id epic))
           (child (beadwork:create-issue store :title "Child" :type :task
                                         :parent epic-id))
           (child-id (beadwork:issue-id child)))
      (dolist (requested (list (format nil "~A.9" child-id)
                               (format nil "~A.9.2" child-id)))
        (let ((result (beadwork::classify-row
                       (list "P1" "Ghost child" requested) store)))
          (is eq :ok (getf result :status))
          (is equal child-id (getf result :id))
          (is equal requested (getf result :requested-id))
          ;; The human line must say which ancestor was used.
          (let ((line (beadwork::format-finding-line result)))
            (true (search requested line))
            (true (search child-id line))))))))

(define-test doctor-missing-dotted-id-is-missing
  :parent doctor-suite
  "bd-r8v: a dotted id with no existing ancestor is MISSING, not OK."
  (beadwork:with-store (store ":memory:" :prefix "bd")
    (let ((result (beadwork::classify-row
                   (list "P1" "Nothing here" "bd-zzz.9.9") store)))
      (is eq :missing (getf result :status))
      (is equal "bd-zzz.9.9" (getf result :id))
      (is equal "bd-zzz.9.9" (getf result :requested-id)))))

(define-test doctor-json-reports-requested-id
  :parent doctor-suite
  "bd-r8v: JSON carries the requested id and the one actually classified."
  (let* ((findings (list (list :status :ok :id "bd-abc.1"
                               :requested-id "bd-abc.1.9"
                               :task "Ghost child" :issue-status :open)))
         (result (list :status-doc #P"doc.md"
                       :checked t
                       :findings findings
                       :summary (list :ok 1 :stale 0 :missing 0 :orphan 0)))
         (output (with-output-to-string (s)
                   (let ((*standard-output* s))
                     (beadwork::report-doctor-findings result :json))))
         (parsed (com.inuoe.jzon:parse output))
         (finding (aref (gethash "findings" parsed) 0)))
    (is equal "bd-abc.1" (gethash "id" finding))
    (is equal "bd-abc.1.9" (gethash "requested-id" finding))))

(define-test doctor-unscoped-finds-dot-bucket
  :parent doctor-suite
  "bd-ywb: issues with source_repo '.', '' or NULL are unscoped."
  (beadwork:with-store (store ":memory:" :prefix "bd")
    (beadwork:create-issue store :title "Scoped" :type :task
                           :source-repo "cogen-meta")
    (let ((dot (beadwork:create-issue store :title "Dot" :type :task
                                      :source-repo "."))
          (blank (beadwork:create-issue store :title "Blank" :type :task
                                        :source-repo ""))
          (closed (beadwork:create-issue store :title "Closed dot" :type :task
                                         :source-repo ".")))
      (beadwork:update-issue store (beadwork:issue-id closed) :status :closed)
      (let ((ids (mapcar #'beadwork:issue-id
                         (beadwork::find-unscoped-issues store))))
        (is = 2 (length ids))
        (true (member (beadwork:issue-id dot) ids :test #'equal))
        (true (member (beadwork:issue-id blank) ids :test #'equal))
        (false (member (beadwork:issue-id closed) ids :test #'equal))))))
