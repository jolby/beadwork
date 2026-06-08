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
      ;; Row with no valid bw ID → ORPHAN
      (let ((result (beadwork::classify-row
                     (list "P1" "No ID" "—") store)))
        (is equal :orphan (getf result :status))
        (is equal nil (getf result :id))))
    (beadwork::close-store store)))

(define-test extracts-bw-ids
  :parent doctor-suite
  ;; Valid bw IDs
  (is equal "bd-0lg" (beadwork::extract-bw-id "bd-0lg"))
  (is equal "bd-abc" (beadwork::extract-bw-id "bd-abc"))
  (is equal "bd-iq1" (beadwork::extract-bw-id "bd-iq1"))
  ;; Non-bw-ID values return nil
  (is equal nil (beadwork::extract-bw-id "—"))
  (is equal nil (beadwork::extract-bw-id "note `f9efe052`"))
  (is equal nil (beadwork::extract-bw-id ""))
  (is equal nil (beadwork::extract-bw-id "N/A"))
  ;; Embedded ID in text returns nil (strict match)
  (is equal nil (beadwork::extract-bw-id "see bd-abc for details")))

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
        (true (getf result :findings))
        (true (getf result :summary))
        (true (listp (getf result :findings)))))))

(define-test reports-doctor-findings-human
  :parent doctor-suite
  (let* ((findings (list (list :status :ok :id "bd-abc" :task "Fix bug" :issue-status :open)
                         (list :status :stale :id "bd-xyz" :task "Done thing" :issue-status :closed)
                         (list :status :missing :id "bd-zzz" :task "Ghost" :issue-status nil)
                         (list :status :orphan :id nil :task "No ID task" :issue-status nil)))
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
