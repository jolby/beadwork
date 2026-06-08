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
