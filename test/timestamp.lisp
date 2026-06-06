(in-package #:beadwork/tests)

;;; Timestamp Formatting Tests
;;;
;;; Verify that format-timestamp produces RFC 3339 timestamps with microsecond
;;; precision (6 fractional digits), matching br (beads_rust) compatibility.
;;; Nanosecond precision (9 digits) causes br to fail with "Failed to parse
;;; datetime".

(define-test timestamp-microsecond-precision
  :parent beadwork-suite
  "format-timestamp produces RFC 3339 with microsecond (6-digit) precision"
  (let* ((now (local-time:now))
         (ts (beadwork::format-timestamp now)))
    ;; Format: YYYY-MM-DDTHH:MM:SS.ffffff±HHMM
    ;; 10(date)+1(T)+8(time)+1(.)+6(frac)+5(offset) = 31
    (is = 31 (length ts))
    (true (cl-ppcre:scan "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}\\.\\d{6}[+-]\\d{4}$" ts)
          "Timestamp ~s does not match RFC 3339 with microsecond format" ts)))

(define-test timestamp-utc-microsecond-precision
  :parent beadwork-suite
  "format-timestamp-utc produces microsecond precision in UTC"
  (let* ((now (local-time:now))
         (ts (beadwork::format-timestamp-utc now)))
    ;; UTC timestamps: YYYY-MM-DDTHH:MM:SS.ffffff+0000
    ;; 10(date)+1(T)+8(time)+1(.)+6(frac)+5(offset) = 31
    (is = 31 (length ts))
    (true (cl-ppcre:scan "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}\\.\\d{6}[-+]\\d{4}$" ts)
          "UTC timestamp ~s does not match expected format" ts)))

(define-test timestamp-parse-roundtrip
  :parent beadwork-suite
  "parse-timestamp correctly round-trips through format-timestamp"
  (let* ((original (local-time:now))
         (formatted (beadwork::format-timestamp original))
         (parsed (beadwork::parse-timestamp formatted)))
    (true parsed "parse-timestamp returned NIL for ~s" formatted)
    ;; Timestamps should be equal to within 1 second (since we lose
    ;; nanosecond precision in the round-trip)
    (true (local-time:timestamp= original parsed)
          "Round-trip failed: ~s vs ~s"
          (beadwork::format-timestamp original)
          (beadwork::format-timestamp parsed))))

(define-test timestamp-backward-compat-parse
  :parent beadwork-suite
  "parse-timestamp still parses nanosecond (9-digit) timestamps from old bw"
  (let ((old-format "2026-06-06T12:06:34.923079000-0700"))
    (true (beadwork::parse-timestamp old-format)
          "parse-timestamp must still parse nanosecond timestamps for backward compat")))
