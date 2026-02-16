(in-package :beadwork)

;;; ============================================================================
;;; ID Generation
;;;
;;; Matches br's ID format: {prefix}-{hash} where hash is a base-36 encoded
;;; substring of SHA-256(title + timestamp + random).  Hierarchical child IDs
;;; use dotted notation: {parent-id}.{counter}.
;;;
;;; See EXISTING_BEADS_STRUCTURE_AND_ARCHITECTURE.md §11 for the full spec.
;;; ============================================================================

(define-constant +base36-alphabet+ "0123456789abcdefghijklmnopqrstuvwxyz")

(defconstant +default-hash-length+ 3
  "Default number of base-36 characters for the hash portion of an ID.
Matches br's min_hash_length default.")

(defun base36-encode (bytes n)
  "Encode BYTES (an octet vector) as a base-36 string of length N.
Treats the byte vector as a big-endian unsigned integer and extracts N
base-36 digits (least-significant first)."
  (let ((value (loop for b across bytes
                     for result = (the integer b)
                       then (+ (ash result 8) b)
                     finally (return result)))
        (result (make-string n)))
    (dotimes (i n result)
      (multiple-value-bind (q r) (truncate value 36)
        (setf (schar result i) (schar +base36-alphabet+ r)
              value q)))))

(defun generate-id (title &key (prefix "bd") (hash-length +default-hash-length+))
  "Generate a hash-based issue ID from TITLE.

The ID has the format PREFIX-HASH where HASH is HASH-LENGTH base-36 characters
derived from SHA-256(title | timestamp-nanos | random-bytes).  This matches
br's ID generation algorithm (§11.1)."
  (let* ((timestamp (local-time:format-timestring
                     nil (local-time:now)
                     :format '((:year 4) #\- (:month 2) #\- (:day 2)
                               #\T (:hour 2) #\: (:min 2) #\: (:sec 2)
                               #\. (:nsec 9))))
         (nonce (ironclad:random-data 8))
         (digester (ironclad:make-digest :sha256)))
    (ironclad:update-digest digester
                           (babel:string-to-octets title :encoding :utf-8))
    (ironclad:update-digest digester
                           (babel:string-to-octets timestamp :encoding :utf-8))
    (ironclad:update-digest digester nonce)
    (let ((hash-bytes (ironclad:produce-digest digester)))
      (format nil "~A-~A" prefix (base36-encode hash-bytes hash-length)))))

(defun generate-child-id (parent-id child-number)
  "Generate a hierarchical child ID: PARENT-ID.CHILD-NUMBER.

For example, (generate-child-id \"bd-abc\" 1) => \"bd-abc.1\".
Matches br's dotted hierarchical ID format (§11.2)."
  (format nil "~A.~D" parent-id child-number))
