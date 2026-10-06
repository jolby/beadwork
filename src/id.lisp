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

(define-constant +base36-alphabet+ "0123456789abcdefghijklmnopqrstuvwxyz" :test 'string=)

(define-constant +default-hash-length+ 3
  :documentation
  "Default number of base-36 characters for the hash portion of an ID.
Matches br's min_hash_length default.")

(define-constant +max-hash-length+ 8
  :documentation
  "Maximum base-36 hash length for the hash portion of an ID.
Matches br's max_hash_length default (spec 11.1.1).")

(defparameter *id-collision-retries* 10
  "Fresh-nonce attempts per hash length in GENERATE-UNIQUE-ID before the
length is grown.  Mirrors br's nonce 0..9 collision fallback.")

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

(defun generate-unique-id (title &key (prefix "bd")
                                     (min-length +default-hash-length+)
                                     (max-length +max-hash-length+)
                                     (exists-p (constantly nil)))
  "Generate an ID for TITLE that EXISTS-P does not already claim.

Retries with a fresh nonce up to *ID-COLLISION-RETRIES* times at each hash
length from MIN-LENGTH through MAX-LENGTH, growing the length once the retries
are exhausted.  This is the collision fallback br performs (spec 11.1 / 15.27)
and keeps CREATE-ISSUE from raising a raw UNIQUE-constraint error.

EXISTS-P is called with a candidate ID string and should return true when the
ID is already taken.  Signals BEADWORK-ERROR if no free ID is found."
  (loop for length from min-length to max-length
        do (loop repeat *id-collision-retries*
                 for candidate = (generate-id title :prefix prefix
                                              :hash-length length)
                 unless (funcall exists-p candidate)
                   do (return-from generate-unique-id candidate)))
  (error 'beadwork-error
         :message (format nil
                          "Could not generate a unique ID for ~S after ~D attempts"
                          title (* (1+ (- max-length min-length))
                                   *id-collision-retries*))))

(defun generate-child-id (parent-id child-number)
  "Generate a hierarchical child ID: PARENT-ID.CHILD-NUMBER.

For example, (generate-child-id \"bd-abc\" 1) => \"bd-abc.1\".
Matches br's dotted hierarchical ID format (§11.2)."
  (format nil "~A.~D" parent-id child-number))
