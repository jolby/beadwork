(in-package #:beadwork)

;;;; Batch Operations  --  bulk create/update/link/comment in one transaction
;;;;
;;;; Entry point: process-batch(store json-string &key idempotency-key) → json-result
;;;;
;;;; Design: bd-ol8 — bw: batch operations via JSON
;;;; Spec: two-pass processing (create first, resolve refs second),
;;;;       idempotency via idempotency_keys table, single-transaction atomicity.

;;; ============================================================================
;;; JSON helpers (no jzon dependency in this package — uses com.inuoe.jzon)
;;; ============================================================================

(defun %batch-parse (json-string)
  "Parse JSON-STRING and return a hash-table. Signals error on parse failure."
  (handler-case
      (let ((result (com.inuoe.jzon:parse json-string)))
        (unless (hash-table-p result)
          (error 'beadwork-error :message "Batch payload must be a JSON object"))
        result)
    (error (c)
      (error 'beadwork-error :message (format nil "Invalid JSON: ~A" c)))))

(defun %batch-get (ht key &key (type :string))
  "Get KEY from hash-table HT, return NIL if missing. Optionally check type."
  (let ((val (gethash key ht)))
    (when val
      (ecase type
        (:string (unless (stringp val)
                   (error 'beadwork-error :message
                          (format nil "Field '~A' must be a string, got: ~S" key val)))
                 val)
        (:hash-table (unless (hash-table-p val)
                       (error 'beadwork-error :message
                              (format nil "Field '~A' must be an object, got: ~S" key val)))
                     val)
        (:array (unless (vectorp val)
                  (error 'beadwork-error :message
                         (format nil "Field '~A' must be an array, got: ~S" key val)))
                val)))
    val))

(defun %batch-get-required (ht key &key (type :string))
  "Get a required field from hash-table HT. Signals if missing."
  (let ((val (gethash key ht)))
    (when (or (null val) (eq val :null))
      (error 'beadwork-error :message (format nil "Missing required field: '~A'" key)))
    (ecase type
      (:string (unless (stringp val)
                 (error 'beadwork-error :message
                        (format nil "Field '~A' must be a string, got: ~S" key val)))
               val)
      (:hash-table (unless (hash-table-p val)
                     (error 'beadwork-error :message
                            (format nil "Field '~A' must be an object, got: ~S" key val)))
                   val)
      (:array (unless (vectorp val)
                (error 'beadwork-error :message
                       (format nil "Field '~A' must be an array, got: ~S" key val)))
              val))))

(defun %batch-get-string (ht key)
  "Get KEY as string, return NIL if missing."
  (%batch-get ht key :type :string))

(defun %batch-get-array (ht key)
  "Get KEY as array, return NIL if missing."
  (%batch-get ht key :type :array))

;;; ============================================================================
;;; Ref resolution
;;; ============================================================================

(defvar *ref-map* nil
  "Dynamic variable: hash-table mapping batch ref labels → beadwork IDs.
Bound during batch processing for use in link resolution pass.")

(defun %resolve-ref (target)
  "Resolve a TARGET reference which is either {\"ref\": \"...\"} or {\"id\": \"...\"}.
Returns the beadwork issue ID string. Signals error on unknown refs."
  (let ((ref (%batch-get-string target "ref"))
        (id (%batch-get-string target "id")))
    (cond
      (ref
       (let ((resolved (gethash ref *ref-map*)))
         (unless resolved
           (error 'beadwork-error :message
                  (format nil "Unknown ref '~A' in link — create the issue with this ref before linking to it" ref)))
         resolved))
      (id id)
      (t (error 'beadwork-error :message
                "Link target must have 'ref' or 'id' field")))))

;;; ============================================================================
;;; Operation handlers
;;; ============================================================================

(defun %batch-create (store op result-array)
  "Handle a single create operation. Returns the created issue's beadwork ID.
RESULT-ARRAY is the vector of result objects to append to."
  (let* ((ref (%batch-get-string op "ref"))
         (title (%batch-get-required op "title" :type :string))
         (itype-str (%batch-get-string op "type"))
         (itype (if itype-str (parse-issue-type itype-str) :task))
         (priority-str (%batch-get-string op "priority"))
         (priority (if priority-str (parse-priority priority-str) 2))
         (description (%batch-get-string op "description"))
         (assignee (%batch-get-string op "assignee"))
         (children (%batch-get-array op "children"))
         (parent-ref nil))
    ;; Create the issue
    (let ((issue (create-issue store
                   :title title
                   :type itype
                   :priority priority
                   :description description
                   :assignee assignee
                   :source-repo ".")))
      (let ((id (issue-id issue)))
        ;; Record in ref map if ref provided
        (when ref
          (setf (gethash ref *ref-map*) id))
        ;; Record result
        (let ((result-ht (make-hash-table :test #'equal)))
          (setf (gethash "op" result-ht) "create")
          (when ref (setf (gethash "ref" result-ht) ref))
          (setf (gethash "id" result-ht) id)
          (vector-push-extend result-ht result-array))
        ;; Process children recursively (depth-first), auto-linking parent-child
        (when children
          (dotimes (i (length children))
            (let ((child-op (aref children i)))
              ;; Ensure child has op="create"
              (let ((child-op-type (%batch-get-string child-op "op")))
                (unless (string= child-op-type "create")
                  (error 'beadwork-error :message
                         "Only 'create' ops are allowed inside children[]")))
              ;; Create child with parent linkage
              (let* ((child-title (%batch-get-required child-op "title" :type :string))
                    (child-type-str (%batch-get-string child-op "type"))
                    (child-type (if child-type-str (parse-issue-type child-type-str) :task))
                    (child-prio-str (%batch-get-string child-op "priority"))
                    (child-prio (if child-prio-str (parse-priority child-prio-str) 2))
                    (child-desc (%batch-get-string child-op "description"))
                    (child-assignee (%batch-get-string child-op "assignee"))
                    (child-ref (%batch-get-string child-op "ref")))
                (let ((child-issue (create-issue store
                                     :title child-title
                                     :type child-type
                                     :priority child-prio
                                     :description child-desc
                                     :assignee child-assignee
                                     :parent id
                                     :source-repo ".")))
                  (let ((child-id (issue-id child-issue)))
                    (when child-ref
                      (setf (gethash child-ref *ref-map*) child-id))
                    (let ((child-result-ht (make-hash-table :test #'equal)))
                      (setf (gethash "op" child-result-ht) "create")
                      (when child-ref (setf (gethash "ref" child-result-ht) child-ref))
                      (setf (gethash "id" child-result-ht) child-id)
                      (vector-push-extend child-result-ht result-array))
                    ;; Recurse into grand-children
                    (let ((grand-children (%batch-get-array child-op "children")))
                      (when grand-children
                        (error 'beadwork-error :message
                               "Nested children beyond one level are not supported — use explicit link operations for deeper hierarchies")))))))))
        id))))

(defun %batch-update (store op result-array)
  "Handle an update operation."
  (let* ((id (%batch-get-required op "id" :type :string))
         (title (%batch-get-string op "title"))
         (status-str (%batch-get-string op "status"))
         (status (when status-str (parse-status status-str)))
         (priority-str (%batch-get-string op "priority"))
         (priority (when priority-str (parse-priority priority-str)))
         (description (%batch-get-string op "description"))
         (assignee (%batch-get-string op "assignee")))
    ;; Verify issue exists
    (get-issue store id)
    ;; Update
    (let ((issue (update-issue store id
                   :title title
                   :status status
                   :priority priority
                   :description description
                   :assignee assignee)))
      (declare (ignore issue))
      (let ((result-ht (make-hash-table :test #'equal)))
        (setf (gethash "op" result-ht) "update")
        (setf (gethash "id" result-ht) id)
        (vector-push-extend result-ht result-array)))))

(defun %batch-link (store op result-array)
  "Handle a link operation (pass 2 — all creates resolved).
Uses upsert so that linking an already-dependent issue updates the type."
  (let* ((source-ht (%batch-get-required op "source" :type :hash-table))
         (target-ht (%batch-get-required op "target" :type :hash-table))
         (relation-str (%batch-get-string op "relation"))
         (relation (if relation-str (parse-dependency-type relation-str) :blocks))
         (source-id (%resolve-ref source-ht))
         (target-id (%resolve-ref target-ht))
         (now-str (format-timestamp (local-time:now))))
    ;; Use INSERT OR REPLACE so children[]-created parent-child deps
    ;; can be upgraded to explicit types without UNIQUE violations.
    (sqlite:execute-non-query
     (store-db store)
     "INSERT OR REPLACE INTO dependencies (issue_id, depends_on_id, type, created_at, created_by)
      VALUES (?, ?, ?, ?, '')"
     source-id target-id (dependency-type-string relation) now-str)
    (mark-dirty store source-id)
    (let ((result-ht (make-hash-table :test #'equal)))
      (setf (gethash "op" result-ht) "link")
      (setf (gethash "source" result-ht) source-id)
      (setf (gethash "target" result-ht) target-id)
      (setf (gethash "relation" result-ht) (dependency-type-string relation))
      (vector-push-extend result-ht result-array))))

(defun %batch-comment (store op result-array)
  "Handle a comment operation. Target can be {ref: ...} or {id: ...}."
  (let* ((id-ht (%batch-get-required op "id" :type :hash-table))
         (text (%batch-get-required op "text" :type :string))
         (target-id (%resolve-ref id-ht)))
    (add-comment store target-id "[batch]" text)
    (let ((result-ht (make-hash-table :test #'equal)))
      (setf (gethash "op" result-ht) "comment")
      (setf (gethash "id" result-ht) target-id)
      (setf (gethash "ok" result-ht) t)
      (vector-push-extend result-ht result-array))))

;;; ============================================================================
;;; Idempotency
;;; ============================================================================

(defun %check-idempotency (store key)
  "Check if KEY has been committed. Returns cached result JSON string, or NIL."
  (let ((rows (sqlite:execute-to-list
               (store-db store)
               "SELECT result FROM idempotency_keys WHERE key = ?"
               key)))
    (when rows
      (let ((cached (first (first rows))))
        (when (and cached (plusp (length cached)))
          cached)))))

(defun %store-idempotency (store key result-json)
  "Store the result of a successful batch operation."
  (sqlite:execute-non-query
   (store-db store)
   "INSERT OR REPLACE INTO idempotency_keys (key, result, committed_at) VALUES (?, ?, ?)"
   key result-json (format-timestamp (local-time:now))))

;;; ============================================================================
;;; Main entry point
;;; ============================================================================

(defun process-batch (store json-string &key idempotency-key)
  "Process a batch of operations encoded as JSON.
STORE is a beadwork store instance.
JSON-STRING is the JSON payload with an \"operations\" array.
IDEMPOTENCY-KEY, if given, enables idempotent replay — retrying the same key
returns the cached result instead of re-executing.
Returns a JSON string with {ok: true/false, results: [...], error: ...}."
  (let ((payload nil))
    ;; Parse JSON (validate before we check idempotency)
    (handler-case
        (setf payload (%batch-parse json-string))
      (beadwork-error (e)
        (return-from process-batch
          (com.inuoe.jzon:stringify
           (let ((ht (make-hash-table :test #'equal)))
             (setf (gethash "ok" ht) nil)
             (setf (gethash "error" ht) (format nil "~A" e))
             ht)))))
    ;; Check idempotency — key can come from function parameter or JSON payload
    (let ((effective-key (or idempotency-key
                             (%batch-get-string payload "idempotency_key"))))
      (when effective-key
        (let ((cached (%check-idempotency store effective-key)))
          (when cached
            (return-from process-batch cached))))
    ;; Validate operations array
    (let ((ops-vec (%batch-get-array payload "operations")))
      (unless (and ops-vec (> (length ops-vec) 0))
        (return-from process-batch
          (com.inuoe.jzon:stringify
           (let ((ht (make-hash-table :test #'equal)))
             (setf (gethash "ok" ht) nil)
             (setf (gethash "error" ht) "operations array is empty or missing")
             ht))))
      ;; Process in a single transaction with two passes:
      ;; Pass 1: all create operations (building ref map)
      ;; Pass 2: link and comment operations (resolving refs)
      (handler-case
          (let* ((db (store-db store))
               (result-array (make-array 0 :fill-pointer t :adjustable t))
               (*ref-map* (make-hash-table :test #'equal)))
          (sqlite:execute-non-query db "BEGIN IMMEDIATE")
          (unwind-protect
               (progn
                 ;; Pass 1: Creates (collect all refs)
                 (dotimes (i (length ops-vec))
                   (let* ((op-ht (aref ops-vec i))
                          (op-type (%batch-get-string op-ht "op")))
                     (when (string= op-type "create")
                       (%batch-create store op-ht result-array))))
                 ;; Pass 2: Updates, links, comments (resolve refs)
                 (dotimes (i (length ops-vec))
                   (let* ((op-ht (aref ops-vec i))
                          (op-type (%batch-get-string op-ht "op")))
                     (cond
                       ((string= op-type "update")
                        (%batch-update store op-ht result-array))
                       ((string= op-type "link")
                        (%batch-link store op-ht result-array))
                       ((string= op-type "comment")
                        (%batch-comment store op-ht result-array))
                       ((string= op-type "create")
                        ;; Already handled in pass 1
                        nil)
                       (t
                        (error 'beadwork-error :message
                               (format nil "Unknown operation type: '~A'" op-type))))))
                 (sqlite:execute-non-query db "COMMIT")
                 ;; Build success response
                 (let ((response-ht (make-hash-table :test #'equal)))
                   (setf (gethash "ok" response-ht) t)
                   (setf (gethash "results" response-ht) result-array)
                   (let ((response-json (com.inuoe.jzon:stringify response-ht)))
                     ;; Store for idempotency
                     (when effective-key
                       (%store-idempotency store effective-key response-json))
                     response-json)))
            ;; Rollback on any error during processing
            (handler-case
                (sqlite:execute-non-query db "ROLLBACK")
              (error ()))))
      (beadwork-error (e)
        (com.inuoe.jzon:stringify
         (let ((ht (make-hash-table :test #'equal)))
           (setf (gethash "ok" ht) nil)
           (setf (gethash "error" ht) (format nil "~A" e))
           ht)))
      (error (e)
        (com.inuoe.jzon:stringify
         (let ((ht (make-hash-table :test #'equal)))
           (setf (gethash "ok" ht) nil)
           (setf (gethash "error" ht) (format nil "Unexpected error: ~A" e))
           ht))))))))
