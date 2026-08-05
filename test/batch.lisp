(in-package #:beadwork/tests)

;;; Batch operations tests

(define-test batch-suite
  :parent beadwork-suite
  :description "Tests for bw batch — bulk create/update/link/comment in one transaction")

;;; ---------------------------------------------------------------------------
;;; Helpers
;;; ---------------------------------------------------------------------------

(defun run-batch (store json-string)
  "Run batch processing on STORE and return parsed JSON result."
  (let ((result-json (beadwork::process-batch store json-string)))
    (com.inuoe.jzon:parse result-json)))

(defun run-batch-with-key (store key json-string)
  "Run batch with idempotency key."
  (let ((result-json (beadwork::process-batch store json-string
                                               :idempotency-key key)))
    (com.inuoe.jzon:parse result-json)))

(defun batch-result-ok-p (result)
  "Check if batch result has ok: true."
  (gethash "ok" result))

(defun batch-first-id (result)
  "Get the id of the first result entry."
  (let ((results (gethash "results" result)))
    (when (and results (> (length results) 0))
      (gethash "id" (aref results 0)))))

;;; ============================================================================
;;; Single create
;;; ============================================================================

(define-test batch-creates-single-issue
  :parent batch-suite
  (beadwork:with-store (store ":memory:" :prefix "bd")
    (let* ((json "{\"operations\":[{\"op\":\"create\",\"ref\":\"t1\",\"title\":\"Test issue\",\"type\":\"task\"}]}")
           (result (run-batch store json)))
      (true (batch-result-ok-p result))
      (let ((id (batch-first-id result)))
        (true id)
        (let ((issue (beadwork:get-issue store id)))
          (is equal "Test issue" (beadwork:issue-title issue))
          (is equal :task (beadwork:issue-type issue)))))))

(define-test batch-create-returns-ref-mapping
  :parent batch-suite
  (beadwork:with-store (store ":memory:" :prefix "bd")
    (let* ((json "{\"operations\":[{\"op\":\"create\",\"ref\":\"my-ref\",\"title\":\"Named ref\",\"type\":\"feature\",\"priority\":\"P1\"}]}")
           (result (run-batch store json))
           (results (gethash "results" result)))
      (true (batch-result-ok-p result))
      (is equal "create" (gethash "op" (aref results 0)))
      (is equal "my-ref" (gethash "ref" (aref results 0)))
      (let ((id (gethash "id" (aref results 0))))
        (true id)
        (let ((issue (beadwork:get-issue store id)))
          (is equal "Named ref" (beadwork:issue-title issue))
          (is equal :feature (beadwork:issue-type issue))
          (is equal 1 (beadwork:issue-priority issue)))))))

(define-test batch-create-with-description
  :parent batch-suite
  (beadwork:with-store (store ":memory:" :prefix "bd")
    (let* ((json "{\"operations\":[{\"op\":\"create\",\"ref\":\"x\",\"title\":\"With desc\",\"type\":\"bug\",\"description\":\"A multi-line\\ndescription with 'quotes' and\\n-special chars.\",\"priority\":\"P2\"}]}")
           (result (run-batch store json)))
      (true (batch-result-ok-p result))
      (let ((issue (beadwork:get-issue store (batch-first-id result))))
        (true (search "multi-line" (beadwork:issue-description issue)))
        (true (search "special chars" (beadwork:issue-description issue)))
        (is equal :bug (beadwork:issue-type issue))
        (is equal 2 (beadwork:issue-priority issue))))))

(define-test batch-create-with-assignee
  :parent batch-suite
  (beadwork:with-store (store ":memory:" :prefix "bd")
    (let* ((json "{\"operations\":[{\"op\":\"create\",\"ref\":\"a\",\"title\":\"Assigned\",\"type\":\"task\",\"assignee\":\"agent-7\"}]}")
           (result (run-batch store json)))
      (true (batch-result-ok-p result))
      (let ((issue (beadwork:get-issue store (batch-first-id result))))
        (is equal "agent-7" (beadwork:issue-assignee issue))))))

(define-test batch-create-fails-without-title
  :parent batch-suite
  (beadwork:with-store (store ":memory:" :prefix "bd")
    (let* ((json "{\"operations\":[{\"op\":\"create\",\"ref\":\"x\",\"type\":\"task\"}]}")
           (result (run-batch store json)))
      (false (batch-result-ok-p result))
      (let ((error (gethash "error" result)))
        (true error)
        (true (search "title" (string-downcase error)))))))

(define-test batch-create-defaults-type-to-task
  :parent batch-suite
  (beadwork:with-store (store ":memory:" :prefix "bd")
    (let* ((json "{\"operations\":[{\"op\":\"create\",\"ref\":\"x\",\"title\":\"No type\"}]}")
           (result (run-batch store json)))
      ;; type defaults to :task, so the create should succeed
      (true (batch-result-ok-p result))
      (let ((issue (beadwork:get-issue store (batch-first-id result))))
        (is equal :task (beadwork:issue-type issue))))))

;;; ============================================================================
;;; Create with children
;;; ============================================================================

(define-test batch-creates-epic-with-children
  :parent batch-suite
  (beadwork:with-store (store ":memory:" :prefix "bd")
    (let* ((json "{\"operations\":[{\"op\":\"create\",\"ref\":\"epic\",\"title\":\"Epic parent\",\"type\":\"epic\",\"priority\":\"P1\",\"children\":[{\"op\":\"create\",\"ref\":\"child1\",\"title\":\"Child one\",\"type\":\"feature\"},{\"op\":\"create\",\"ref\":\"child2\",\"title\":\"Child two\",\"type\":\"bug\"}]}]}")
           (result (run-batch store json)))
      (true (batch-result-ok-p result))
      (let* ((results (gethash "results" result))
             (epic-id (gethash "id" (aref results 0)))
             (child1-id (gethash "id" (aref results 1)))
             (child2-id (gethash "id" (aref results 2))))
        ;; All three created
        (true epic-id)
        (true child1-id)
        (true child2-id)
        ;; Children are dotted IDs
        (true (search (format nil "~A." epic-id) child1-id))
        (true (search (format nil "~A." epic-id) child2-id))
        ;; Verify parent-child dependency exists
        (let ((deps1 (beadwork:list-dependencies store child1-id))
              (deps2 (beadwork:list-dependencies store child2-id)))
          (true deps1)
          (true deps2)
          (let ((d1 (first deps1)))
            (is equal epic-id (beadwork:dependency-depends-on-id d1))
            (is equal :parent-child (beadwork:dependency-dep-type d1))))))))

;;; ============================================================================
;;; Links (ref resolution)
;;; ============================================================================

(define-test batch-links-issues-by-ref
  :parent batch-suite
  (beadwork:with-store (store ":memory:" :prefix "bd")
    (let* ((json "{\"operations\":[{\"op\":\"create\",\"ref\":\"a\",\"title\":\"Issue A\",\"type\":\"feature\"},{\"op\":\"create\",\"ref\":\"b\",\"title\":\"Issue B\",\"type\":\"bug\"},{\"op\":\"link\",\"source\":{\"ref\":\"b\"},\"target\":{\"ref\":\"a\"},\"relation\":\"blocks\"}]}")
           (result (run-batch store json)))
      (true (batch-result-ok-p result))
      (let* ((results (gethash "results" result))
             (id-a (gethash "id" (aref results 0)))
             (id-b (gethash "id" (aref results 1))))
        ;; Verify B blocks A (dependency: b depends_on a, type blocks)
        (let ((deps (beadwork:list-dependencies store id-b)))
          (is equal 1 (length deps))
          (let ((d (first deps)))
            (is equal id-b (beadwork:dependency-issue-id d))
            (is equal id-a (beadwork:dependency-depends-on-id d))
            (is equal :blocks (beadwork:dependency-dep-type d))))))))

(define-test batch-links-to-existing-issue-by-id
  :parent batch-suite
  (beadwork:with-store (store ":memory:" :prefix "bd")
    (let* ((existing (beadwork:create-issue store
                       :title "Pre-existing" :type :feature))
           (existing-id (beadwork:issue-id existing))
           (json (format nil "{\"operations\":[{\"op\":\"create\",\"ref\":\"x\",\"title\":\"New issue\",\"type\":\"task\"},{\"op\":\"link\",\"source\":{\"ref\":\"x\"},\"target\":{\"id\":\"~A\"},\"relation\":\"waits-for\"}]}" existing-id))
           (result (run-batch store json)))
      (true (batch-result-ok-p result))
      (let* ((results (gethash "results" result))
             (new-id (gethash "id" (aref results 0))))
        (let ((deps (beadwork:list-dependencies store new-id)))
          (is equal 1 (length deps))
          (let ((d (first deps)))
            (is equal existing-id (beadwork:dependency-depends-on-id d))
            (is equal :waits-for (beadwork:dependency-dep-type d))))))))

(define-test batch-link-fails-on-unknown-ref
  :parent batch-suite
  (beadwork:with-store (store ":memory:" :prefix "bd")
    (let* ((json "{\"operations\":[{\"op\":\"create\",\"ref\":\"a\",\"title\":\"Only A\",\"type\":\"task\"},{\"op\":\"link\",\"source\":{\"ref\":\"a\"},\"target\":{\"ref\":\"nonexistent\"},\"relation\":\"blocks\"}]}")
           (result (run-batch store json)))
      (false (batch-result-ok-p result))
      (let ((error (gethash "error" result)))
        (true error)
        (true (search "nonexistent" error))))))

(define-test batch-link-fails-on-unknown-relation
  :parent batch-suite
  (beadwork:with-store (store ":memory:" :prefix "bd")
    (let* ((json "{\"operations\":[{\"op\":\"create\",\"ref\":\"a\",\"title\":\"A\",\"type\":\"task\"},{\"op\":\"link\",\"source\":{\"ref\":\"a\"},\"target\":{\"ref\":\"a\"},\"relation\":\"frobnicates\"}]}")
           (result (run-batch store json)))
      (false (batch-result-ok-p result)))))

;;; ============================================================================
;;; Comments
;;; ============================================================================

(define-test batch-adds-comment-to-new-issue-by-ref
  :parent batch-suite
  (beadwork:with-store (store ":memory:" :prefix "bd")
    (let* ((json "{\"operations\":[{\"op\":\"create\",\"ref\":\"a\",\"title\":\"Comment target\",\"type\":\"task\"},{\"op\":\"comment\",\"id\":{\"ref\":\"a\"},\"text\":\"First comment from batch\"}]}")
           (result (run-batch store json)))
      (true (batch-result-ok-p result))
      (let* ((results (gethash "results" result))
             (id (gethash "id" (aref results 0)))
             (comments (beadwork:list-comments store id)))
        (is equal 1 (length comments))
        (let ((c (first comments)))
          (is equal "First comment from batch" (beadwork::comment-body c)))))))

(define-test batch-adds-comment-to-existing-issue
  :parent batch-suite
  (beadwork:with-store (store ":memory:" :prefix "bd")
    (let* ((existing (beadwork:create-issue store
                       :title "Old issue" :type :chore))
           (existing-id (beadwork:issue-id existing))
           (json (format nil "{\"operations\":[{\"op\":\"comment\",\"id\":{\"id\":\"~A\"},\"text\":\"Batch comment on old issue\"}]}" existing-id))
           (result (run-batch store json)))
      (true (batch-result-ok-p result))
      (let ((comments (beadwork:list-comments store existing-id)))
        (is equal 1 (length comments))))))

;;; ============================================================================
;;; Updates
;;; ============================================================================

(define-test batch-updates-existing-issue
  :parent batch-suite
  (beadwork:with-store (store ":memory:" :prefix "bd")
    (let* ((existing (beadwork:create-issue store
                       :title "Old title" :type :task))
           (existing-id (beadwork:issue-id existing))
           (json (format nil "{\"operations\":[{\"op\":\"update\",\"id\":\"~A\",\"title\":\"Updated title\",\"status\":\"in_progress\",\"priority\":\"P1\"}]}" existing-id))
           (result (run-batch store json)))
      (true (batch-result-ok-p result))
      (let ((issue (beadwork:get-issue store existing-id)))
        (is equal "Updated title" (beadwork:issue-title issue))
        (is equal :in-progress (beadwork:issue-status issue))
        (is equal 1 (beadwork:issue-priority issue))))))

(define-test batch-update-fails-on-nonexistent-issue
  :parent batch-suite
  (beadwork:with-store (store ":memory:" :prefix "bd")
    (let* ((json "{\"operations\":[{\"op\":\"update\",\"id\":\"bd-nonexistent\",\"title\":\"Nope\"}]}")
           (result (run-batch store json)))
      (false (batch-result-ok-p result)))))

;;; ============================================================================
;;; Idempotency
;;; ============================================================================

(define-test batch-idempotency-returns-cached-result
  :parent batch-suite
  (beadwork:with-store (store ":memory:" :prefix "bd")
    (let* ((key "idem-test-1")
           (json "{\"idempotency_key\":\"idem-test-1\",\"operations\":[{\"op\":\"create\",\"ref\":\"x\",\"title\":\"Idempotent\",\"type\":\"task\"}]}")
           (result1 (run-batch-with-key store key json))
           (result2 (run-batch-with-key store key json)))
      ;; Both succeed
      (true (batch-result-ok-p result1))
      (true (batch-result-ok-p result2))
      ;; Same IDs in both responses
      (let ((id1 (gethash "id" (aref (gethash "results" result1) 0)))
            (id2 (gethash "id" (aref (gethash "results" result2) 0))))
        (is equal id1 id2))
      ;; Only one issue actually created
      (let ((issues (beadwork:list-issues store :source-repo nil)))
        (is equal 1 (length issues))))))

(define-test batch-idempotency-different-keys-create-separately
  :parent batch-suite
  (beadwork:with-store (store ":memory:" :prefix "bd")
    (let* ((json1 "{\"idempotency_key\":\"key-aaa\",\"operations\":[{\"op\":\"create\",\"ref\":\"x\",\"title\":\"First key\",\"type\":\"task\"}]}")
           (json2 "{\"idempotency_key\":\"key-bbb\",\"operations\":[{\"op\":\"create\",\"ref\":\"y\",\"title\":\"Second key\",\"type\":\"feature\"}]}")
           (r1 (run-batch store json1))
           (r2 (run-batch store json2)))
      (true (batch-result-ok-p r1))
      (true (batch-result-ok-p r2))
      ;; Two issues created (different keys)
      (let ((issues (beadwork:list-issues store :source-repo nil)))
        (true (>= (length issues) 2))))))

;;; ============================================================================
;;; Mixed operations (single transaction)
;;; ============================================================================

(define-test batch-mixed-ops-atomic
  :parent batch-suite
  "Creates issues, links them, adds comments, all in one atomic transaction."
  (beadwork:with-store (store ":memory:" :prefix "bd")
    (let* ((json "{\"operations\":[{\"op\":\"create\",\"ref\":\"epic\",\"title\":\"Mixed epic\",\"type\":\"epic\",\"children\":[{\"op\":\"create\",\"ref\":\"child\",\"title\":\"Mixed child\",\"type\":\"feature\"}]},{\"op\":\"link\",\"source\":{\"ref\":\"child\"},\"target\":{\"ref\":\"epic\"},\"relation\":\"blocks\"},{\"op\":\"comment\",\"id\":{\"ref\":\"epic\"},\"text\":\"Batch created\"}]}")
           (result (run-batch store json)))
      (true (batch-result-ok-p result))
      (let* ((results (gethash "results" result))
             (epic-id (gethash "id" (aref results 0)))
             (child-id (gethash "id" (aref results 1))))
        ;; Epic exists
        (true (beadwork:get-issue store epic-id))
        ;; Child exists
        (true (beadwork:get-issue store child-id))
        ;; Dependency exists (child has parent dep to epic)
        (let ((deps (beadwork:list-dependencies store child-id)))
          (true (find epic-id deps :key #'beadwork:dependency-depends-on-id :test #'equal)))
        ;; Comment on epic
        (is equal 1 (length (beadwork:list-comments store epic-id)))))))

;;; ============================================================================
;;; Rollback on error (atomicity)
;;; ============================================================================

(define-test batch-rolls-back-on-link-error
  :parent batch-suite
  "If a link references an unknown ref, the entire batch rolls back."
  (beadwork:with-store (store ":memory:" :prefix "bd")
    (let* ((json "{\"operations\":[{\"op\":\"create\",\"ref\":\"a\",\"title\":\"Should roll back\",\"type\":\"task\"},{\"op\":\"link\",\"source\":{\"ref\":\"a\"},\"target\":{\"ref\":\"does-not-exist\"},\"relation\":\"blocks\"}]}")
           (result (run-batch store json)))
      (false (batch-result-ok-p result))
      ;; No issues should exist — transaction rolled back
      (let ((issues (beadwork:list-issues store :source-repo nil)))
        (is equal 0 (length issues))))))
