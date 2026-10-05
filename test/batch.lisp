(in-package #:beadwork/tests)

;;; Batch operations tests

(define-test batch-suite
  :parent beadwork-suite
  :description "Tests for bw batch -- bulk create/update/link/comment in one transaction")

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
;;; Source-repo attribution (bd-818)
;;; ============================================================================

(define-test batch-create-persists-default-source-repo
  :parent batch-suite
  "bd-818: source-repo passed to process-batch is persisted on creates."
  (beadwork:with-store (store ":memory:" :prefix "bd")
    (let* ((json "{\"operations\":[{\"op\":\"create\",\"ref\":\"r\",\"title\":\"Repo default\",\"type\":\"task\"}]}")
           (result (beadwork::process-batch store json :source-repo "beadwork"))
           (parsed (com.inuoe.jzon:parse result)))
      (true (batch-result-ok-p parsed))
      (is equal "beadwork"
               (beadwork:issue-source-repo
                (beadwork:get-issue store (batch-first-id parsed)))))))

(define-test batch-create-per-op-repo-overrides-default
  :parent batch-suite
  "bd-818: a per-op \"repo\" field wins over the process-batch default."
  (beadwork:with-store (store ":memory:" :prefix "bd")
    (let* ((json "{\"operations\":[{\"op\":\"create\",\"ref\":\"r\",\"title\":\"Repo op\",\"type\":\"task\",\"repo\":\"csct\"}]}")
           (result (beadwork::process-batch store json :source-repo "beadwork"))
           (parsed (com.inuoe.jzon:parse result)))
      (true (batch-result-ok-p parsed))
      (is equal "csct"
               (beadwork:issue-source-repo
                (beadwork:get-issue store (batch-first-id parsed)))))))

(define-test batch-create-children-inherit-repo
  :parent batch-suite
  "bd-818: children[] inherit the parent's resolved repo unless they set
their own \"repo\" field."
  (beadwork:with-store (store ":memory:" :prefix "bd")
    (let* ((json "{\"operations\":[{\"op\":\"create\",\"ref\":\"p\",\"title\":\"Parent\",\"type\":\"epic\",\"children\":[{\"op\":\"create\",\"ref\":\"c1\",\"title\":\"Child inherit\",\"type\":\"task\"},{\"op\":\"create\",\"ref\":\"c2\",\"title\":\"Child override\",\"type\":\"task\",\"repo\":\"cogen-kb\"}]}]}")
           (result (beadwork::process-batch store json :source-repo "beadwork"))
           (parsed (com.inuoe.jzon:parse result))
           (results (gethash "results" parsed)))
      (true (batch-result-ok-p parsed))
      (is equal "beadwork"
               (beadwork:issue-source-repo
                (beadwork:get-issue store (gethash "id" (aref results 0)))))
      (is equal "beadwork"
               (beadwork:issue-source-repo
                (beadwork:get-issue store (gethash "id" (aref results 1)))))
      (is equal "cogen-kb"
               (beadwork:issue-source-repo
                (beadwork:get-issue store (gethash "id" (aref results 2))))))))

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

(define-test batch-update-to-closed-stamps-closed-at
  :parent batch-suite
  "batch update op with status closed must set closed_at, not trip the schema
CHECK constraint (bd-lu5)."
  (beadwork:with-store (store ":memory:" :prefix "bd")
    (let* ((existing (beadwork:create-issue store :title "Batch close" :type :task))
           (existing-id (beadwork:issue-id existing))
           (json (format nil "{\"operations\":[{\"op\":\"update\",\"id\":\"~A\",\"status\":\"closed\"}]}" existing-id))
           (result (run-batch store json)))
      (true (batch-result-ok-p result))
      (let ((issue (beadwork:get-issue store existing-id)))
        (is eq :closed (beadwork:issue-status issue))
        (true (beadwork:issue-closed-at issue)
              "batch close must stamp closed_at")))))

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
      ;; No issues should exist -- transaction rolled back
      (let ((issues (beadwork:list-issues store :source-repo nil)))
        (is equal 0 (length issues))))))

;;; ============================================================================
;;; Dry run (bd-vcu)
;;; ============================================================================

(define-test batch-dry-run-does-not-persist
  :parent batch-suite
  "bd-vcu: a dry run validates and reports would-be ids but must not write
anything to the database."
  (beadwork:with-store (store ":memory:" :prefix "bd")
    (let* ((json "{\"operations\":[{\"op\":\"create\",\"ref\":\"x\",\"title\":\"Dry run issue\",\"type\":\"task\"}]}")
           (result (beadwork::process-batch store json :dry-run t))
           (parsed (com.inuoe.jzon:parse result)))
      (true (batch-result-ok-p parsed))
      (true (batch-first-id parsed) "dry run still reports a would-be id")
      (is equal 0 (length (beadwork:list-issues store :source-repo nil))
          "dry run must not persist created issues"))))

(define-test batch-dry-run-marks-response
  :parent batch-suite
  "A dry-run response is distinguishable from a committed one."
  (beadwork:with-store (store ":memory:" :prefix "bd")
    (let* ((json "{\"operations\":[{\"op\":\"create\",\"ref\":\"x\",\"title\":\"Marker\",\"type\":\"task\"}]}")
           (parsed (com.inuoe.jzon:parse
                    (beadwork::process-batch store json :dry-run t))))
      (true (gethash "dry-run" parsed)))))

(define-test batch-dry-run-does-not-store-idempotency
  :parent batch-suite
  "bd-vcu: a dry run must not poison the idempotency cache. A real run with
the same key afterwards must still create the issue."
  (beadwork:with-store (store ":memory:" :prefix "bd")
    (let* ((json "{\"idempotency_key\":\"dry-key-1\",\"operations\":[{\"op\":\"create\",\"ref\":\"x\",\"title\":\"After dry run\",\"type\":\"task\"}]}")
           (dry (com.inuoe.jzon:parse
                 (beadwork::process-batch store json :dry-run t)))
           (real (com.inuoe.jzon:parse
                  (beadwork::process-batch store json))))
      (true (batch-result-ok-p dry))
      (true (batch-result-ok-p real))
      (is equal 1 (length (beadwork:list-issues store :source-repo nil))
          "the real run must create the issue; dry run must not have cached it"))))

(define-test batch-dry-run-does-not-apply-links
  :parent batch-suite
  "Dry-run link ops are simulated inside the transaction and rolled back."
  (beadwork:with-store (store ":memory:" :prefix "bd")
    (let* ((a (beadwork:create-issue store :title "A" :type :task))
           (json (format nil "{\"operations\":[{\"op\":\"link\",\"source\":{\"id\":\"~A\"},\"target\":{\"id\":\"~A\"},\"relation\":\"blocks\"}]}"
                         (beadwork:issue-id a) (beadwork:issue-id a)))
           (parsed (com.inuoe.jzon:parse
                    (beadwork::process-batch store json :dry-run t))))
      (true (batch-result-ok-p parsed))
      (is equal 0 (length (beadwork:list-dependencies store (beadwork:issue-id a)))))))

;;; ============================================================================
;;; Orphaned-hierarchy warnings (bd-uz3)
;;; ============================================================================

(define-test batch-warns-on-unlinked-top-level-create
  :parent batch-suite
  "A top-level create with no children that no link op references is a
likely-orphaned hierarchy and must produce a warning."
  (beadwork:with-store (store ":memory:" :prefix "bd")
    (let* ((json "{\"operations\":[{\"op\":\"create\",\"ref\":\"epic\",\"title\":\"Lonely epic\",\"type\":\"epic\"},{\"op\":\"create\",\"ref\":\"other\",\"title\":\"Other\",\"type\":\"task\"}]}")
           (parsed (run-batch store json))
           (warnings (gethash "warnings" parsed)))
      (true (batch-result-ok-p parsed))
      (true (vectorp warnings))
      (is equal 2 (length warnings)))))

(define-test batch-no-warning-when-creates-are-linked
  :parent batch-suite
  "When every top-level ref is referenced by a link op, no warning fires."
  (beadwork:with-store (store ":memory:" :prefix "bd")
    (let* ((json "{\"operations\":[{\"op\":\"create\",\"ref\":\"child\",\"title\":\"Child\",\"type\":\"task\"},{\"op\":\"create\",\"ref\":\"parent\",\"title\":\"Parent\",\"type\":\"epic\"},{\"op\":\"link\",\"source\":{\"ref\":\"child\"},\"target\":{\"ref\":\"parent\"},\"relation\":\"parent-child\"}]}")
           (parsed (run-batch store json))
           (warnings (gethash "warnings" parsed)))
      (true (batch-result-ok-p parsed))
      (is equal 0 (length (or warnings #()))))))

(define-test batch-no-warning-when-create-has-children
  :parent batch-suite
  "A nested children[] create is not an orphaned top-level op."
  (beadwork:with-store (store ":memory:" :prefix "bd")
    (let* ((json "{\"operations\":[{\"op\":\"create\",\"ref\":\"epic\",\"title\":\"Epic\",\"type\":\"epic\",\"children\":[{\"op\":\"create\",\"ref\":\"c1\",\"title\":\"Child\",\"type\":\"task\"}]}]}")
           (parsed (run-batch store json))
           (warnings (gethash "warnings" parsed)))
      (true (batch-result-ok-p parsed))
      (is equal 0 (length (or warnings #()))))))

(define-test batch-link-fails-on-unknown-id
  :parent batch-suite
  "bd-wux: a link endpoint that is not an existing issue is rejected and not
stored (previously it became a dangling edge and broke ready/list)."
  (beadwork:with-store (store ":memory:" :prefix "bd")
    (let* ((a (beadwork:create-issue store :title "A" :type :task))
           (aid (beadwork:issue-id a))
           (json (format nil
                         "{\"operations\":[{\"op\":\"link\",\"source\":{\"id\":\"~A\"},\"target\":{\"id\":\"Decision D1 from the L0-L6 matrix\"},\"relation\":\"blocks\"}]}"
                         aid))
           (result (run-batch store json)))
      (false (batch-result-ok-p result))
      (is equal 0 (length (beadwork:list-dependencies store aid))))))
