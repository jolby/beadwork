(in-package #:beadwork/tests)

;;; Sync Tests — JSONL import/export correctness

(define-test sync-issue-to-json-handles-open-issue
  :parent beadwork-suite
  "issue-to-json-object does not crash when issue has no closed-at timestamp"
  (let ((issue (make-instance 'beadwork::issue
                              :id "bd-test"
                              :title "open issue"
                              :status :open
                              :priority 2
                              :issue-type :task)))
    ;; Must not signal — open issues have NIL closed-at
    (finish (beadwork::issue-to-json-object issue))))

(define-test sync-export-issue-to-json-excludes-nil-closed-at
  :parent beadwork-suite
  "issue-to-json-object omits closed_at key for open issues"
  (let* ((issue (make-instance 'beadwork::issue
                               :id "bd-test2"
                               :title "open issue"
                               :status :open
                               :priority 2
                               :issue-type :task))
         (ht (beadwork::issue-to-json-object issue)))
    (is equal nil (gethash "closed_at" ht))))
