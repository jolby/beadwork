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

(define-test sync-export-idempotent-with-legacy-dependency-created-at
  :parent beadwork-suite
  "export-jsonl must be a pure function of the DB: a dependency created_at
stored in br's legacy space-format emits identically on every run (bd-e84)."
  (beadwork:with-store (store ":memory:" :prefix "bd")
    (let* ((a (beadwork:create-issue store :title "A" :type :task))
           (b (beadwork:create-issue store :title "B" :type :task)))
      (beadwork:add-dependency store (beadwork:issue-id a) (beadwork:issue-id b))
      ;; Simulate legacy br data: space-separated UTC timestamp, no offset.
      (sqlite-compat:execute-non-query (beadwork::store-db store)
        "UPDATE dependencies SET created_at = ?"
        "2026-02-09 22:07:38")
      (let ((runs
              (loop repeat 2
                    collect (uiop:with-temporary-file (:pathname path)
                              (beadwork::export-jsonl store path)
                              (uiop:read-file-string path)))))
        (is equal (first runs) (second runs)
            "repeated exports must be byte-identical")
        (true (search "\"created_at\":\"2026-02-09T22:07:38.000000+00:00\""
                      (first runs))
              "dependency created_at must be the stored legacy timestamp as UTC, got ~A"
              (first runs))))))

(define-test normalize-timestamps-covers-dependencies
  :parent beadwork-suite
  "%normalize-timestamps migrates legacy dependency created_at to RFC3339 UTC
on store open, matching the issues-table handling (bd-e84)."
  (let* ((dir (uiop:ensure-directory-pathname (namestring (uiop:temporary-directory))))
         (db-path (format nil "~Abeadwork-norm-deps-~D.db" (namestring dir)
                          (random 1000000))))
    (unwind-protect
         (progn
           ;; Seed a DB with a legacy-format dependency created_at, then close.
           (beadwork:with-store (s1 db-path :prefix "bd")
             (let ((a (beadwork:create-issue s1 :title "A" :type :task))
                   (b (beadwork:create-issue s1 :title "B" :type :task)))
               (beadwork:add-dependency s1 (beadwork:issue-id a) (beadwork:issue-id b))
               (sqlite-compat:execute-non-query (beadwork::store-db s1)
                 "UPDATE dependencies SET created_at = ?"
                 "2026-02-09 22:07:38")))
           ;; Re-open: %normalize-timestamps must rewrite the row to RFC3339 UTC.
           (beadwork:with-store (s2 db-path :prefix "bd")
             (let ((rows (sqlite-compat:execute-to-list (beadwork::store-db s2)
                           "SELECT created_at FROM dependencies")))
               (is equal "2026-02-09T22:07:38.000000+00:00"
                   (caar rows)
                   "dependencies.created_at must be normalized to RFC3339 UTC"))))
      (when (probe-file db-path)
        (delete-file db-path)))))
