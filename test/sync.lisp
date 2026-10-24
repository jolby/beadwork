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

(define-test export-reports-unparseable-timestamps
  :parent beadwork-suite
  "bd-1j9: a timestamp bw cannot parse is exported without a value; say so
instead of silently dropping it."
  (beadwork:with-store (store ":memory:" :prefix "bd")
    (let* ((issue (beadwork:create-issue store :title "Bad stamp" :type :task
                                         :source-repo "cogen-meta"))
           (id (beadwork:issue-id issue))
           (path (uiop:with-temporary-file (:pathname p :type "jsonl")
                     (namestring p))))
      (sqlite:execute-non-query
       (beadwork::store-db store)
       "UPDATE issues SET closed_at = ?, status = 'closed' WHERE id = ?"
       "2026-06-06T12:29:19.1247400700" id)
      (multiple-value-bind (exported problems)
          (beadwork::export-jsonl store path)
        (declare (ignore exported))
        (is = 1 (length problems))
        (is equal id (first (first problems)))
        (is equal "closed_at" (second (first problems))))
      (let* ((line (with-open-file (in path) (read-line in)))
             (ht (com.inuoe.jzon:parse line)))
        (multiple-value-bind (value present) (gethash "closed_at" ht)
          ;; issue-to-json-object omits nil timestamps, so the field is either
          ;; absent or JSON null -- either way the value is gone.
          (true (or (not present) (eq 'null value))))))))

;;; ---------------------------------------------------------------------------
;;; Comments in JSONL (bd-pkv)
;;; ---------------------------------------------------------------------------

(define-test sync-export-includes-comments
  :parent beadwork-suite
  "bd-pkv: comments are durable evidence, so they must travel in issues.jsonl
as the same comments array br uses."
  (beadwork:with-store (store ":memory:" :prefix "bd")
    (let* ((issue (beadwork:create-issue store :title "Comment export" :type :task))
           (id (beadwork:issue-id issue))
           (path (uiop:with-temporary-file (:pathname p :type "jsonl")
                     (namestring p))))
      (beadwork:add-comment store id "agent-7" "measured evidence")
      (beadwork::export-jsonl store path)
      (let* ((line (with-open-file (in path) (read-line in)))
             (ht (com.inuoe.jzon:parse line))
             (comments (gethash "comments" ht)))
        (is = 1 (length comments))
        (let ((comment (aref comments 0)))
          (is equal id (gethash "issue_id" comment))
          (is equal "agent-7" (gethash "author" comment))
          (is equal "measured evidence" (gethash "text" comment))
          (true (gethash "created_at" comment)))))))

(define-test sync-export-omits-empty-comments
  :parent beadwork-suite
  "An issue with no comments keeps the key out of JSONL, matching br's
skip_serializing_if behavior."
  (beadwork:with-store (store ":memory:" :prefix "bd")
    (let* ((issue (beadwork:create-issue store :title "No comments" :type :task))
           (path (uiop:with-temporary-file (:pathname p :type "jsonl")
                     (namestring p))))
      (declare (ignore issue))
      (beadwork::export-jsonl store path)
      (let* ((line (with-open-file (in path) (read-line in)))
             (ht (com.inuoe.jzon:parse line)))
        (is eq nil (nth-value 1 (gethash "comments" ht)))))))

(define-test sync-import-restores-comments
  :parent beadwork-suite
  "A fresh clone can reconstruct comments from issues.jsonl."
  (beadwork:with-store (source ":memory:" :prefix "bd")
    (let* ((issue (beadwork:create-issue source :title "Round trip" :type :task))
           (id (beadwork:issue-id issue))
           (path (uiop:with-temporary-file (:pathname p :type "jsonl")
                     (namestring p))))
      (beadwork:add-comment source id "agent-7" "survives the clone")
      (beadwork::export-jsonl source path)
      (beadwork:with-store (dest ":memory:" :prefix "bd")
        (beadwork::import-jsonl dest path)
        (let ((comments (beadwork:list-comments dest id)))
          (is = 1 (length comments))
          (let ((comment (first comments)))
            (is equal "agent-7" (beadwork::comment-author comment))
            (is equal "survives the clone" (beadwork::comment-body comment))))))))

(define-test sync-import-comments-is-idempotent
  :parent beadwork-suite
  "Importing the same JSONL twice must not duplicate comments."
  (beadwork:with-store (source ":memory:" :prefix "bd")
    (let* ((issue (beadwork:create-issue source :title "Idempotent" :type :task))
           (id (beadwork:issue-id issue))
           (path (uiop:with-temporary-file (:pathname p :type "jsonl")
                     (namestring p))))
      (beadwork:add-comment source id "agent-7" "once")
      (beadwork::export-jsonl source path)
      (beadwork:with-store (dest ":memory:" :prefix "bd")
        (beadwork::import-jsonl dest path)
        (beadwork::import-jsonl dest path)
        (is = 1 (length (beadwork:list-comments dest id)))))))

(define-test sync-import-comment-id-collision-does-not-clobber
  :parent beadwork-suite
  "An id already owned by another issue must not overwrite that comment."
  (beadwork:with-store (source ":memory:" :prefix "bd")
    (let* ((issue (beadwork:create-issue source :title "Source issue" :type :task))
           (id (beadwork:issue-id issue))
           (path (uiop:with-temporary-file (:pathname p :type "jsonl")
                     (namestring p))))
      (beadwork:add-comment source id "source-author" "source comment")
      (beadwork::export-jsonl source path)
      (beadwork:with-store (dest ":memory:" :prefix "bd")
        (let* ((local (beadwork:create-issue dest :title "Local issue" :type :task))
               (local-id (beadwork:issue-id local)))
          (beadwork:add-comment dest local-id "local-author" "local comment")
          (beadwork::import-jsonl dest path)
          (beadwork::import-jsonl dest path)
          (let ((local-comments (beadwork:list-comments dest local-id))
                (source-comments (beadwork:list-comments dest id)))
            (is = 1 (length local-comments))
            (is equal "local comment"
                    (beadwork::comment-body (first local-comments)))
            (is = 1 (length source-comments))
            (is equal "source comment"
                    (beadwork::comment-body (first source-comments)))))))))
