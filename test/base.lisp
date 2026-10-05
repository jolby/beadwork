(in-package #:beadwork/tests)

;;; Base Beadwork Test Suite

;;; Run tests with: (asdf:test-system "beadwork")

(define-test comment-edit-updates-text
  :parent beadwork-suite
  (beadwork:with-store (store ":memory:" :prefix "bd")
    (let* ((issue (beadwork:create-issue store :title "Comment test" :type :task))
           (issue-id (beadwork:issue-id issue)))
      (beadwork:add-comment store issue-id "test" "original text")
      (let* ((comments (beadwork:list-comments store issue-id))
             (comment-id (beadwork::comment-id (first comments))))
        (beadwork::edit-comment store comment-id "updated text")
        (let ((updated (beadwork:list-comments store issue-id)))
          (is equal 1 (length updated))
          (is equal "updated text" (beadwork::comment-body (first updated))))))))

(define-test comment-edit-preserves-author
  :parent beadwork-suite
  (beadwork:with-store (store ":memory:" :prefix "bd")
    (let* ((issue (beadwork:create-issue store :title "Author test" :type :task))
           (issue-id (beadwork:issue-id issue)))
      (beadwork:add-comment store issue-id "agent-7" "original")
      (let* ((comments (beadwork:list-comments store issue-id))
             (comment-id (beadwork::comment-id (first comments))))
        (beadwork::edit-comment store comment-id "new text")
        (let ((updated (beadwork:list-comments store issue-id)))
          (is equal "agent-7" (beadwork::comment-author (first updated))))))))

(define-test comment-edit-fails-on-nonexistent
  :parent beadwork-suite
  (beadwork:with-store (store ":memory:" :prefix "bd")
    (fail (beadwork::edit-comment store 99999 "new text")
          'beadwork:beadwork-error)))

(define-test comment-delete-removes-comment
  :parent beadwork-suite
  (beadwork:with-store (store ":memory:" :prefix "bd")
    (let* ((issue (beadwork:create-issue store :title "Delete test" :type :task))
           (issue-id (beadwork:issue-id issue)))
      (beadwork:add-comment store issue-id "test" "to be deleted")
      (beadwork:add-comment store issue-id "test" "to keep")
      (let* ((comments (beadwork:list-comments store issue-id))
             (first-id (beadwork::comment-id (first comments))))
        (is equal 2 (length comments))
        (beadwork::delete-comment store first-id)
        (let ((remaining (beadwork:list-comments store issue-id)))
          (is equal 1 (length remaining))
          (is equal "to keep" (beadwork::comment-body (first remaining))))))))

(define-test comment-delete-noop-on-nonexistent
  :parent beadwork-suite
  (beadwork:with-store (store ":memory:" :prefix "bd")
    ;; Deleting a non-existent comment should not error
    (finish (beadwork::delete-comment store 99999))))

(define-test comment-edit-then-delete
  :parent beadwork-suite
  (beadwork:with-store (store ":memory:" :prefix "bd")
    (let* ((issue (beadwork:create-issue store :title "Edit+delete" :type :task))
           (issue-id (beadwork:issue-id issue)))
      (beadwork:add-comment store issue-id "test" "interim text")
      (let* ((comments (beadwork:list-comments store issue-id))
             (comment-id (beadwork::comment-id (first comments))))
        (beadwork::edit-comment store comment-id "edited text")
        (beadwork::delete-comment store comment-id)
        (is equal 0 (length (beadwork:list-comments store issue-id)))))))

(define-test update-issue-to-closed-stamps-closed-at
  :parent beadwork-suite
  "update-issue with :status :closed must set closed_at to satisfy the schema
CHECK constraint (bd-lu5)."
  (beadwork:with-store (store ":memory:" :prefix "bd")
    (let* ((issue (beadwork:create-issue store :title "Close me" :type :task))
           (id (beadwork:issue-id issue)))
      (beadwork:update-issue store id :status :closed)
      (let ((updated (beadwork:get-issue store id)))
        (is eq :closed (beadwork:issue-status updated))
        (true (beadwork:issue-closed-at updated)
              "closing via update-issue must stamp closed_at")))))

(define-test update-issue-from-closed-clears-closed-at
  :parent beadwork-suite
  "update-issue moving out of :closed must clear closed_at to satisfy the
schema CHECK constraint (bd-lu5)."
  (beadwork:with-store (store ":memory:" :prefix "bd")
    (let* ((issue (beadwork:create-issue store :title "Reopen me" :type :task))
           (id (beadwork:issue-id issue)))
      (beadwork:close-issue store id :reason "done")
      (true (beadwork:issue-closed-at (beadwork:get-issue store id)))
      (beadwork:update-issue store id :status :in-progress)
      (let ((updated (beadwork:get-issue store id)))
        (is eq :in-progress (beadwork:issue-status updated))
        (is eq nil (beadwork:issue-closed-at updated)
            "moving out of closed must clear closed_at")))))

(define-test create-issue-returns-source-repo
  :parent beadwork-suite
  "bd-x50: create-issue must return an issue whose source-repo slot matches
what was persisted (the slot default is '.'), so CLI JSON output is not
misleading."
  (beadwork:with-store (store ":memory:" :prefix "bd")
    (let* ((issue (beadwork:create-issue store :title "Repo attribution" :type :task
                                         :source-repo "cogen-source-code-tools"))
           (id (beadwork:issue-id issue)))
      (is equal "cogen-source-code-tools" (beadwork:issue-source-repo issue))
      (is equal "cogen-source-code-tools"
               (beadwork:issue-source-repo (beadwork:get-issue store id))))))

;;; ---------------------------------------------------------------------------
;;; Graph neighbors (bd-uz3)
;;; ---------------------------------------------------------------------------

(define-test get-parent-id-returns-parent
  :parent beadwork-suite
  "get-parent-id follows the parent-child dependency to the parent id."
  (beadwork:with-store (store ":memory:" :prefix "bd")
    (let* ((epic (beadwork:create-issue store :title "Epic" :type :epic))
           (child (beadwork:create-issue store :title "Child" :type :task
                                         :parent (beadwork:issue-id epic))))
      (is equal (beadwork:issue-id epic)
               (beadwork::get-parent-id store (beadwork:issue-id child)))
      (is eq nil (beadwork::get-parent-id store (beadwork:issue-id epic))))))

(define-test list-children-returns-direct-children
  :parent beadwork-suite
  "list-children returns the direct children of an issue, not grandchildren."
  (beadwork:with-store (store ":memory:" :prefix "bd")
    (let* ((epic (beadwork:create-issue store :title "Epic" :type :epic))
           (epic-id (beadwork:issue-id epic))
           (c1 (beadwork:create-issue store :title "Child one" :type :task
                                       :parent epic-id))
           (c2 (beadwork:create-issue store :title "Child two" :type :task
                                       :parent epic-id)))
      (let ((children (beadwork::list-children store epic-id)))
        (is equal 2 (length children))
        (true (find (beadwork:issue-id c1) children
                    :key #'beadwork:issue-id :test #'equal))
        (true (find (beadwork:issue-id c2) children
                    :key #'beadwork:issue-id :test #'equal))))))

(define-test list-dependents-returns-incoming-edges
  :parent beadwork-suite
  "list-dependents returns the dependencies pointing AT an issue (the
issues that depend on it), mirroring list-dependencies (outgoing)."
  (beadwork:with-store (store ":memory:" :prefix "bd")
    (let* ((a (beadwork:create-issue store :title "A" :type :task))
           (b (beadwork:create-issue store :title "B" :type :task)))
      (beadwork:add-dependency store (beadwork:issue-id b) (beadwork:issue-id a)
                               :type :blocks)
      (let ((dependents (beadwork::list-dependents store (beadwork:issue-id a))))
        (is equal 1 (length dependents))
        (is equal (beadwork:issue-id b)
                 (beadwork:dependency-issue-id (first dependents)))
        (is equal :blocks (beadwork:dependency-dep-type (first dependents)))))))

;;; ---------------------------------------------------------------------------
;;; Dependency endpoint validation + dangling-edge robustness (bd-wux)
;;; ---------------------------------------------------------------------------

(defun plant-dependency-row (store issue-id depends-on-id type)
  "Insert a dependency row directly, bypassing validation and foreign keys, to
simulate a legacy dangling edge (bd-wux)."
  (let ((db (beadwork:store-db store)))
    (sqlite:execute-non-query db "PRAGMA foreign_keys=OFF")
    (sqlite:execute-non-query db
     "INSERT INTO dependencies (issue_id, depends_on_id, type, created_at, created_by)
      VALUES (?, ?, ?, '2026-01-01T00:00:00Z', '')"
     issue-id depends-on-id type)
    (sqlite:execute-non-query db "PRAGMA foreign_keys=ON")))

(define-test add-dependency-rejects-unknown-target
  :parent beadwork-suite
  "bd-wux: a pasted description (or any unknown id) must not become an edge."
  (beadwork:with-store (store ":memory:" :prefix "bd")
    (let* ((a (beadwork:create-issue store :title "A" :type :task))
           (aid (beadwork:issue-id a)))
      (fail (beadwork:add-dependency store aid
                                     "Decision D1 from the L0-L6 matrix")
            'beadwork:issue-not-found)
      (is equal 0 (length (beadwork:list-dependencies store aid))))))

(define-test add-dependency-rejects-unknown-source
  :parent beadwork-suite
  (beadwork:with-store (store ":memory:" :prefix "bd")
    (let* ((a (beadwork:create-issue store :title "A" :type :task))
           (aid (beadwork:issue-id a)))
      (fail (beadwork:add-dependency store "bd-missing" aid)
            'beadwork:issue-not-found)
      (is equal 0 (length (beadwork:list-dependents store aid))))))

(define-test find-issue-returns-nil-when-unknown
  :parent beadwork-suite
  (beadwork:with-store (store ":memory:" :prefix "bd")
    (is eq nil (beadwork:find-issue store "bd-missing"))
    (let ((a (beadwork:create-issue store :title "A" :type :task)))
      (is equal (beadwork:issue-id a)
                (beadwork:issue-id
                 (beadwork:find-issue store (beadwork:issue-id a)))))))

(define-test issue-graph-render-survives-dangling-dependency
  :parent beadwork-suite
  "bd-wux: a legacy edge whose target is not an issue must not abort graph
rendering (bw list/show/ready render neighbors)."
  (beadwork:with-store (store ":memory:" :prefix "bd")
    (let ((a (beadwork:create-issue store :title "A" :type :task)))
      (plant-dependency-row store (beadwork:issue-id a)
                            "prose that is not an issue id" "blocks")
      ;; Must not signal; the dangling edge is simply not rendered.
      (let ((json (beadwork::issue->cli-json a store)))
        (is equal 0 (length (gethash "dependencies" json)))))))

(define-test list-children-skips-dangling-child
  :parent beadwork-suite
  "bd-wux: a parent-child edge to a non-existent child is skipped, not fatal."
  (beadwork:with-store (store ":memory:" :prefix "bd")
    (let* ((p (beadwork:create-issue store :title "P" :type :epic))
           (pid (beadwork:issue-id p)))
      (plant-dependency-row store "bd-ghost-child" pid "parent-child")
      (is equal 0 (length (beadwork:list-children store pid))))))
