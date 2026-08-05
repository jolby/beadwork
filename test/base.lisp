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
