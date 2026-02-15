(in-package :beadwork)

(define-condition beadwork-error (error)
  ((message :initarg :message
            :reader beadwork-error-message
            :type string))
  (:report (lambda (condition stream)
             (format stream "Beadwork error: ~A"
                     (beadwork-error-message condition)))))

(define-condition issue-not-found (beadwork-error)
  ((issue-id :initarg :issue-id
             :reader issue-not-found-issue-id
             :type string))
  (:report (lambda (condition stream)
             (format stream "Issue not found: ~A"
                     (issue-not-found-issue-id condition)))))

(define-condition invalid-status (beadwork-error)
  ((status :initarg :status
           :reader invalid-status-status
           :type string))
  (:report (lambda (condition stream)
             (format stream "Invalid status: ~S"
                     (invalid-status-status condition)))))

(define-condition invalid-priority (beadwork-error)
  ((priority :initarg :priority
             :reader invalid-priority-priority))
  (:report (lambda (condition stream)
             (format stream "Invalid priority: ~S"
                     (invalid-priority-priority condition)))))

(define-condition invalid-issue-type (beadwork-error)
  ((issue-type :initarg :issue-type
               :reader invalid-issue-type-issue-type
               :type string))
  (:report (lambda (condition stream)
             (format stream "Invalid issue type: ~S"
                     (invalid-issue-type-issue-type condition)))))

(define-condition duplicate-issue (beadwork-error)
  ((issue-id :initarg :issue-id
             :reader duplicate-issue-issue-id
             :type string))
  (:report (lambda (condition stream)
             (format stream "Duplicate issue: ~A"
                     (duplicate-issue-issue-id condition)))))

(define-condition storage-error (beadwork-error)
  ((operation :initarg :operation
              :reader storage-error-operation
              :type string))
  (:report (lambda (condition stream)
             (format stream "Storage error during ~A: ~A"
                     (storage-error-operation condition)
                     (beadwork-error-message condition)))))

(define-condition sync-error (beadwork-error)
  ((path :initarg :path
         :reader sync-error-path
         :type pathname))
  (:report (lambda (condition stream)
             (format stream "Sync error for ~A: ~A"
                     (sync-error-path condition)
                     (beadwork-error-message condition)))))
