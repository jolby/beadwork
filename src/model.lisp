(in-package :beadwork)

;;; ============================================================================
;;; Status
;;; ============================================================================

(defun status-string (status)
  "Convert a status keyword to its wire-format string."
  (ecase status
    (:open          "open")
    (:in-progress   "in_progress")
    (:blocked       "blocked")
    (:deferred      "deferred")
    (:closed        "closed")
    (:tombstone     "tombstone")))

(defun parse-status (string)
  "Parse a status string into a keyword. Signals an error on unknown values."
  (let ((s (string-downcase string)))
    (cond
      ((string= s "open")                        :open)
      ((or (string= s "in_progress")
           (string= s "inprogress"))              :in-progress)
      ((string= s "blocked")                      :blocked)
      ((string= s "deferred")                     :deferred)
      ((string= s "closed")                       :closed)
      ((string= s "tombstone")                    :tombstone)
      (t (error "Invalid status: ~A" string)))))

(defun status-terminal-p (status)
  "True when STATUS is a terminal state (:closed or :tombstone)."
  (member status '(:closed :tombstone) :test #'eq))

(defun status-active-p (status)
  "True when STATUS is an active state (:open or :in-progress)."
  (member status '(:open :in-progress) :test #'eq))

;;; ============================================================================
;;; Priority
;;; ============================================================================

(define-constant +priority-critical+ 0)
(define-constant +priority-high+     1)
(define-constant +priority-medium+   2)
(define-constant +priority-low+      3)
(define-constant +priority-backlog+  4)

(defun parse-priority (string)
  "Parse a priority string (\"P2\" or \"2\") into an integer 0-4."
  (let* ((s (string-upcase (string-trim '(#\Space #\Tab) string)))
         (digits (if (and (plusp (length s))
                          (char= (char s 0) #\P))
                     (subseq s 1)
                     s))
         (val (parse-integer digits :junk-allowed nil)))
    (unless (valid-priority-p val)
      (error "Priority out of range: ~A" val))
    val))

(defun format-priority (priority)
  "Format an integer priority as \"P2\" style string."
  (format nil "P~D" priority))

(defun valid-priority-p (priority)
  "True when PRIORITY is an integer in the range 0-4."
  (and (integerp priority) (<= 0 priority 4)))

;;; ============================================================================
;;; Issue Type
;;; ============================================================================

(defun issue-type-string (issue-type)
  "Convert an issue-type keyword to its wire-format string."
  (ecase issue-type
    (:task     "task")
    (:bug      "bug")
    (:feature  "feature")
    (:epic     "epic")
    (:chore    "chore")
    (:docs     "docs")
    (:question "question")))

(defun parse-issue-type (string)
  "Parse an issue-type string into a keyword."
  (let ((s (string-downcase string)))
    (cond
      ((string= s "task")     :task)
      ((string= s "bug")      :bug)
      ((string= s "feature")  :feature)
      ((string= s "epic")     :epic)
      ((string= s "chore")    :chore)
      ((string= s "docs")     :docs)
      ((string= s "question") :question)
      (t (error "Invalid issue type: ~A" string)))))

;;; ============================================================================
;;; Dependency Type
;;; ============================================================================

(defun dependency-type-string (dep-type)
  "Convert a dependency-type keyword to its wire-format string."
  (ecase dep-type
    (:blocks             "blocks")
    (:parent-child       "parent-child")
    (:conditional-blocks "conditional-blocks")
    (:waits-for          "waits-for")
    (:related            "related")
    (:discovered-from    "discovered-from")
    (:replies-to         "replies-to")
    (:relates-to         "relates-to")
    (:duplicates         "duplicates")
    (:supersedes         "supersedes")
    (:caused-by          "caused-by")))

(defun parse-dependency-type (string)
  "Parse a dependency-type string into a keyword."
  (let ((s (string-downcase string)))
    (cond
      ((string= s "blocks")             :blocks)
      ((string= s "parent-child")       :parent-child)
      ((string= s "conditional-blocks") :conditional-blocks)
      ((string= s "waits-for")          :waits-for)
      ((string= s "related")            :related)
      ((string= s "discovered-from")    :discovered-from)
      ((string= s "replies-to")         :replies-to)
      ((string= s "relates-to")         :relates-to)
      ((string= s "duplicates")         :duplicates)
      ((string= s "supersedes")         :supersedes)
      ((string= s "caused-by")          :caused-by)
      (t (error "Invalid dependency type: ~A" string)))))

(defun dependency-blocking-p (dep-type)
  "True when DEP-TYPE is a blocking dependency that affects ready-work."
  (member dep-type '(:blocks :parent-child :conditional-blocks :waits-for)
          :test #'eq))

;;; ============================================================================
;;; Issue
;;; ============================================================================

(defclass issue ()
  ((id
    :initarg :id
    :accessor issue-id)
   (content-hash
    :initarg :content-hash
    :accessor issue-content-hash
    :initform nil)
   (title
    :initarg :title
    :accessor issue-title)
   (description
    :initarg :description
    :accessor issue-description
    :initform "")
   (design
    :initarg :design
    :accessor issue-design
    :initform "")
   (acceptance-criteria
    :initarg :acceptance-criteria
    :accessor issue-acceptance-criteria
    :initform "")
   (notes
    :initarg :notes
    :accessor issue-notes
    :initform "")
   (status
    :initarg :status
    :accessor issue-status
    :initform :open)
   (priority
    :initarg :priority
    :accessor issue-priority
    :initform 2)
   (issue-type
    :initarg :issue-type
    :accessor issue-type
    :initform :task)
   (assignee
    :initarg :assignee
    :accessor issue-assignee
    :initform nil)
   (owner
    :initarg :owner
    :accessor issue-owner
    :initform "")
   (estimated-minutes
    :initarg :estimated-minutes
    :accessor issue-estimated-minutes
    :initform nil)
   (created-at
    :initarg :created-at
    :accessor issue-created-at)
   (created-by
    :initarg :created-by
    :accessor issue-created-by
    :initform "")
   (updated-at
    :initarg :updated-at
    :accessor issue-updated-at)
   (closed-at
    :initarg :closed-at
    :accessor issue-closed-at
    :initform nil)
   (close-reason
    :initarg :close-reason
    :accessor issue-close-reason
    :initform "")
   (source-repo
    :initarg :source-repo
    :accessor issue-source-repo
    :initform ".")
   (external-ref
    :initarg :external-ref
    :accessor issue-external-ref
    :initform nil)
   (labels
    :initarg :labels
    :accessor issue-labels
    :initform nil)
   (dependencies
    :initarg :dependencies
    :accessor issue-dependencies
    :initform nil)
   (comments
    :initarg :comments
    :accessor issue-comments
    :initform nil))
  (:default-initargs
   :created-at (local-time:now)
   :updated-at (local-time:now)))

;;; ============================================================================
;;; Dependency
;;; ============================================================================

(defclass dependency ()
  ((issue-id
    :initarg :issue-id
    :accessor dependency-issue-id)
   (depends-on-id
    :initarg :depends-on-id
    :accessor dependency-depends-on-id)
   (dep-type
    :initarg :dep-type
    :accessor dependency-dep-type
    :initform :blocks)
   (created-at
    :initarg :created-at
    :accessor dependency-created-at)
   (created-by
    :initarg :created-by
    :accessor dependency-created-by
    :initform nil)
   (metadata
    :initarg :metadata
    :accessor dependency-metadata
    :initform nil)
   (thread-id
    :initarg :thread-id
    :accessor dependency-thread-id
    :initform nil))
  (:default-initargs
   :created-at (local-time:now)))

;;; ============================================================================
;;; Comment
;;; ============================================================================

(defclass comment ()
  ((id
    :initarg :id
    :accessor comment-id)
   (issue-id
    :initarg :issue-id
    :accessor comment-issue-id)
   (author
    :initarg :author
    :accessor comment-author)
   (body
    :initarg :body
    :accessor comment-body)
   (created-at
    :initarg :created-at
    :accessor comment-created-at))
  (:default-initargs
   :created-at (local-time:now)))

;;; ============================================================================
;;; Event
;;; ============================================================================

(defclass event ()
  ((id
    :initarg :id
    :accessor event-id)
   (issue-id
    :initarg :issue-id
    :accessor event-issue-id)
   (event-type
    :initarg :event-type
    :accessor event-event-type)
   (actor
    :initarg :actor
    :accessor event-actor)
   (old-value
    :initarg :old-value
    :accessor event-old-value
    :initform nil)
   (new-value
    :initarg :new-value
    :accessor event-new-value
    :initform nil)
   (comment
    :initarg :comment
    :accessor event-comment
    :initform nil)
   (created-at
    :initarg :created-at
    :accessor event-created-at))
  (:default-initargs
   :created-at (local-time:now)))

;;; ============================================================================
;;; Content Hash
;;;
;;; Matches br's content_hash algorithm: SHA-256 of null-separated fields in
;;; stable order.  Fields included: title, description, design,
;;; acceptance-criteria, notes, status, priority, issue-type, assignee, owner,
;;; created-by, external-ref.  Excludes: id, timestamps, relations.
;;; ============================================================================

(defun %hash-field (digester value)
  "Update DIGESTER with VALUE (string) followed by a null byte separator.
Null bytes within VALUE are replaced with spaces to match br behavior."
  (let ((clean (substitute #\Space #\Nul value)))
    (ironclad:update-digest digester
                           (babel:string-to-octets clean :encoding :utf-8)))
  (ironclad:update-digest digester
                          (make-array 1 :element-type '(unsigned-byte 8)
                                        :initial-element 0)))

(defun %hash-field-opt (digester value)
  "Update DIGESTER with VALUE when non-nil/non-empty, else empty string."
  (%hash-field digester (if (and value (plusp (length value))) value "")))

(defgeneric compute-content-hash (issue)
  (:documentation
   "Compute the SHA-256 content hash for ISSUE.  The hash covers content fields
only (title, description, design, acceptance-criteria, notes, status, priority,
issue-type, assignee, owner, created-by, external-ref) and excludes id,
timestamps, and relations — matching br behavior."))

(defmethod compute-content-hash ((issue issue))
  (let ((digester (ironclad:make-digest :sha256)))
    (%hash-field     digester (issue-title issue))
    (%hash-field-opt digester (issue-description issue))
    (%hash-field-opt digester (issue-design issue))
    (%hash-field-opt digester (issue-acceptance-criteria issue))
    (%hash-field-opt digester (issue-notes issue))
    (%hash-field     digester (status-string (issue-status issue)))
    (%hash-field     digester (format-priority (issue-priority issue)))
    (%hash-field     digester (issue-type-string (issue-type issue)))
    (%hash-field-opt digester (issue-assignee issue))
    (%hash-field-opt digester (let ((o (issue-owner issue)))
                                (if (string= o "") nil o)))
    (%hash-field-opt digester (let ((c (issue-created-by issue)))
                                (if (string= c "") nil c)))
    (%hash-field-opt digester (issue-external-ref issue))
    ;; source-system not tracked on CL issue class; hash as empty for compat
    (%hash-field-opt digester nil)
    ;; pinned / is-template not tracked; hash as false for compat
    (%hash-field     digester "false")
    (%hash-field     digester "false")
    (ironclad:byte-array-to-hex-string (ironclad:produce-digest digester))))
