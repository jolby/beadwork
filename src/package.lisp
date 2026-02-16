(defpackage #:beadwork
  (:use #:cl)
  (:local-nicknames
   (#:jzon #:com.inuoe.jzon))
  (:import-from #:alexandria
                #:define-constant
                #:when-let
                #:if-let)
  (:export
   ;; Status enum equivalents
   #:status-open
   #:status-in-progress
   #:status-blocked
   #:status-deferred
   #:status-closed
   #:status-tombstone
   #:status-string
   #:parse-status
   #:status-terminal-p
   #:status-active-p

   ;; Priority
   #:priority-critical
   #:priority-high
   #:priority-medium
   #:priority-low
   #:priority-backlog
   #:priority-value
   #:parse-priority
   #:format-priority

   ;; Issue types
   #:issue-type-string
   #:parse-issue-type

   ;; Dependency types
   #:dependency-type-string
   #:parse-dependency-type
   #:dependency-blocking-p

   ;; Core model classes
   #:issue
   #:dependency
   #:comment
   #:event

   ;; Issue accessors (slot names)
   #:issue-id
   #:issue-title
   #:issue-description
   #:issue-status
   #:issue-priority
   #:issue-type
   #:issue-assignee
   #:issue-created-at
   #:issue-updated-at
   #:issue-closed-at
   #:issue-close-reason
   #:issue-labels
   #:issue-dependencies
   #:issue-content-hash
   #:issue-parent-id
   #:issue-source-repo

   ;; Storage protocol
   #:open-store
   #:close-store
   #:with-store
   #:create-issue
   #:get-issue
   #:update-issue
   #:close-issue
   #:reopen-issue
   #:delete-issue
   #:list-issues
   #:ready-issues
   #:blocked-issues
   #:search-issues
   #:add-dependency
   #:remove-dependency
   #:list-dependencies
   #:add-label
   #:remove-label
   #:get-labels
   #:list-all-labels
   #:add-comment

   ;; Dependency accessors
   #:dependency-issue-id
   #:dependency-depends-on-id
   #:dependency-dep-type
   #:list-comments

   ;; Dirty tracking
   #:mark-dirty
   #:get-dirty-issues
   #:clear-dirty

   ;; Store class
   #:store
   #:store-db
   #:store-db-path
   #:store-prefix

   ;; Sync
   #:export-jsonl
   #:import-jsonl
   #:sync-store
   #:format-timestamp
   #:format-timestamp-utc

   ;; ID generation
   #:generate-id
   #:generate-child-id
   #:base36-encode
   #:compute-content-hash

   ;; CLI entry
   #:main

   ;; Conditions
   #:beadwork-error
   #:issue-not-found
   #:invalid-status
   #:invalid-priority
   #:duplicate-issue))
