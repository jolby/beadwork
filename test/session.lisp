(in-package #:beadwork/tests)

;;; Session list/show tests (bd-8q6)

(define-test session-suite
  :parent beadwork-suite
  :description "Tests for bw session list/show -- enumerate and inspect session history")

;;; ---------------------------------------------------------------------------
;;; Helpers
;;; ---------------------------------------------------------------------------

(defun plant-session (store id started-at &key ended-at active-issue-id agent-id)
  "Insert a session row directly with a controlled started_at, for ordering
and filter tests."
  (sqlite:execute-non-query
   (beadwork::store-db store)
   "INSERT INTO sessions (id, started_at, ended_at, active_issue_id,
                          handoff_notes, last_action, agent_id, agent_session_id)
    VALUES (?, ?, ?, ?, '', '', ?, '')"
   id started-at ended-at active-issue-id (or agent-id "")))

;;; ============================================================================
;;; Storage: get-session
;;; ============================================================================

(define-test get-session-returns-plist
  :parent session-suite
  (beadwork:with-store (store ":memory:" :prefix "bd")
    (let ((issue (beadwork:create-issue store :title "Worked" :type :task)))
      (plant-session store "S-one" "2026-10-01T10:00:00-07:00"
                     :ended-at "2026-10-01T11:00:00-07:00"
                     :active-issue-id (beadwork:issue-id issue)
                     :agent-id "worker")
      (let ((s (beadwork::get-session store "S-one")))
        (true s)
        (is equal "S-one" (getf s :id))
        (is equal (beadwork:issue-id issue) (getf s :active-issue-id))
        (is equal "worker" (getf s :agent-id))
        (true (getf s :ended-at))
        (true (getf s :started-at))))))

(define-test get-session-returns-nil-when-unknown
  :parent session-suite
  (beadwork:with-store (store ":memory:" :prefix "bd")
    (is eq nil (beadwork::get-session store "S-nope"))))

;;; ============================================================================
;;; Storage: list-sessions
;;; ============================================================================

(define-test list-sessions-newest-first
  :parent session-suite
  (beadwork:with-store (store ":memory:" :prefix "bd")
    (plant-session store "S-old" "2026-09-01T10:00:00-07:00")
    (plant-session store "S-mid" "2026-10-01T10:00:00-07:00")
    (plant-session store "S-new" "2026-10-05T10:00:00-07:00")
    (is equal '("S-new" "S-mid" "S-old")
             (mapcar (lambda (s) (getf s :id))
                     (beadwork::list-sessions store)))))

(define-test list-sessions-honors-limit
  :parent session-suite
  (beadwork:with-store (store ":memory:" :prefix "bd")
    (dotimes (i 5)
      (plant-session store (format nil "S-~D" i)
                     (format nil "2026-10-0~DT10:00:00-07:00" (1+ i))))
    (is equal 2 (length (beadwork::list-sessions store :limit 2)))
    (is equal '("S-4" "S-3")
             (mapcar (lambda (s) (getf s :id))
                     (beadwork::list-sessions store :limit 2)))))

(define-test list-sessions-active-filter
  :parent session-suite
  (beadwork:with-store (store ":memory:" :prefix "bd")
    (plant-session store "S-active" "2026-10-05T10:00:00-07:00")
    (plant-session store "S-done" "2026-10-04T10:00:00-07:00"
                   :ended-at "2026-10-04T11:00:00-07:00")
    (is equal '("S-active")
             (mapcar (lambda (s) (getf s :id))
                     (beadwork::list-sessions store :active t)))))

(define-test list-sessions-agent-filter
  :parent session-suite
  (beadwork:with-store (store ":memory:" :prefix "bd")
    (plant-session store "S-a" "2026-10-05T10:00:00-07:00" :agent-id "worker")
    (plant-session store "S-b" "2026-10-04T10:00:00-07:00" :agent-id "scout")
    (is equal '("S-a")
             (mapcar (lambda (s) (getf s :id))
                     (beadwork::list-sessions store :agent-id "worker")))))

(define-test list-sessions-issue-filter
  :parent session-suite
  (beadwork:with-store (store ":memory:" :prefix "bd")
    (let ((a (beadwork:create-issue store :title "A" :type :task))
          (b (beadwork:create-issue store :title "B" :type :task)))
      (plant-session store "S-a" "2026-10-05T10:00:00-07:00"
                     :active-issue-id (beadwork:issue-id a))
      (plant-session store "S-b" "2026-10-04T10:00:00-07:00"
                     :active-issue-id (beadwork:issue-id b))
      (is equal '("S-a")
               (mapcar (lambda (s) (getf s :id))
                       (beadwork::list-sessions store :issue-id (beadwork:issue-id a)))))))

;;; ============================================================================
;;; CLI helpers: state / duration / JSON
;;; ============================================================================

(define-test session-state-classifies-rows
  :parent session-suite
  (let ((now (local-time:now)))
    (is eq :ended (beadwork::%session-state
                   (list :started-at (local-time:timestamp- now 1 :hour)
                         :ended-at (local-time:timestamp- now 30 :minute))))
    (is eq :active (beadwork::%session-state
                    (list :started-at (local-time:timestamp- now 1 :hour)
                          :ended-at nil)))
    (is eq :stale (beadwork::%session-state
                   (list :started-at (local-time:timestamp- now 10 :hour)
                         :ended-at nil)))))

(define-test format-session-duration-renders-units
  :parent session-suite
  (is equal "45s" (beadwork::format-session-duration 45))
  (is equal "12m" (beadwork::format-session-duration 720))
  (is equal "2h03m" (beadwork::format-session-duration (+ (* 2 3600) (* 3 60))))
  (is equal "3d04h" (beadwork::format-session-duration (+ (* 3 86400) (* 4 3600)))))

(define-test session-json-has-expected-keys
  :parent session-suite
  (beadwork:with-store (store ":memory:" :prefix "bd")
    (let* ((issue (beadwork:create-issue store :title "Worked" :type :task))
           (issue-id (beadwork:issue-id issue)))
      (plant-session store "S-one" "2026-10-01T10:00:00-07:00"
                     :ended-at "2026-10-01T11:00:00-07:00"
                     :active-issue-id issue-id
                     :agent-id "worker")
      (let* ((s (beadwork::get-session store "S-one"))
             (ht (beadwork::session->cli-json s)))
        (is equal "S-one" (gethash "id" ht))
        (is equal "ended" (gethash "state" ht))
        (is equal issue-id (gethash "active-issue-id" ht))
        (is equal 3600 (gethash "duration-seconds" ht))))))

(define-test session-json-null-for-absent-fields
  :parent session-suite
  "Absent ended-at / active-issue-id serialize as JSON null, not false."
  (beadwork:with-store (store ":memory:" :prefix "bd")
    (plant-session store "S-open" "2026-10-06T10:00:00-07:00")
    (let ((ht (beadwork::session->cli-json
               (beadwork::get-session store "S-open"))))
      (is eq 'null (gethash "ended-at" ht))
      (is eq 'null (gethash "active-issue-id" ht))
      (is equal "active" (gethash "state" ht)))))
