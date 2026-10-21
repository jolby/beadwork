(in-package :beadwork)

;;; ============================================================================
;;; Commands: session (subcommands)
;;; ============================================================================

;;; ---------------------------------------------------------------------------
;;; Command: session (subcommands)
;;; ---------------------------------------------------------------------------

(defun session-start/handler (cmd)
  (let* ((format-val (clingon:getopt cmd :format))
         (*format* (parse-format format-val))
       (*no-color* (clingon:getopt* cmd :no-color))
         (store (ensure-store cmd))
         (agent-id (clingon:getopt cmd :agent-id)))
    (let ((current (get-current-session store)))
      (if current
          ;; Resume existing session
          (progn
            (format t "Resuming session ~A~%" (getf current :id))
            (let ((active-id (getf current :active-issue-id)))
              (if active-id
                  (format t "Working on: ~A~%" active-id)
                  (format t "Working on: (none)~%")))
            (let ((last-action (getf current :last-action)))
              (when (and last-action (plusp (length last-action)))
                (format t "Last action: ~A~%" last-action))))
          ;; No active session -- show previous handoff then start new
          (progn
            (let ((last (get-last-session store)))
              (when last
                (let ((ended-at (getf last :ended-at))
                      (notes (getf last :handoff-notes)))
                  (when ended-at
                    (format t "Previous session ended: ~A~%"
                            (format-timestamp ended-at)))
                  (when (and notes (plusp (length notes)))
                    (format t "Handoff notes:~%  ~A~%~%" notes)))))
            (let ((session (start-session store :agent-id agent-id)))
              (if session
                  (progn
                    (format t "Session ~A started.~%" (getf session :id))
                    (when agent-id
                      (format t "Agent: ~A~%" agent-id)))
                  (format t "(could not start session)~%"))))))))

(defun session-start/options ()
  (list
   (clingon:make-option
    :string
    :long-name "agent-id"
    :description "Agent identifier for multi-agent tracking"
    :key :agent-id)))

(defun session-start/command ()
  (clingon:make-command
   :name "start"
   :description "Start a new work session, showing previous handoff notes"
   :options (append (session-start/options) (global-options))
   :handler #'session-start/handler))

(defun session-end/handler (cmd)
  (let* ((store (ensure-store cmd))
         (notes (clingon:getopt cmd :notes))
         (current (get-current-session store)))
    (unless current
      (format *error-output* "No active session.~%")
      (clingon:exit 1))
    (end-session store (getf current :id) :notes notes)
    (format t "Session ~A ended.~%" (getf current :id))
    (when notes
      (format t "Handoff notes saved.~%"))))

(defun session-end/options ()
  (list
   (clingon:make-option
    :string
    :long-name "notes"
    :short-name #\n
    :description "Handoff notes for the next session"
    :key :notes)))

(defun session-end/command ()
  (clingon:make-command
   :name "end"
   :description "End the current session with optional handoff notes"
   :options (append (session-end/options) (global-options))
   :handler #'session-end/handler))

(defun session-work/handler (cmd)
  (let* ((store (ensure-store cmd))
         (issue-id (first (clingon:command-arguments cmd)))
         (current (get-current-session store)))
    (unless current
      (format *error-output* "No active session. Use 'bw session start' first.~%")
      (clingon:exit 1))
    (unless issue-id
      (format *error-output* "Usage: bw session work <issue-id>~%")
      (clingon:exit 1))
    ;; Verify issue exists
    (handler-case
        (get-issue store issue-id)
      (issue-not-found ()
        (format *error-output* "Issue ~A not found.~%" issue-id)
        (clingon:exit 1)))
    (set-session-work store (getf current :id) issue-id)
    (format t "Now working on: ~A~%" issue-id)))

(defun session-work/command ()
  (clingon:make-command
   :name "work"
   :description "Set the active issue for the current session"
   :options (global-options)
   :handler #'session-work/handler
   :usage "<issue-id>"))

(defun session-action/handler (cmd)
  (let* ((store (ensure-store cmd))
         (text (first (clingon:command-arguments cmd)))
         (current (get-current-session store)))
    (unless current
      (format *error-output* "No active session. Use 'bw session start' first.~%")
      (clingon:exit 1))
    (unless text
      (format *error-output* "Usage: bw session action <text>~%")
      (clingon:exit 1))
    (record-session-action store (getf current :id) text)
    (format t "Action recorded: ~A~%" text)))

(defun session-action/command ()
  (clingon:make-command
   :name "action"
   :description "Record a breadcrumb action (survives context compression)"
   :options (global-options)
   :handler #'session-action/handler
   :usage "<text>"))

(defun session-status->json (session minutes)
  "Convert SESSION plus its elapsed MINUTES to a JSON object hash-table.

An alist stringifies to a JSON array in jzon; consumers expect an object
(bd-7b6)."
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash "session-id" ht) (getf session :id))
    (setf (gethash "started-at" ht) (format-timestamp (getf session :started-at)))
    (setf (gethash "duration-minutes" ht) minutes)
    (setf (gethash "active-issue-id" ht) (or (getf session :active-issue-id) 'null))
    (setf (gethash "last-action" ht) (or (getf session :last-action) ""))
    (setf (gethash "agent-id" ht) (or (getf session :agent-id) ""))
    ht))

(defun session-status/handler (cmd)
  (let* ((format-val (clingon:getopt cmd :format))
         (*format* (parse-format format-val))
       (*no-color* (clingon:getopt* cmd :no-color))
         (store (ensure-store cmd))
         (current (get-current-session store)))
    (if current
        (let* ((started (getf current :started-at))
               ;; %session-duration-seconds computes end - started; doing the
               ;; subtraction inline here had the arguments reversed, so the
               ;; human output and the JSON both always said 0 minutes (bd-224).
               (minutes (floor (%session-duration-seconds current) 60))
               (active-id (getf current :active-issue-id))
               (last-action (getf current :last-action)))
          (unless (eq *format* :json)
            (format t "Session ~A (started ~A, ~D min ago)~%"
                    (getf current :id)
                    (format-timestamp started)
                    minutes)
            (if active-id
                (format t "Working on: ~A~%" active-id)
                (format t "Working on: (none)~%"))
            (when (and last-action (plusp (length last-action)))
              (format t "Last action: ~A~%" last-action)))
          (when (eq *format* :json)
            (format t "~A"
                    (jzon:stringify (session-status->json current minutes)
                                    :pretty t))))
        (unless (eq *format* :json)
          (format t "No active session. Use 'bw session start' to begin.~%")))))

(defun session-status/command ()
  (clingon:make-command
   :name "status"
   :description "Show current session status"
   :aliases '("st")
   :options (global-options)
   :handler #'session-status/handler))

(defun %session-state (session)
  "Return :ended, :stale, or :active for SESSION."
  (cond
    ((getf session :ended-at) :ended)
    ((local-time:timestamp<
      (getf session :started-at)
      (local-time:timestamp- (local-time:now) *session-stale-hours* :hour))
     :stale)
    (t :active)))

(defun %session-duration-seconds (session)
  "Elapsed seconds for SESSION: ended_at - started_at, else now - started_at."
  (let ((end (or (getf session :ended-at) (local-time:now))))
    (max 0 (floor (local-time:timestamp-difference
                   end (getf session :started-at))))))

(defun format-session-duration (seconds)
  "Render SECONDS as e.g. 45s, 12m, 2h03m, 3d04h."
  (cond
    ((< seconds 60) (format nil "~Ds" seconds))
    ((< seconds 3600) (format nil "~Dm" (floor seconds 60)))
    ((< seconds 86400)
     (format nil "~Dh~2,'0Dm" (floor seconds 3600)
             (floor (mod seconds 3600) 60)))
    (t (format nil "~Dd~2,'0Dh" (floor seconds 86400)
               (floor (mod seconds 86400) 3600)))))

(defun session->cli-json (session)
  "Build the CLI JSON hash-table for SESSION."
  (let ((ht (make-hash-table :test #'equal))
        (ended (getf session :ended-at)))
    (setf (gethash "id" ht) (getf session :id))
    (setf (gethash "started-at" ht) (format-timestamp (getf session :started-at)))
    (setf (gethash "ended-at" ht) (if ended (format-timestamp ended) 'null))
    (setf (gethash "duration-seconds" ht) (%session-duration-seconds session))
    (setf (gethash "state" ht)
          (string-downcase (symbol-name (%session-state session))))
    (setf (gethash "active-issue-id" ht)
          (or (getf session :active-issue-id) 'null))
    (setf (gethash "last-action" ht) (getf session :last-action))
    (setf (gethash "handoff-notes" ht) (getf session :handoff-notes))
    (setf (gethash "agent-id" ht) (getf session :agent-id))
    (setf (gethash "agent-session-id" ht) (getf session :agent-session-id))
    ht))

(defun %session-action-summary (session)
  "One-line last-action for the list table, truncated to 50 chars."
  (let ((text (string-trim '(#\Newline #\Return #\Tab)
                           (or (getf session :last-action) ""))))
    (if (> (length text) 50)
        (concatenate 'string (subseq text 0 47) "...")
        text)))

(defun %session-timestamp-short (timestamp)
  "Render TIMESTAMP as YYYY-MM-DD HH:MM for the list view."
  (local-time:format-timestring
   nil timestamp
   :format '((:year 4) #\- (:month 2) #\- (:day 2) #\Space (:hour 2) #\: (:min 2))))

(defun print-sessions (sessions)
  "Render SESSIONS according to *format*."
  (ecase *format*
    (:json
     (format t "~A" (jzon:stringify
                     (coerce (mapcar #'session->cli-json sessions) 'vector)
                     :pretty t)))
    (:plain
     (dolist (s sessions)
       (format t "~A  ~A  ~A  ~A~%"
               (getf s :id)
               (%session-timestamp-short (getf s :started-at))
               (format-session-duration (%session-duration-seconds s))
               (string-downcase (symbol-name (%session-state s))))))
    (:rich
     (let ((id-w (max 6 (reduce #'max
                                (mapcar (lambda (s) (length (getf s :id))) sessions)
                                :initial-value 0)))
           (state-w (max 5 (reduce #'max
                                   (mapcar (lambda (s)
                                             (length (symbol-name (%session-state s))))
                                           sessions)
                                   :initial-value 0)))
           (agent-w (max 5 (reduce #'max
                                   (mapcar (lambda (s) (length (getf s :agent-id)))
                                           sessions)
                                   :initial-value 0))))
       (format t "~A  ~A  ~A  ~A  ~A  ~A  ~A~%"
               (pad-right "ID" id-w) (pad-right "STARTED" 16)
               (pad-right "DURATION" 8) (pad-right "STATE" state-w)
               (pad-right "AGENT" agent-w) (pad-right "ISSUE" 12)
               "LAST ACTION")
       (format t "~A  ~A  ~A  ~A  ~A  ~A  ~A~%"
               (make-string id-w :initial-element #\-)
               (make-string 16 :initial-element #\-)
               (make-string 8 :initial-element #\-)
               (make-string state-w :initial-element #\-)
               (make-string agent-w :initial-element #\-)
               (make-string 12 :initial-element #\-)
               "-----------")
       (dolist (s sessions)
         (format t "~A  ~A  ~A  ~A  ~A  ~A  ~A~%"
                 (pad-right (getf s :id) id-w)
                 (pad-right (%session-timestamp-short (getf s :started-at)) 16)
                 (pad-right (format-session-duration (%session-duration-seconds s)) 8)
                 (pad-right (string-downcase (symbol-name (%session-state s))) state-w)
                 (pad-right (getf s :agent-id) agent-w)
                 (pad-right (or (getf s :active-issue-id) "-") 12)
                 (%session-action-summary s)))))))

(defun print-session-detail (session)
  "Render one SESSION in detail according to *format*."
  (ecase *format*
    (:json
     (format t "~A" (jzon:stringify (session->cli-json session) :pretty t)))
    ((:plain :rich)
     (let ((ended (getf session :ended-at)))
       (format t "ID: ~A~%" (getf session :id))
       (format t "State: ~A~%"
               (string-downcase (symbol-name (%session-state session))))
       (format t "Started: ~A~%" (format-timestamp (getf session :started-at)))
       (format t "Ended: ~A~%" (if ended (format-timestamp ended) "-"))
       (format t "Duration: ~A~%"
               (format-session-duration (%session-duration-seconds session)))
       (format t "Active issue: ~A~%" (or (getf session :active-issue-id) "-"))
       (format t "Agent: ~A~%" (or (getf session :agent-id) "-"))
       (format t "Agent session: ~A~%" (or (getf session :agent-session-id) "-"))
       (format t "Last action: ~A~%" (or (getf session :last-action) "-"))
       (format t "~%Handoff notes:~%~A~%" (or (getf session :handoff-notes) ""))))))

(defun session-list/handler (cmd)
  (let* ((format-val (clingon:getopt cmd :format))
         (*format* (parse-format format-val))
         (*no-color* (clingon:getopt* cmd :no-color))
         (store (ensure-store cmd))
         (all (clingon:getopt cmd :all))
         (explicit-limit (clingon:getopt cmd :limit))
         (active (clingon:getopt cmd :active))
         (agent-id (clingon:getopt cmd :agent-id))
         (limit (cond (all nil)
                      (explicit-limit explicit-limit)
                      (t 20)))
         (sessions (list-sessions store :limit limit :active active
                                        :agent-id agent-id)))
    (if (and (null sessions) (not (eq *format* :json)))
        (format t "No sessions found.~%")
        (print-sessions sessions))))

(defun session-list/options ()
  (list
   (clingon:make-option
    :boolean/true
    :long-name "all"
    :description "List all sessions (default: most recent 20)"
    :key :all)
   (clingon:make-option
    :integer
    :short-name #\n
    :long-name "limit"
    :description "Maximum number of sessions to list"
    :key :limit)
   (clingon:make-option
    :boolean/true
    :long-name "active"
    :description "Only active (not ended) sessions"
    :key :active)
   (clingon:make-option
    :string
    :long-name "agent-id"
    :description "Filter by agent id"
    :key :agent-id)))

(defun session-list/command ()
  (clingon:make-command
   :name "list"
   :description "List sessions, newest first"
   :aliases '("ls")
   :options (append (session-list/options) (global-options))
   :handler #'session-list/handler))

(defun session-show/handler (cmd)
  (let* ((format-val (clingon:getopt cmd :format))
         (*format* (parse-format format-val))
         (*no-color* (clingon:getopt* cmd :no-color))
         (store (ensure-store cmd))
         (arg (first (clingon:command-arguments cmd)))
         (session (if arg
                      (get-session store arg)
                      (get-last-session store))))
    (cond
      ((null session)
       (format *error-output* "~A~%"
               (if arg
                   (format nil "Session ~A not found." arg)
                   "No sessions found."))
       (clingon:exit 1))
      (t (print-session-detail session)))))

(defun session-show/command ()
  (clingon:make-command
   :name "show"
   :description "Show a session in detail (default: the last session)"
   :aliases '("sh")
   :usage "[session-id]"
   :options (global-options)
   :handler #'session-show/handler))

(defun session/command ()
  (clingon:make-command
   :name "session"
   :description "Manage work sessions for context continuity"
   :sub-commands (list (session-start/command)
                       (session-end/command)
                       (session-work/command)
                       (session-action/command)
                       (session-status/command)
                       (session-list/command)
                       (session-show/command))))
