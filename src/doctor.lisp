(in-package :beadwork)

(defun parse-whats-next-table (lines)
  "Parse the What's Next markdown table from LINES (list of strings).
Returns a list of rows, each a list of (priority task beadwork-id).
Skips the header and separator rows. Stops at blank line or next heading
only after at least one data row has been collected."
  (let ((in-table nil)
        (past-separator nil)
        (started nil)
        (rows nil))
    (dolist (line lines)
      (cond
        ;; Detect What's Next heading
        ((ppcre:scan "^#+\\s*What'?s Next" line)
         (setf in-table t))
        ;; Detect separator row (dashes and pipes only) — mark past it
        ((and in-table
              (ppcre:scan "^\\|" line)
              (ppcre:scan "^\\|[- :|]*\\|$" line))
         (setf past-separator t))
        ;; Exit table on blank line or next heading (only after rows collected)
        ((and in-table started
              (or (string= "" (string-trim " " line))
                  (ppcre:scan "^#" line)))
         (return))
        ;; Parse data row (pipe-delimited, skip header/separator)
        ((and in-table past-separator
              (ppcre:scan "^\\|" line))
         (let ((cells (mapcar (lambda (c) (string-trim " " c))
                              (cdr (ppcre:split "\\|" line)))))
           ;; Remove trailing pipe artifact (empty string after last |)
           (when (string= "" (car (last cells)))
             (setf cells (butlast cells)))
           ;; Require at least 2 cells (priority + task); ID is optional
           (when (>= (length cells) 2)
             (let ((priority (first cells))
                   (task (second cells))
                   (id (if (>= (length cells) 3) (third cells) "")))
               (when (plusp (length (string-trim " " task)))
                 (push (list priority task id) rows)
                 (setf started t))))))))
    (nreverse rows)))
