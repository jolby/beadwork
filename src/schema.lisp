(in-package #:beadwork)

;;; Schema DDL — matches beads_rust (br) exactly for .beads/beads.db interop.

(define-constant +schema-version+ 1)

(defparameter *schema-statements*
  (list
   ;; ---- Issues table ----
   "CREATE TABLE IF NOT EXISTS issues (
        id TEXT PRIMARY KEY,
        content_hash TEXT,
        title TEXT NOT NULL CHECK(length(title) <= 500),
        description TEXT NOT NULL DEFAULT '',
        design TEXT NOT NULL DEFAULT '',
        acceptance_criteria TEXT NOT NULL DEFAULT '',
        notes TEXT NOT NULL DEFAULT '',
        status TEXT NOT NULL DEFAULT 'open',
        priority INTEGER NOT NULL DEFAULT 2 CHECK(priority >= 0 AND priority <= 4),
        issue_type TEXT NOT NULL DEFAULT 'task',
        assignee TEXT,
        owner TEXT DEFAULT '',
        estimated_minutes INTEGER,
        created_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
        created_by TEXT DEFAULT '',
        updated_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
        closed_at DATETIME,
        close_reason TEXT DEFAULT '',
        closed_by_session TEXT DEFAULT '',
        due_at DATETIME,
        defer_until DATETIME,
        external_ref TEXT,
        source_system TEXT DEFAULT '',
        source_repo TEXT NOT NULL DEFAULT '.',
        deleted_at DATETIME,
        deleted_by TEXT DEFAULT '',
        delete_reason TEXT DEFAULT '',
        original_type TEXT DEFAULT '',
        compaction_level INTEGER DEFAULT 0,
        compacted_at DATETIME,
        compacted_at_commit TEXT,
        original_size INTEGER,
        sender TEXT DEFAULT '',
        ephemeral INTEGER DEFAULT 0,
        pinned INTEGER DEFAULT 0,
        is_template INTEGER DEFAULT 0,
        CHECK (
            (status = 'closed' AND closed_at IS NOT NULL) OR
            (status = 'tombstone') OR
            (status NOT IN ('closed', 'tombstone') AND closed_at IS NULL)
        )
    )"

   ;; Primary access patterns
   "CREATE INDEX IF NOT EXISTS idx_issues_status ON issues(status)"
   "CREATE INDEX IF NOT EXISTS idx_issues_priority ON issues(priority)"
   "CREATE INDEX IF NOT EXISTS idx_issues_issue_type ON issues(issue_type)"
   "CREATE INDEX IF NOT EXISTS idx_issues_assignee ON issues(assignee) WHERE assignee IS NOT NULL"
   "CREATE INDEX IF NOT EXISTS idx_issues_created_at ON issues(created_at)"
   "CREATE INDEX IF NOT EXISTS idx_issues_updated_at ON issues(updated_at)"

   ;; Export/sync patterns
   "CREATE INDEX IF NOT EXISTS idx_issues_content_hash ON issues(content_hash)"
   "CREATE INDEX IF NOT EXISTS idx_issues_external_ref ON issues(external_ref) WHERE external_ref IS NOT NULL"
   "CREATE UNIQUE INDEX IF NOT EXISTS idx_issues_external_ref_unique ON issues(external_ref) WHERE external_ref IS NOT NULL"

   ;; Special states
   "CREATE INDEX IF NOT EXISTS idx_issues_ephemeral ON issues(ephemeral) WHERE ephemeral = 1"
   "CREATE INDEX IF NOT EXISTS idx_issues_pinned ON issues(pinned) WHERE pinned = 1"
   "CREATE INDEX IF NOT EXISTS idx_issues_tombstone ON issues(status) WHERE status = 'tombstone'"

   ;; Time-based
   "CREATE INDEX IF NOT EXISTS idx_issues_due_at ON issues(due_at) WHERE due_at IS NOT NULL"
   "CREATE INDEX IF NOT EXISTS idx_issues_defer_until ON issues(defer_until) WHERE defer_until IS NOT NULL"

   ;; Ready work composite index
   "CREATE INDEX IF NOT EXISTS idx_issues_ready
        ON issues(status, priority, created_at)
        WHERE status IN ('open', 'in_progress')
        AND ephemeral = 0
        AND pinned = 0
        AND (is_template = 0 OR is_template IS NULL)"

   ;; ---- Dependencies table ----
   "CREATE TABLE IF NOT EXISTS dependencies (
        issue_id TEXT NOT NULL,
        depends_on_id TEXT NOT NULL,
        type TEXT NOT NULL DEFAULT 'blocks',
        created_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
        created_by TEXT NOT NULL DEFAULT '',
        metadata TEXT DEFAULT '{}',
        thread_id TEXT DEFAULT '',
        PRIMARY KEY (issue_id, depends_on_id),
        FOREIGN KEY (issue_id) REFERENCES issues(id) ON DELETE CASCADE
    )"
   "CREATE INDEX IF NOT EXISTS idx_dependencies_issue ON dependencies(issue_id)"
   "CREATE INDEX IF NOT EXISTS idx_dependencies_depends_on ON dependencies(depends_on_id)"
   "CREATE INDEX IF NOT EXISTS idx_dependencies_type ON dependencies(type)"
   "CREATE INDEX IF NOT EXISTS idx_dependencies_depends_on_type ON dependencies(depends_on_id, type)"
   "CREATE INDEX IF NOT EXISTS idx_dependencies_thread ON dependencies(thread_id) WHERE thread_id != ''"
   "CREATE INDEX IF NOT EXISTS idx_dependencies_blocking
        ON dependencies(depends_on_id, issue_id)
        WHERE type IN ('blocks', 'parent-child', 'conditional-blocks', 'waits-for')"

   ;; ---- Labels table ----
   "CREATE TABLE IF NOT EXISTS labels (
        issue_id TEXT NOT NULL,
        label TEXT NOT NULL,
        PRIMARY KEY (issue_id, label),
        FOREIGN KEY (issue_id) REFERENCES issues(id) ON DELETE CASCADE
    )"
   "CREATE INDEX IF NOT EXISTS idx_labels_label ON labels(label)"
   "CREATE INDEX IF NOT EXISTS idx_labels_issue ON labels(issue_id)"

   ;; ---- Comments table ----
   "CREATE TABLE IF NOT EXISTS comments (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        issue_id TEXT NOT NULL,
        author TEXT NOT NULL,
        text TEXT NOT NULL,
        created_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
        FOREIGN KEY (issue_id) REFERENCES issues(id) ON DELETE CASCADE
    )"
   "CREATE INDEX IF NOT EXISTS idx_comments_issue ON comments(issue_id)"
   "CREATE INDEX IF NOT EXISTS idx_comments_created_at ON comments(created_at)"

   ;; ---- Events (Audit) table ----
   "CREATE TABLE IF NOT EXISTS events (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        issue_id TEXT NOT NULL,
        event_type TEXT NOT NULL,
        actor TEXT NOT NULL DEFAULT '',
        old_value TEXT,
        new_value TEXT,
        comment TEXT,
        created_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
        FOREIGN KEY (issue_id) REFERENCES issues(id) ON DELETE CASCADE
    )"
   "CREATE INDEX IF NOT EXISTS idx_events_issue ON events(issue_id)"
   "CREATE INDEX IF NOT EXISTS idx_events_type ON events(event_type)"
   "CREATE INDEX IF NOT EXISTS idx_events_created_at ON events(created_at)"
   "CREATE INDEX IF NOT EXISTS idx_events_actor ON events(actor) WHERE actor != ''"

   ;; ---- Config table ----
   "CREATE TABLE IF NOT EXISTS config (
        key TEXT PRIMARY KEY,
        value TEXT NOT NULL
    )"

   ;; ---- Metadata table ----
   "CREATE TABLE IF NOT EXISTS metadata (
        key TEXT PRIMARY KEY,
        value TEXT NOT NULL
    )"

   ;; ---- Dirty Issues table ----
   "CREATE TABLE IF NOT EXISTS dirty_issues (
        issue_id TEXT PRIMARY KEY,
        marked_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
        FOREIGN KEY (issue_id) REFERENCES issues(id) ON DELETE CASCADE
    )"
   "CREATE INDEX IF NOT EXISTS idx_dirty_issues_marked_at ON dirty_issues(marked_at)"

   ;; ---- Export Hashes table ----
   "CREATE TABLE IF NOT EXISTS export_hashes (
        issue_id TEXT PRIMARY KEY,
        content_hash TEXT NOT NULL,
        exported_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
        FOREIGN KEY (issue_id) REFERENCES issues(id) ON DELETE CASCADE
    )"

   ;; ---- Blocked Issues Cache table ----
   "CREATE TABLE IF NOT EXISTS blocked_issues_cache (
        issue_id TEXT PRIMARY KEY,
        blocked_by TEXT NOT NULL,
        blocked_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
        FOREIGN KEY (issue_id) REFERENCES issues(id) ON DELETE CASCADE
    )"
   "CREATE INDEX IF NOT EXISTS idx_blocked_cache_blocked_at ON blocked_issues_cache(blocked_at)"

   ;; ---- Child Counters table ----
   "CREATE TABLE IF NOT EXISTS child_counters (
        parent_id TEXT PRIMARY KEY,
        last_child INTEGER NOT NULL DEFAULT 0,
        FOREIGN KEY (parent_id) REFERENCES issues(id) ON DELETE CASCADE
    )"))

(defparameter +schema-sql+
  (format nil "~{~A;~%~}" *schema-statements*)
  "Full DDL as a single string for reference. Use *schema-statements* for execution.")

(defun apply-schema (db)
  "Apply the beadwork schema to DB (an sqlite:sqlite-handle).
Executes all DDL statements, sets WAL journal mode, enables foreign keys,
and configures performance PRAGMAs."
  (dolist (stmt *schema-statements*)
    (sqlite:execute-non-query db stmt))
  ;; WAL journal mode for concurrency
  (sqlite:execute-non-query db "PRAGMA journal_mode=WAL")
  ;; Enable foreign key enforcement
  (sqlite:execute-non-query db "PRAGMA foreign_keys=ON")
  ;; Performance PRAGMAs (safe with WAL mode)
  (sqlite:execute-non-query db "PRAGMA synchronous=NORMAL")
  (sqlite:execute-non-query db "PRAGMA temp_store=MEMORY")
  (sqlite:execute-non-query db "PRAGMA cache_size=-8000")
  (values))

(defun table-exists-p (db table-name)
  "Return T if TABLE-NAME exists in DB."
  (let ((result (sqlite:execute-single
                 db
                 (format nil
                         "SELECT 1 FROM sqlite_master WHERE type='table' AND name='~A'"
                         table-name))))
    (and result t)))

(defun ensure-schema (db)
  "Idempotent schema application. Checks whether the issues table exists;
if not, applies the full schema. Safe to call on every connection open."
  (unless (table-exists-p db "issues")
    (apply-schema db))
  ;; Always set PRAGMAs even if schema already exists — they are per-connection.
  (sqlite:execute-non-query db "PRAGMA journal_mode=WAL")
  (sqlite:execute-non-query db "PRAGMA foreign_keys=ON")
  (sqlite:execute-non-query db "PRAGMA synchronous=NORMAL")
  (sqlite:execute-non-query db "PRAGMA temp_store=MEMORY")
  (sqlite:execute-non-query db "PRAGMA cache_size=-8000")
  (values))
