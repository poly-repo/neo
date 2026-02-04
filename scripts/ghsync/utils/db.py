import sqlite3
from datetime import datetime, timezone
from pathlib import Path

TABLE_DEFINITIONS = {
    "sync_metadata": {
        "columns": [
            ("user", "TEXT NOT NULL"),
            ("repository", "TEXT NOT NULL"),
            ("endpoint", "TEXT NOT NULL"),
            ("last_sync_timestamp", "TEXT"),
            ("etag", "TEXT"),
        ],
        "constraints": ["PRIMARY KEY (user, repository, endpoint)"],
    },
    "repositories": {
        "columns": [
            ("id", "INTEGER PRIMARY KEY"),
            ("full_name", "TEXT NOT NULL UNIQUE"),
            ("fork", "INTEGER NOT NULL"),
            ("created_at", "TEXT"),
            ("pushed_at", "TEXT"),
            ("updated_at", "TEXT"),
            ("visibility", "TEXT"),
            ("forks", "INTEGER"),
            ("default_branch", "TEXT"),
        ],
    },
    "issues": {
        "columns": [
            ("id", "INTEGER PRIMARY KEY"),
            ("number", "INTEGER NOT NULL"),
            ("title", "TEXT"),
            ("type", "TEXT NOT NULL"),
            ("state", "TEXT NOT NULL"),
            ("draft", "INTEGER NOT NULL DEFAULT 0"),
            ("created_at", "TEXT NOT NULL"),
            ("updated_at", "TEXT NOT NULL"),
            ("closed_at", "TEXT"),
            ("merged_at", "TEXT"),
            ("repository_id", "INTEGER NOT NULL"),
        ],
        "constraints": [
            "FOREIGN KEY (repository_id) REFERENCES repositories (id)",
            "UNIQUE(repository_id, number)",
        ],
    },
    "issue_labels": {
        "columns": [
            ("issue_id", "INTEGER NOT NULL"),
            ("label_id", "INTEGER NOT NULL"),
        ],
        "constraints": [
            "PRIMARY KEY (issue_id, label_id)",
            "FOREIGN KEY (issue_id) REFERENCES issues (id) ON DELETE CASCADE",
            "FOREIGN KEY (label_id) REFERENCES labels (id) ON DELETE CASCADE",
        ],
    },
    "labels": {
        "columns": [
            ("id", "INTEGER PRIMARY KEY"),
            ("name", "TEXT NOT NULL"),
            ("color", "TEXT"),
            ("description", "TEXT"),
            ("repository_id", "INTEGER NOT NULL"),
        ],
        "constraints": [
            "UNIQUE (repository_id, name)",
            "FOREIGN KEY (repository_id) REFERENCES repositories (id)",
        ],
    },
}


def get_db_connection(db_path: Path):
    """
    Establishes a connection to the SQLite database.
    The database file is created if it doesn't exist.
    """
    db_path.parent.mkdir(parents=True, exist_ok=True)
    conn = sqlite3.connect(str(db_path))
    return conn


def create_tables(conn):
    """Creates database tables from TABLE_DEFINITIONS if they don't exist."""
    cursor = conn.cursor()
    for table_name, definition in TABLE_DEFINITIONS.items():
        columns_sql = ", ".join(
            [f'"{col_name}" {col_type}' for col_name, col_type in definition["columns"]]
        )
        constraints_sql = ", ".join(definition.get("constraints", []))

        query_parts = [columns_sql]
        if constraints_sql:
            query_parts.append(constraints_sql)

        create_table_sql = (
            f"CREATE TABLE IF NOT EXISTS {table_name} ({', '.join(query_parts)})"
        )
        cursor.execute(create_table_sql)
    conn.commit()


def init_db(db_path: Path):
    """Initializes the database connection and creates tables if they don't exist."""
    conn = get_db_connection(db_path)
    create_tables(conn)
    return conn


def get_repository_id(conn, repo_full_name):
    """Fetches a repository's ID from the database by its full name."""
    cursor = conn.cursor()
    cursor.execute("SELECT id FROM repositories WHERE full_name = ?", (repo_full_name,))
    result = cursor.fetchone()
    return result[0] if result else None


def get_all_repositories(conn):
    """Fetches all repository full names from the database."""
    cursor = conn.cursor()
    cursor.execute("SELECT full_name FROM repositories")
    results = cursor.fetchall()
    return [result[0] for result in results]


def get_sync_metadata(conn, user, repository, endpoint):
    """Fetches sync metadata (etag, last_sync_timestamp) from the database."""
    cursor = conn.cursor()
    cursor.execute(
        "SELECT etag, last_sync_timestamp FROM sync_metadata WHERE user = ? AND repository = ? AND endpoint = ?",
        (user, repository, endpoint),
    )
    result = cursor.fetchone()
    if result:
        return {"etag": result[0], "last_sync_timestamp": result[1]}
    return {"etag": None, "last_sync_timestamp": None}


def update_sync_metadata(conn, user, repository, endpoint, etag):
    """Updates or inserts sync metadata into the database."""
    cursor = conn.cursor()
    timestamp = datetime.now(timezone.utc).isoformat()
    cursor.execute(
        """
        INSERT INTO sync_metadata (user, repository, endpoint, etag, last_sync_timestamp)
        VALUES (?, ?, ?, ?, ?)
        ON CONFLICT(user, repository, endpoint) DO UPDATE SET
        etag = excluded.etag,
        last_sync_timestamp = excluded.last_sync_timestamp
        """,
        (user, repository, endpoint, etag, timestamp),
    )
    conn.commit()


def upsert_labels(conn, labels_data, repository_id):
    """Updates or inserts label data into the database."""
    cursor = conn.cursor()
    labels_to_upsert = []
    for label in labels_data:
        labels_to_upsert.append(
            (
                label["id"],
                label["name"],
                label["color"],
                label.get("description"),
                repository_id,
            )
        )

    cursor.executemany(
        """
        INSERT INTO labels (id, name, color, description, repository_id)
        VALUES (?, ?, ?, ?, ?)
        ON CONFLICT(id) DO UPDATE SET
            name = excluded.name,
            color = excluded.color,
            description = excluded.description
        """,
        labels_to_upsert,
    )
    conn.commit()


def upsert_issues(conn, issues_data, repository_id):
    """Updates or inserts issue data and their labels into the database."""
    cursor = conn.cursor()
    issues_to_upsert = []
    all_labels_from_issues = []
    issue_label_links = []

    for issue in issues_data:
        issue_type = "PR" if "pull_request" in issue else "Issue"
        merged_at = (
            issue.get("pull_request", {}).get("merged_at")
            if issue.get("pull_request")
            else None
        )
        issues_to_upsert.append(
            (
                issue["id"],
                issue["number"],
                issue["title"],
                issue_type,
                issue["state"],
                issue.get("draft", False),
                issue["created_at"],
                issue["updated_at"],
                issue["closed_at"],
                merged_at,
                repository_id,
            )
        )

        if "labels" in issue and issue["labels"]:
            all_labels_from_issues.extend(issue["labels"])
            for label in issue["labels"]:
                issue_label_links.append((issue["id"], label["id"]))

    cursor.executemany(
        """
        INSERT INTO issues (id, number, title, type, state, draft, created_at, updated_at, closed_at, merged_at, repository_id)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
        ON CONFLICT(id) DO UPDATE SET
            number = excluded.number,
            title = excluded.title,
            type = excluded.type,
            state = excluded.state,
            draft = excluded.draft,
            created_at = excluded.created_at,
            updated_at = excluded.updated_at,
            closed_at = excluded.closed_at,
            merged_at = excluded.merged_at,
            repository_id = excluded.repository_id
        """,
        issues_to_upsert,
    )

    if all_labels_from_issues:
        upsert_labels(conn, all_labels_from_issues, repository_id)

    issue_ids = [issue["id"] for issue in issues_data]
    if issue_ids:
        placeholders = ",".join("?" for _ in issue_ids)
        cursor.execute(
            f"DELETE FROM issue_labels WHERE issue_id IN ({placeholders})", issue_ids
        )

    if issue_label_links:
        cursor.executemany(
            """
            INSERT INTO issue_labels (issue_id, label_id)
            VALUES (?, ?)
            """,
            issue_label_links,
        )

    conn.commit()


def upsert_repositories(conn, repos_data):
    """Updates or inserts repository data into the database."""
    cursor = conn.cursor()
    repos_to_upsert = []
    for repo in repos_data:
        repos_to_upsert.append(
            (
                repo["id"],
                repo["full_name"],
                repo["fork"],
                repo["created_at"],
                repo["pushed_at"],
                repo["updated_at"],
                repo["visibility"],
                repo["forks"],
                repo["default_branch"],
            )
        )

    cursor.executemany(
        """
        INSERT INTO repositories (id, full_name, fork, created_at, pushed_at, updated_at, visibility, forks, default_branch)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
        ON CONFLICT(id) DO UPDATE SET
            full_name = excluded.full_name,
            fork = excluded.fork,
            created_at = excluded.created_at,
            pushed_at = excluded.pushed_at,
            updated_at = excluded.updated_at,
            visibility = excluded.visibility,
            forks = excluded.forks,
            default_branch = excluded.default_branch
        """,
        repos_to_upsert,
    )
    conn.commit()
