import click
from rich import print
import requests
import json
from datetime import datetime, timezone

from infra.tools.ghsync.utils.auth import get_password
from infra.tools.ghsync.utils.db import (
    get_all_repositories,
    get_repository_id,
    init_db,
    get_sync_metadata,
    update_sync_metadata,
    upsert_issues,
    upsert_labels,
    upsert_repositories,
)


def get_repositories(user: str, db_conn, force: bool = False):
    token = get_password('api.github.com', user + "^forge") # TODO something smarter here :-)
    endpoint_name = "repos"

    etag = None
    if not force:
        metadata = get_sync_metadata(db_conn, user=user, repository="", endpoint=endpoint_name)
        etag = metadata.get("etag")

    headers = {
        "Accept": "application/vnd.github+json",
        "Authorization": f"Bearer {token}",
        "X-GitHub-Api-Version": "2022-11-28"
    }
    if etag:
        headers["If-None-Match"] = etag

    all_repos = []
    url = f"https://api.github.com/orgs/{user}/repos"
    params = {'per_page': 100}

    first_response_etag = None
    while url:
        response = requests.get(url, headers=headers, params=params)
        # params are only for the first request with the base url
        params = None

        if not first_response_etag:
            first_response_etag = response.headers.get("ETag")

        if response.status_code == 304:
            print(f"Repositories for {user} are not modified.")
            return

        if response.status_code != 200:
            print(f"Failed to fetch repositories for {user}. Status code: {response.status_code}")
            print(response.json())
            return

        repos_data = response.json()
        all_repos.extend(repos_data)

        if 'next' in response.links:
            url = response.links['next']['url']
        else:
            url = None

    if first_response_etag:
        update_sync_metadata(
            db_conn, user=user, repository="", endpoint=endpoint_name, etag=first_response_etag
        )

    if all_repos:
        upsert_repositories(db_conn, all_repos)
    print(f"Successfully fetched and synced {len(all_repos)} repositories for {user}.")


def get_issues(owner: str, repo: str, db_conn, force: bool = False):
    """
    Fetches issues for a given repository, handling pagination.

    Args:
        owner: The owner of the repository.
        repo: The name of the repository.
        db_conn: The database connection.
    """
    token = get_password(
        "api.github.com", owner + "^forge"
    )  # TODO something smarter here :-)
    endpoint_name = "issues"
    repo_full_name = f"{owner}/{repo}"

    repository_id = get_repository_id(db_conn, repo_full_name)
    if not repository_id:
        print(
            f"Repository '{repo_full_name}' not found in the database. Please sync repositories first."
        )
        return None

    # Capture start time
    start_time = datetime.now(timezone.utc).isoformat()

    etag = None
    since = None
    if not force:
        metadata = get_sync_metadata(
            db_conn, user=owner, repository=repo, endpoint=endpoint_name
        )
        etag = metadata.get("etag")
        since = metadata.get("last_sync_timestamp")

    headers = {
        "Accept": "application/vnd.github+json",
        "Authorization": f"Bearer {token}",
        "X-GitHub-Api-Version": "2022-11-28",
    }
    if etag:
        headers["If-None-Match"] = etag

    all_issues = []
    url = f"https://api.github.com/repos/{owner}/{repo}/issues"
    params = {"per_page": 100, "state": "all"}
    if since:
        params["since"] = since

    first_response_etag = None
    while url:
        response = requests.get(url, headers=headers, params=params)
        # params are only for the first request with the base url
        params = None

        if not first_response_etag:
            first_response_etag = response.headers.get("ETag")

        if response.status_code == 304:
            print(f"Issues for {owner}/{repo} are not modified.")
            return []

        if response.status_code != 200:
            print(
                f"Failed to fetch issues for {owner}/{repo}. Status code: {response.status_code}"
            )
            print(response.json())
            return None

        issues_data = response.json()
        print(issues_data)
        all_issues.extend(issues_data)

        if "next" in response.links:
            url = response.links["next"]["url"]
        else:
            url = None

    if first_response_etag:
        update_sync_metadata(
            db_conn,
            user=owner,
            repository=repo,
            endpoint=endpoint_name,
            etag=first_response_etag,
            timestamp=start_time,
        )

    if all_issues:
        upsert_issues(db_conn, all_issues, repository_id)

    print(f"Successfully fetched and synced {len(all_issues)} issues for {owner}/{repo}.")
    return all_issues


def get_labels(owner: str, repo: str, db_conn, force: bool = False):
    """
    Fetches labels for a given repository, handling pagination.

    Args:
        owner: The owner of the repository.
        repo: The name of the repository.
        db_conn: The database connection.
    """
    token = get_password(
        "api.github.com", owner + "^forge"
    )  # TODO something smarter here :-)
    endpoint_name = "labels"
    repo_full_name = f"{owner}/{repo}"

    repository_id = get_repository_id(db_conn, repo_full_name)
    if not repository_id:
        print(
            f"Repository '{repo_full_name}' not found in the database. Please sync repositories first."
        )
        return None

    # Capture start time
    start_time = datetime.now(timezone.utc).isoformat()

    etag = None
    if not force:
        metadata = get_sync_metadata(
            db_conn, user=owner, repository=repo, endpoint=endpoint_name
        )
        etag = metadata.get("etag")

    headers = {
        "Accept": "application/vnd.github+json",
        "Authorization": f"Bearer {token}",
        "X-GitHub-Api-Version": "2022-11-28",
    }
    if etag:
        headers["If-None-Match"] = etag

    all_labels = []
    url = f"https://api.github.com/repos/{owner}/{repo}/labels"
    params = {"per_page": 100}

    first_response_etag = None
    while url:
        response = requests.get(url, headers=headers, params=params)
        # params are only for the first request with the base url
        params = None

        if not first_response_etag:
            first_response_etag = response.headers.get("ETag")

        if response.status_code == 304:
            print(f"Labels for {owner}/{repo} are not modified.")
            return []

        if response.status_code != 200:
            print(
                f"Failed to fetch labels for {owner}/{repo}. Status code: {response.status_code}"
            )
            print(response.json())
            return None

        labels_data = response.json()
        print(labels_data)
        all_labels.extend(labels_data)

        if "next" in response.links:
            url = response.links["next"]["url"]
        else:
            url = None

    if first_response_etag:
        update_sync_metadata(
            db_conn,
            user=owner,
            repository=repo,
            endpoint=endpoint_name,
            etag=first_response_etag,
            timestamp=start_time,
        )

    if all_labels:
        upsert_labels(db_conn, all_labels, repository_id)

    print(f"Successfully fetched and synced {len(all_labels)} labels for {owner}/{repo}.")
    return all_labels


@click.command()
@click.argument(
    "datatypes",
    nargs=-1,
    type=click.Choice(["issues", "prs", "repos", "labels"], case_sensitive=False),
)
@click.option("--user", default="poly-repo", help="The user or organization to sync repositories for.")
@click.option("--repo", help="The repository to sync issues for.")
@click.option("--force", is_flag=True, help="Force a full sync, ignoring etags.")
@click.pass_context
def sync(ctx, datatypes, user, repo, force):
    """Syncs GitHub issues, pull requests, and repositories."""
    if not datatypes:
        print("Nothing to sync. Please specify issues, prs, or repos, or labels.")
        return

    db_path = ctx.obj["db_path"]
    db_conn = init_db(db_path)

    datatypes = {dt.lower() for dt in datatypes}

    if "repos" in datatypes:
        print("Syncing repositories...")
        get_repositories(user, db_conn, force=force)

    # Issues and PRs are fetched from the same endpoint
    if "issues" in datatypes or "prs" in datatypes:
        repos_to_sync = []
        if repo:
            repos_to_sync.append(f"{user}/{repo}")
        else:
            print("No --repo specified, syncing issues for all repositories in the database.")
            repos_to_sync.extend(get_all_repositories(db_conn))

        if not repos_to_sync:
            print("No repositories to sync issues for.")
            return

        for repo_full_name in repos_to_sync:
            owner, repo_name = repo_full_name.split("/")
            print(f"Syncing issues and PRs for {repo_full_name}...")
            get_issues(owner, repo_name, db_conn, force=force)

    if "labels" in datatypes:
        repos_to_sync = []
        if repo:
            repos_to_sync.append(f"{user}/{repo}")
        else:
            print("No --repo specified, syncing labels for all repositories in the database.")
            repos_to_sync.extend(get_all_repositories(db_conn))

        if not repos_to_sync:
            print("No repositories to sync labels for.")
            return

        for repo_full_name in repos_to_sync:
            owner, repo_name = repo_full_name.split("/")
            print(f"Syncing labels for {repo_full_name}...")
            get_labels(owner, repo_name, db_conn, force=force)
