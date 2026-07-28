"""Small domain model shared by the Python smoke-test modules."""

from dataclasses import dataclass


@dataclass(frozen=True, slots=True)
class User:
    name: str
    email: str
    active: bool


def default_user() -> User:
    """Return a valid user for navigation and completion tests."""
    return User(name="Ada", email="ada@example.test", active=True)


def append_tag(tag: str, tags: list[str] = []) -> list[str]:
    """Return TAG appended to TAGS.

    The mutable default is an intentional Ruff B006 diagnostic.
    """
    tags.append(tag)
    return tags
