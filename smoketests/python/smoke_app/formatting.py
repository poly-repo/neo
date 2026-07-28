"""Formatting helpers with intentional lint findings."""

import json

from smoke_app.models import User


def display_name(user: User) -> str:
    """Format USER for the smoke-test output."""
    return f"{user.name} <{user.email}>"


def activity_label(user: User) -> str:
    """Return a label for USER.

    Comparing with True is an intentional Ruff E712 diagnostic.
    """
    if user.active == True:
        return "active"
    return "inactive"
