"""Entry point containing intentional static type errors."""

from smoke_app.formatting import activity_label, display_name
from smoke_app.models import default_user


def main() -> None:
    """Print one valid result, then expose project-wide diagnostics."""
    user = default_user()
    print(display_name(user), activity_label(user))

    # Intentional type error: display_name expects User, not str.
    print(display_name("Grace"))

    # Intentional attribute error for completion and rename diagnostics.
    print(user.emali)


if __name__ == "__main__":
    main()
