import shlex
import subprocess
from pathlib import Path


def get_password(machine: str, login: str) -> str | None:
    """
    Retrieves a password from a gpg-encrypted ~/.authinfo.gpg file.

    The file is expected to have entries in the format:
    machine <hostname> login <username> password <password>

    Args:
        machine: The machine name to look for.
        login: The login name to look for.

    Returns:
        The password string if a match is found, otherwise None.
    """
    authinfo_path = Path.home() / ".authinfo.gpg"
    if not authinfo_path.is_file():
        return None

    try:
        completed_process = subprocess.run(
            ["gpg", "--decrypt", str(authinfo_path)],
            capture_output=True,
            text=True,
            check=True,
        )
        content = completed_process.stdout
    except (subprocess.CalledProcessError, FileNotFoundError):
        return None

    for line in content.strip().split("\n"):
        parts = shlex.split(line)
        if len(parts) % 2 != 0:
            continue
        data = dict(zip(parts[::2], parts[1::2]))

        if data.get("machine") == machine and data.get("login") == login:
            return data.get("password")

    return None
