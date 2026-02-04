#!/usr/bin/env python

import os
from pathlib import Path

import click

from infra.tools.ghsync.cmd.model import model
from infra.tools.ghsync.cmd.schema import schema
from infra.tools.ghsync.cmd.sync import sync


@click.group()
@click.option(
    "--db",
    "db_path",
    default=str(
        Path(os.environ.get("XDG_DATA_HOME", Path.home() / ".local" / "share"))
        / "neo"
        / "workflow.sqlite"
    ),
    type=click.Path(dir_okay=False, writable=True, path_type=Path),
    help="Path to the SQLite database file.",
)
@click.pass_context
def cli(ctx, db_path):
    """A CLI tool to sync GitHub data."""
    ctx.ensure_object(dict)
    ctx.obj["db_path"] = db_path


cli.add_command(sync)
cli.add_command(schema)
cli.add_command(model)

if __name__ == "__main__":
    cli()
