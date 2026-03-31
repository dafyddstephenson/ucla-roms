#!/usr/bin/env python3
"""
Scan text files for lines that begin with '#' and contain 'ifdef',
then list the words that follow 'ifdef' and the files where they appear.
"""

from __future__ import annotations

import argparse
import re
from collections import defaultdict
from pathlib import Path
from typing import DefaultDict, Iterable, Set


IFDEF_PATTERN = re.compile(r"^\s*#.*\bifdef\b\s+([A-Za-z_][A-Za-z0-9_]*)")


def iter_candidate_files(root: Path, extensions: Set[str]) -> Iterable[Path]:
    for path in root.rglob("*"):
        if not path.is_file():
            continue
        if extensions and path.suffix.lower() not in extensions:
            continue
        yield path


def collect_ifdefs(root: Path, extensions: Set[str]) -> DefaultDict[str, Set[Path]]:
    found: DefaultDict[str, Set[Path]] = defaultdict(set)

    for file_path in iter_candidate_files(root, extensions):
        try:
            with file_path.open("r", encoding="utf-8", errors="ignore") as f:
                for line in f:
                    match = IFDEF_PATTERN.match(line)
                    if match:
                        symbol = match.group(1)
                        found[symbol].add(file_path)
        except OSError as exc:
            print(f"Warning: could not read {file_path}: {exc}")

    return found


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description=(
            "Scan files for '# ... ifdef ...' lines and list symbols "
            "with the files where they appear."
        )
    )
    parser.add_argument(
        "directory",
        nargs="?",
        default=".",
        help="Directory to scan (default: current directory)",
    )
    parser.add_argument(
        "--ext",
        nargs="*",
        default=[],
        help=(
            "Optional list of file extensions to include, e.g. "
            "--ext .h .hpp .c .cpp .txt"
        ),
    )
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    root = Path(args.directory).expanduser().resolve()

    if not root.exists() or not root.is_dir():
        raise SystemExit(f"Error: '{root}' is not a valid directory.")

    extensions = {ext.lower() if ext.startswith(".") else f".{ext.lower()}" for ext in args.ext}

    results = collect_ifdefs(root, extensions)

    if not results:
        print("No matching #ifdef lines found.")
        return

    for symbol in sorted(results):
        print(symbol)
        for file_path in sorted(results[symbol]):
            print(f"  - {file_path}")
        print()


if __name__ == "__main__":
    main()

