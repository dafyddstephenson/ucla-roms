#!/usr/bin/env python3
from pathlib import Path
import argparse
def find_ifdef_lines(root: Path) -> None:
    # Scan recursively for text files and print matching lines
    for path in root.rglob("*"):
        if not path.is_file():
            continue
        try:
            # Read as UTF-8 text; skip files that are binary or unreadable as text
            with path.open("r", encoding="utf-8", errors="strict") as f:
                for line_num, line in enumerate(f, start=1):
                    stripped = line.lstrip()
                    if stripped.startswith("#") and "ifdef" in stripped:
                        print(f"{path}:{line_num}: {line.rstrip()}")
        except (UnicodeDecodeError, OSError):
            # Ignore non-text files and unreadable files
            continue
def main():
    parser = argparse.ArgumentParser(
        description='Find lines that begin with "#" and contain "ifdef".'
    )
    parser.add_argument(
        "directory",
        nargs="?",
        default=".",
        help="Directory to scan (default: current directory)",
    )
    args = parser.parse_args()
    root = Path(args.directory)
    if not root.is_dir():
        raise SystemExit(f"Not a directory: {root}")
    find_ifdef_lines(root)
if __name__ == "__main__":
    main()
