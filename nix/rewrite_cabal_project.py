"""Rewrite cabal.project packages for the devshell."""

import json
import re
from pathlib import Path


TEMPLATE_PATH = Path("@cabal_project_template@")
PATCHED_NODE_SRC = "@patched_node_src@"
EXTRA_PACKAGES = json.loads(r"""@extra_packages_json@""")
EXTRA_PROJECT_SUFFIX = r"""@extra_project_suffix@"""
LOCAL_CHAP_PATH = "@local_chap_path@"
REPO_ROOT = str(Path.cwd())

REPO_NAME = "cardano-haskell-packages"


def rewrite_packages(text: str) -> str:
    """Rewrite the packages section to use patched and extra packages."""
    lines = text.splitlines()
    out = []
    i = 0
    while i < len(lines):
        line = lines[i]
        if line.strip() != "packages:":
            out.append(line)
            i += 1
            continue

        out.append(line)
        i += 1
        block_lines = []
        while i < len(lines):
            next_line = lines[i]
            if next_line and not next_line.startswith(" "):
                break
            block_lines.append(next_line)
            i += 1

        trailing_blanks = []
        while block_lines and block_lines[-1].strip() == "":
            trailing_blanks.insert(0, block_lines.pop())

        for block_line in block_lines:
            stripped = block_line.strip()
            if stripped == "" or stripped.startswith("--"):
                out.append(block_line)
                continue
            entry = stripped
            if entry.startswith("/"):
                out.append(f"  {entry}")
            else:
                out.append(f"  {PATCHED_NODE_SRC}/{entry}")

        for pkg in EXTRA_PACKAGES:
            out.append(f"  {pkg.replace('@REPO_ROOT@', REPO_ROOT)}")

        out.extend(trailing_blanks)

    result = "\n".join(out)
    if text.endswith("\n"):
        result += "\n"
    return result


def use_local_chap(text: str) -> str:
    """Point the CHaP repository at a local Nix-store copy.

    This uses the same pinned CHaP snapshot that cardano-node's haskell.nix
    uses, giving us cabal-file revisions and full reproducibility.
    """
    # Replace the "repository cardano-haskell-packages" block (url, secure,
    # root-keys, and any key-threshold lines) with a local file: URL.
    text = re.sub(
        r"(?:--[^\n]*\n)*"  # optional preceding comment lines
        r"repository cardano-haskell-packages\n"
        r"(?:  [^\n]+\n)*",  # indented continuation lines
        f"repository {REPO_NAME}\n  url: file:{LOCAL_CHAP_PATH}\n  secure: True\n",
        text,
    )

    return text


base_text = TEMPLATE_PATH.read_text(encoding="utf-8")
result = rewrite_packages(base_text)
if LOCAL_CHAP_PATH.strip():
    result = use_local_chap(result)
if EXTRA_PROJECT_SUFFIX.strip():
    result = result.rstrip("\n") + "\n\n" + EXTRA_PROJECT_SUFFIX.strip() + "\n"
Path("cabal.project").write_text(result, encoding="utf-8")
