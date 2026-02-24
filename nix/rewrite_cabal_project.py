"""Rewrite cabal.project packages for the devshell."""

import json
from pathlib import Path


TEMPLATE_PATH = Path("@cabal_project_template@")
PATCHED_NODE_SRC = "@patched_node_src@"
EXTRA_PACKAGES = json.loads(r"""@extra_packages_json@""")
REPO_ROOT = str(Path.cwd())


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


base_text = TEMPLATE_PATH.read_text(encoding="utf-8")
Path("cabal.project").write_text(rewrite_packages(base_text), encoding="utf-8")
