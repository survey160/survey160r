#!/usr/bin/env bash
# Install survey160r's local git hooks (shared across worktrees; run once per clone).
#   pre-commit: leak_check --staged + make lint
#   pre-push:   leak_check on each pushed ref + make check && make coverage
# Existing non-survey160r hooks are backed up to <hook>.bak, never clobbered.
set -euo pipefail

common_dir="$(git rev-parse --git-common-dir)"
case "$common_dir" in /*) ;; *) common_dir="$(pwd)/$common_dir" ;; esac
common_dir="$(cd "$common_dir" && pwd)"

hooks_dir="$(git rev-parse --git-path hooks)"
case "$hooks_dir" in /*) ;; *) hooks_dir="$(pwd)/$hooks_dir" ;; esac
mkdir -p "$hooks_dir"
if [ "$hooks_dir" != "$common_dir/hooks" ]; then
  echo "WARNING: core.hooksPath points at $hooks_dir (outside this repo); existing hooks there are backed up, not merged." >&2
fi

install_hook() {  # $1 = hook name; body on stdin
  target="$hooks_dir/$1"
  if [ -e "$target" ] && ! grep -q 'installed by scripts/install-hooks.sh' "$target" 2>/dev/null; then
    cp "$target" "$target.bak"
    echo "Backed up existing $1 to $1.bak" >&2
  fi
  cat > "$target"
  chmod +x "$target"
}

install_hook pre-commit <<'EOF'
#!/bin/sh
# survey160r pre-commit -- installed by scripts/install-hooks.sh
cd "$(git rev-parse --show-toplevel)" || exit 1
if [ -x scripts/leak_check.sh ]; then
  scripts/leak_check.sh --staged || exit 1
fi
if [ -f Makefile ]; then
  make lint || exit 1
fi
EOF

install_hook pre-push <<'EOF'
#!/bin/sh
# survey160r pre-push -- installed by scripts/install-hooks.sh
cd "$(git rev-parse --show-toplevel)" || exit 1
if [ -x scripts/leak_check.sh ]; then
  # Scan each ref being pushed (stdin: <local ref> <local sha> <remote ref> <remote sha>),
  # not just HEAD, so another branch or tag can't slip content past the guard.
  status=0
  while read -r local_ref local_sha remote_ref remote_sha; do
    case "$local_sha" in *[!0]*) : ;; *) continue ;; esac   # skip ref deletions
    if expr "$remote_sha" : '00*$' >/dev/null 2>&1; then
      base="$(git merge-base "$local_sha" origin/main 2>/dev/null || true)"
    else
      base="$remote_sha"
    fi
    scripts/leak_check.sh --range "$base" "$local_sha" || status=1
  done
  [ "$status" -eq 0 ] || exit 1
fi
if [ -f Makefile ]; then
  make check && make coverage || exit 1
fi
EOF

# Ensure the machine-local denylist template exists (git-ignored, never committed).
denylist="$(dirname "$common_dir")/.claude/leak-denylist.txt"
if [ ! -f "$denylist" ]; then
  mkdir -p "$(dirname "$denylist")"
  cat > "$denylist" <<'EOF'
# survey160r leak-check denylist -- machine-local, git-ignored, NEVER committed.
# One never-commit token per line (client/customer names, etc.), matched
# case-insensitively as fixed strings. Jira keys and secret literals are matched
# by pattern in scripts/leak_check.sh -- do not list them here.
EOF
  echo "Created denylist template at $denylist" >&2
fi

echo "Installed pre-commit and pre-push hooks in $hooks_dir"
