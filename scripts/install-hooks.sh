#!/usr/bin/env bash
# Install survey160r's local git hooks (shared across worktrees; run once per clone).
#   pre-commit: leak_check --staged + make lint
#   pre-push:   leak_check on the outgoing range + make check && make coverage
set -euo pipefail

hooks_dir="$(git rev-parse --git-path hooks)"
case "$hooks_dir" in /*) ;; *) hooks_dir="$(pwd)/$hooks_dir" ;; esac
mkdir -p "$hooks_dir"

cat > "$hooks_dir/pre-commit" <<'EOF'
#!/bin/sh
# survey160r pre-commit -- installed by scripts/install-hooks.sh
cd "$(git rev-parse --show-toplevel)" || exit 1
scripts/leak_check.sh --staged || exit 1
make lint
EOF

cat > "$hooks_dir/pre-push" <<'EOF'
#!/bin/sh
# survey160r pre-push -- installed by scripts/install-hooks.sh
cd "$(git rev-parse --show-toplevel)" || exit 1
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
make check && make coverage
EOF

chmod +x "$hooks_dir/pre-commit" "$hooks_dir/pre-push"
echo "Installed pre-commit and pre-push hooks in $hooks_dir"
