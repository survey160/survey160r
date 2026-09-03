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
base="$(git merge-base HEAD origin/main 2>/dev/null || true)"
if [ -n "$base" ]; then
  scripts/leak_check.sh --range "$base" HEAD || exit 1
else
  scripts/leak_check.sh --tree || exit 1
fi
make check && make coverage
EOF

chmod +x "$hooks_dir/pre-commit" "$hooks_dir/pre-push"
echo "Installed pre-commit and pre-push hooks in $hooks_dir"
