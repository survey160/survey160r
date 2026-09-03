#!/usr/bin/env bash
# Public-repo leak guard: block internal Jira keys, secret literals, and
# denylisted names (<repo>/.claude/leak-denylist.txt) from reaching this repo.
#   --staged | --range BASE HEAD | --tree   (exit 1 = leak found; fails closed on git error)
set -euo pipefail

mode="${1:---staged}"

# Denylist lives in the main working tree's .claude/ (shared across worktrees).
common_dir="$(git rev-parse --git-common-dir)"
case "$common_dir" in /*) ;; *) common_dir="$(pwd)/$common_dir" ;; esac
denylist="$(cd "$(dirname "$common_dir")" && pwd)/.claude/leak-denylist.txt"

# Set $payload to the added content lines of a git diff/log command. Keep lines
# starting '+' but drop the '+++ ' file header (trailing space: git always writes
# the header that way, so an added line whose own content starts with '+'/'++' is
# kept), then strip the one leading '+'. Fails closed: a git error (bad/absent
# rev, shallow clone) aborts rather than passing an empty scan.
payload=""
extract_added() {
  local raw
  if ! raw="$("$@" 2>/dev/null)"; then
    echo "leak_check: '$*' failed -- refusing to pass this scan" >&2
    exit 1
  fi
  payload="$(printf '%s\n' "$raw" | grep '^+' | grep -v '^+++ ' | sed 's/^+//' || true)"
}

case "$mode" in
  --staged)
    extract_added git diff --cached --unified=0
    ;;
  --range)
    base="${2-}"
    head="${3:?--range needs [BASE] HEAD}"
    # -p over the range (not the net diff) so a secret added then removed within
    # the range is still caught; --diff-merges=first-parent so merge-commit edits
    # (conflict resolutions) are scanned too.
    if [ -n "$base" ]; then
      extract_added git log -p --diff-merges=first-parent --unified=0 --no-color "$base..$head"
    else
      extract_added git log -p --diff-merges=first-parent --unified=0 --no-color "$head" --not --remotes
    fi
    ;;
  --tree)
    :
    ;;
  *)
    echo "usage: leak_check.sh --staged | --range BASE HEAD | --tree" >&2
    exit 2
    ;;
esac

fail=0
flag() { printf '  \033[31m✗ %s\033[0m\n' "$1" >&2; fail=1; }

grep_regex() {
  if [ "$mode" = "--tree" ]; then
    git grep -nE -e "$1" -- . 2>/dev/null || true
  else
    printf '%s\n' "$payload" | grep -nE -e "$1" || true
  fi
}
grep_fixed() {
  if [ "$mode" = "--tree" ]; then
    git grep -inF -e "$1" -- . 2>/dev/null || true
  else
    printf '%s\n' "$payload" | grep -inF -e "$1" || true
  fi
}
show() { printf '%s\n' "$1" | sed 's/^/      /' >&2; }

out="$(grep_regex 'SUR-[0-9]+')"
if [ -n "$out" ]; then flag "internal Jira key(s) (SUR-####) -- this repo is PUBLIC"; show "$out"; fi

for pat in \
  'AIza[0-9A-Za-z_-]{35}' \
  'GOCSPX-[A-Za-z0-9_-]{10,}' \
  'BEGIN[A-Za-z ]*PRIVATE KEY' \
  'client_secret["'"'"']?[[:space:]]*[:=][[:space:]]*["'"'"']?[A-Za-z0-9_/+-]{16,}'
do
  out="$(grep_regex "$pat")"
  if [ -n "$out" ]; then flag "possible secret literal (/$pat/)"; show "$out"; fi
done

if [ -f "$denylist" ]; then
  while IFS= read -r term || [ -n "$term" ]; do
    case "$term" in ''|\#*) continue ;; esac
    out="$(grep_fixed "$term")"
    if [ -n "$out" ]; then flag "denylisted term: $term"; show "$out"; fi
  done < "$denylist"
else
  echo "leak_check: note -- no denylist at $denylist; client-name check skipped" >&2
fi

if [ "$fail" -ne 0 ]; then
  echo "leak_check: BLOCKED -- remove the above before committing to this PUBLIC repo." >&2
  exit 1
fi
echo "leak_check: clean ($mode)"
