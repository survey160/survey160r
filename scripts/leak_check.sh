#!/usr/bin/env bash
# Public-repo leak guard: block internal Jira keys, secret literals, and
# denylisted names (<repo>/.claude/leak-denylist.txt) from reaching this repo.
#   --staged | --range BASE HEAD | --tree   (exit 1 = leak found)
set -euo pipefail

mode="${1:---staged}"

# Denylist lives in the main working tree's .claude/ (shared across worktrees).
common_dir="$(git rev-parse --git-common-dir)"
case "$common_dir" in /*) ;; *) common_dir="$(pwd)/$common_dir" ;; esac
denylist="$(cd "$(dirname "$common_dir")" && pwd)/.claude/leak-denylist.txt"

payload=""
case "$mode" in
  --staged)
    payload="$(git diff --cached --unified=0 | sed -n 's/^+//p' | grep -v '^++' || true)"
    ;;
  --range)
    base="${2:?--range needs BASE HEAD}"
    head="${3:?--range needs BASE HEAD}"
    payload="$(git diff --unified=0 "$base...$head" | sed -n 's/^+//p' | grep -v '^++' || true)"
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
  'client_secret["'"'"']?[[:space:]]*[:=][[:space:]]*["'"'"'][A-Za-z0-9_/+-]{16,}'
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
fi

if [ "$fail" -ne 0 ]; then
  echo "leak_check: BLOCKED -- remove the above before committing to this PUBLIC repo." >&2
  exit 1
fi
echo "leak_check: clean ($mode)"
