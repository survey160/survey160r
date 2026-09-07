#!/usr/bin/env bash
# Local mirror of the CI version-bump + NEWS gate (see .github/workflows/R-CMD-check.yaml):
#   - a change under R/, man/, or src/ requires a DESCRIPTION Version bump;
#   - any Version bump requires a matching NEWS.md edit.
# Run in the pre-push hook so a forgotten bump fails locally, not after a red CI.
#   usage: version_gate.sh BASE HEAD   (exit 1 = gate violated)
set -euo pipefail

base="${1:?usage: version_gate.sh BASE HEAD}"
head="${2:?usage: version_gate.sh BASE HEAD}"

base_version="$(git show "$base:DESCRIPTION" 2>/dev/null | sed -n 's/^Version: //p' || true)"
head_version="$(git show "$head:DESCRIPTION" 2>/dev/null | sed -n 's/^Version: //p' || true)"

# Can't read the base DESCRIPTION (unrelated history / shallow): nothing to compare, skip.
if [ -z "$base_version" ]; then
  exit 0
fi

code_changed="$(git diff --name-only "$base...$head" -- R/ man/ src/ | head -1)"
news_changed="$(git diff --name-only "$base...$head" -- NEWS.md | head -1)"

if [ -n "$code_changed" ] && [ "$base_version" = "$head_version" ]; then
  echo "version_gate: R/man/src changed but DESCRIPTION Version was not bumped (still $base_version). Bump it." >&2
  exit 1
fi
if [ "$base_version" != "$head_version" ] && [ -z "$news_changed" ]; then
  echo "version_gate: Version bumped $base_version -> $head_version but NEWS.md was not updated. Add a section for $head_version." >&2
  exit 1
fi
exit 0
