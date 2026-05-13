# Releasing survey160r

Distribution is via [R-universe](https://survey160.r-universe.dev), which auto-rebuilds from `main` on every commit. Tags are historical anchors so users can say "I'm on 0.5.0" -- they are not part of distribution.

## Per release

1. Open a PR that:
   - Bumps `Version:` in `DESCRIPTION` per SemVer (see below).
   - Renames the `# survey160r (development version)` header in `NEWS.md` to `# survey160r X.Y.Z` and adds a fresh `(development version)` header above it.
   - Lists the changes under the new version, grouped by `## New features`, `## Bug fixes`, `## Breaking changes`, `## CI / infrastructure`, etc., with `(#NNN)` PR references.
2. Merge to `main`. CI enforces that any code change under `R/`, `man/`, or `src/` carries a Version bump, and any Version bump carries a NEWS.md edit.
3. After merge, tag the release PR's merge commit and push. The release PR's merge commit SHA is shown on the PR page under "Merged"; use that SHA (not `HEAD`) so a later merge to `main` cannot shift the tag.

   ```bash
   git fetch origin main
   git tag vX.Y.Z <merge-sha>
   git push origin vX.Y.Z
   ```

4. R-universe rebuilds within ~30 minutes. Confirm the new version at https://survey160.r-universe.dev/survey160r.

## Versioning (SemVer 2.0)

| Bump | When |
|---|---|
| Major (X.0.0) | Breaking change to exported API: removed function, renamed argument, changed return type. |
| Minor (X.Y.0) | New exported function, new optional argument, backward-compatible feature. |
| Patch (X.Y.Z) | Bug fix only, no API change. |

## Conventions

- `NEWS.md` newest at top. `# survey160r (development version)` stays at the top between releases as the scratch area for incoming PRs.
- One bullet per change. Reference the PR with `(#NNN)`.
- Group bullets under `##` subsections.
- `DESCRIPTION` carries `URL:` and `BugReports:`; keep them current if the repo moves.

## Installing a specific version

R-universe always serves the version on `main`. To install an older version by tag (reproducibility audits, bug triage):

```r
pak::pkg_install("survey160/survey160r@v0.5.0")
```

The recommended install path for users is still R-universe.
