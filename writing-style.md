# survey160r documentation style

How to write the README and the article vignettes (`vignettes/*.Rmd`) so they read as one voice and respect the reader's time.

## Audience

One reader: a **Survey Manager who is competent with R** and already writes scripts against the raw campaign-export CSVs to run and optimize their campaigns. The whole package is for this reader -- there is no separate "analyst" or "developer" persona, and no part of the API is framed as being for one audience over another.

Two consequences:

- **Assume R fluency.** Do not narrate `library()`, `<-`, or `data.frame()`. Explain what is specific to survey160r, not what any R user already knows.
- **They have a task, not a lesson.** Write **how-to guides**, not tutorials.

## Register: how-to, not tutorial

Articles are **task-oriented how-to guides** (Diataxis sense): the reader arrives with a goal and wants the recipe. So:

- Open with the goal in one or two sentences. No motivational preamble.
- Get to runnable code fast -- the first code block should be copy-and-go.
- Prefer short imperative steps to narrative paragraphs.
- Stay scannable: task-named headings, code first, one idea per section.
- Reference (exhaustive argument/column detail) is not the article's job -- see below.

## Shared structure

Every article follows the same skeleton, so knowing one means knowing them all:

1. **Intro** -- 1-2 sentences: the task the guide covers, and that it assumes First-time setup.
2. **## \<Primary task\>** -- the main recipe, code first (e.g. "Screen a sample", "Build a report").
3. **## \<Secondary task\>** (one or more) -- common variations.
4. **## What you get back** -- the result shape; a compact, task-relevant column table.
5. **## Notes** -- gotchas, beta limits, performance.
6. **## See also** -- `?function` links and the sibling article.

## Voice and terminology

- Second person, imperative for steps ("Pull the dataset, then screen your sample").
- Person words match the code: **caller** = whoever invokes a function; **user** = the person using the package; **config author** = whoever writes a config. Never "analyst" or "the Survey-Manager surface".
- Spell names out (`disposition`, not `disp`).

## Code in articles

- **Terse comments, not narration.** A trailing `# one browser sign-in, cached` beats a sentence.
- **Runnable vs illustrative.** Chunks needing auth or network are `eval = FALSE` (shown, not run). Prefer a runnable chunk where the inputs allow -- a pure operation on a small in-memory frame (e.g. `disposition_summary()` on a data frame) -- so at least some shown output is real. Some articles legitimately have none (latency's inputs are a real campaign CSV). Never present fabricated output as if it ran.
- **NA-safe idioms.** Filter with `!(ever_completed %in% TRUE | ever_terminated %in% TRUE)`, never `!ever_completed & !ever_terminated` -- the latter silently drops all-`NA` blank-phone rows.
- Canonical example values: campaign id `1234`; fictional phone numbers only (`555` / `999` ranges), never real PII.

## Reference lives in `?fn`

Exhaustive argument and column detail belongs in the function help, not the article. An article shows only the columns the reader uses for the task and links to `?function` for the rest. (Today the full result-shape and appended-column tables still live in the articles; move them into the `@return` roxygen when that is expanded, and the articles link instead.)

## Mechanics

- Sentence-case headings, no trailing period.
- `--` for a parenthetical aside (renders as an en-dash); never the em-dash character.
- Put wide example output in a plain fenced block, not an R chunk, so lintr's line-length rule does not trip.
- Cross-link functions as `disposition_screen()` (pkgdown auto-links) and articles as `vignette("disposition")`.
