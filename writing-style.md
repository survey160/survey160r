# survey160r documentation style

How to write the README and the function help (`?fn`, from roxygen) so they read as one voice and respect the reader's time.

## Audience

One reader: a **Survey Manager who is competent with R** and already writes scripts against the raw campaign-export CSVs to run and optimize their campaigns. The whole package is for this reader -- there is no separate "analyst" or "developer" persona, and no part of the API is framed as being for one audience over another.

Two consequences:

- **Assume R fluency.** Do not narrate `library()`, `<-`, or `data.frame()`. Explain what is specific to survey160r, not what any R user already knows.
- **They have a task, not a lesson.** Write how-to guidance, not tutorials: open with the goal, get to runnable code fast, prefer short imperative steps to narrative paragraphs.

## Public and technical

The repository and its documentation are public. Describe how the package works -- arguments, results, runtime, memory, model accuracy, data-quality caveats in qualitative form -- never what the production data says about the business: no reply or completion rates by history, contact volumes, row counts, or findings about recipient behavior. Use toy values in examples.

## Voice and terminology

- Second person, imperative for steps ("Pull the dataset, then screen your sample").
- Person words match the code: **caller** = whoever invokes a function; **user** = the person using the package; **config author** = whoever writes a config. Never "analyst" or "the Survey-Manager surface".
- Spell names out (`disposition`, not `disp`).

## README

The README is the landing page on GitHub and the package site. It opens with the area list (followed by a one-line "New here?" pointer), then **"Which function do I need?"**: a task -> call -> section table, one row per thing a Survey Manager comes to do, in the words they would use ("drop numbers that opted out"), not the function's words. Each area then gets a section with one copy-and-go block and a pointer to the help pages that cover it; keep the sections parallel (every area has one, in the order of the table). Troubleshooting is a symptom -> cause -> fix table; add a row whenever a support question repeats.

## Examples in `?fn`

Prefer a runnable `@examples` block over `\dontrun{}` alone. A reader that takes a dataset path can still run: write a tiny frame to `tempfile(fileext = ".parquet")` with `nanoparquet::write_parquet()` (an Import, so no extra dependency) and screen against that, then keep the real `*_pull()` call in a `\dontrun{}` tail. Say in a comment that the temp file stands in for the pull.

- **Terse comments, not narration.** A trailing `# one browser sign-in, cached` beats a sentence.
- **NA-safe idioms.** Filter with `!((n_completed > 0 | n_web_complete > 0) %in% TRUE)`, never `!(n_completed > 0 | n_web_complete > 0)` -- the latter silently drops all-`NA` blank-phone rows.
- Canonical example values: campaign id `1234`; fictional phone numbers only (`555` / `999` ranges), never real PII. Never present fabricated output as if it ran.

## Reference lives in `?fn`

Exhaustive argument and column detail belongs in the function help: `@param` for every argument, `@return` for the result shape. The README shows only what the reader needs for the task and links to `?function` for the rest.

## Mechanics

- Sentence-case headings, no trailing period.
- `--` for a parenthetical aside (renders as an en-dash); never the em-dash character.
- Cross-link functions as `disposition_screen()` (pkgdown auto-links) and help pages as `?disposition_screen`.
