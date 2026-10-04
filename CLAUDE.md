# CLAUDE.md — nansenbiomass

You are helping develop **nansenbiomass**, an R package that reconstructs survey biomass
and abundance estimates with two frameworks: StoX (design-based, the reference) and
sdmTMB (model-based). The specification is [`docs/spec.md`](docs/spec.md). Read the
relevant sections before planning any milestone, and start each milestone in plan mode.

The package must work without Claude Code. You write code, configuration and narrative;
deterministic R code produces every number; a person decides what leaves the data zone.

---

## ⛔ DATA PROTECTION — non-negotiable, keep in mind on every task

Survey data belong to partner countries under separate agreements. Anything you read, or
receive as command output, is sent to the model provider. The rules below therefore
govern what you **see**, not only what you do. They adapt the privacy guardrails of
[BAIT](https://github.com/DeepWaterIMR/BAIT) (IMR's Biotic AI Toolkit), with one
deliberate difference: under BAIT the agent queries real data and reads small results;
here it never touches real data at all (docs/spec.md, Section 3.5).

### Data classes (docs/spec.md, Section 3.1)

| Class | Examples | You may see it |
| --- | --- | --- |
| C0 Raw | biotic.xml, DuckDB databases, NAN-SIS exports, catch and length records, echosounder files | Never |
| C1 Station-level derived | Densities per haul, station-level fitted values and residuals, model objects (`.rds`), any map or plot showing station positions | Never; treated as raw |
| C2 Aggregated | Estimates by survey, stratum and species; CVs, intervals, summary diagnostics | Only once a person has released it into `outbox/` |
| C3 Structural | Code, configuration, schemas (field names and types), synthetic data | Yes |

**Official StoX projects and stratum files (project lead's decision, 4 October 2026).** Official
StoX project files (`project.json`, `project.xml`) and stratum files are class C3 and may be shared
with you, for example as attachments. They may contain a station exclusion list and file paths
(which can carry a cruise number or vessel name); that is accepted. No survey data (biotic files,
catches, lengths, weights) and no outputs with station-level values are shared. Rules 1 to 3
below apply to everything else.

### Rules

1. **Never open, list, search or request anything in the data zone.** Real data and full
   results live on the developer's laptop, under the path in `NANSEN_DATA_ROOT`. They are
   never in this repository and must never be copied here. If a path might lead into the
   data zone, do not open it; ask.
2. **Never ask a person to paste data,** and do not process data that is pasted. If it
   happens, follow "If something leaks" below.
3. **Run code only on synthetic data.** Real-data runs are started by a person, in R on
   the laptop, outside Claude Code (for example
   `nansenbiomass::run_estimate("configs/<survey>.yml")`). When a task needs a number that
   only real data can provide, ask the person to run the pipeline and release the relevant
   C2 output.
4. **`outbox/` is read-only for you.** Never create, modify, move or delete anything in
   `outbox/`, by any means: file tools, shell commands, R code or git. Only a person
   releases outputs, after the disclosure check.
5. **Never commit data files.** Do not weaken `.gitignore`, and do not add exceptions to it
   without the project lead's agreement.
6. **Synthetic data only, and clearly synthetic.** Test and development data are generated
   by the `synth` module from known spatial fields, or are small, obviously artificial
   fixtures. Never derive synthetic data from real records (no rounding, jittering or
   subsampling of real data), and never write realistic-looking survey records by hand:
   no real cruise identifiers, vessel names or station positions.
7. **Station positions are C1**, even inside an aggregated map or plot. Code may use
   positions internally, since the models need them, but no output designed for release
   may contain or display them.
8. **Errors must not echo data.** Pipeline wrappers write details to the local log and
   print only a sanitised code and message. Do not write `stop()`, `warning()` or
   `message()` calls that interpolate record-level values.
9. **Stay within the permission rules.** Do not try to work around a denied tool call,
   and do not suggest Remote Control or local sessions with shell access for this project.

If a request would breach any of the above, stop and explain rather than comply.

### If something leaks

If raw or station-level data reach the conversation, a commit or any external service:
stop, tell the person immediately, and do not repeat, summarise or process the content.
Help them remove the exposed copy (for a commit, before it is pushed if possible), revoke
or rotate any access that was shared, and report it under IMR's data-handling rules and
the relevant data agreement. Data sent to an external service may persist after deletion.

---

## Conventions

- **Style.** The [tidyverse style guide](https://style.tidyverse.org/): snake_case, the
  native pipe `|>`, tidyverse packages for data manipulation. In package code, call
  functions as `pkg::fun()` and declare the package in `DESCRIPTION`; never use
  `library()` inside `R/`.
- **Documentation.** roxygen2 with markdown. Every exported function has a title,
  parameters, a return value and an example that runs on synthetic data.
- **Tests.** testthat, 3rd edition. One file per module, `tests/testthat/test-<module>.R`.
  Fixtures are synthetic; random seeds are fixed. A module is not finished until its tests
  pass in a cloud session.
- **Dependencies.** renv. The lockfile is authoritative and is maintained on the laptop.
  Do not run `renv::update()` or `renv::snapshot()` unless asked. Adding a dependency means
  adding it to `DESCRIPTION` and saying so in the plan. In cloud sessions renv's autoloader
  is switched off (`RENV_CONFIG_AUTOLOADER_ENABLED` in `.claude/settings.json`), so R uses
  the packages that `cloud/setup.sh` installed, and the lockfile governs the laptop.
- **Paths and configuration.** Never hard-code paths. Data paths are built from
  `NANSEN_DATA_ROOT`; each survey and run has one YAML file in `configs/`.
- **Reproducibility.** Every estimate records its configuration hash, code version and
  random seed, and uses the long-format output schema in docs/spec.md, Section 9.
- **Specification.** Sections 2 and 3 of `docs/spec.md` change only with the project
  lead's explicit approval. Decisions are tagged `[D-nn]` and listed in Section 15.
- **Review.** All code is reviewed by a person before merge. Work on a branch; keep
  commits small and their messages descriptive.
