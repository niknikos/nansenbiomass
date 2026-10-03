# M1 acceptance record

Milestone M1 (data layer, synthetic generator, airlock) has two acceptance checks
(docs/spec.md, Section 11):

1. Real and synthetic data pass the same schema checks.
2. The disclosure check rejects a deliberately disclosive export.

**Both checks are met** (3 October 2026).

- Check 2 and the synthetic half of check 1 run in a cloud session on synthetic data
  (section 2).
- The real half of check 1 needs real data, which never exist in the cloud. The project
  lead ran it on the laptop, outside Claude Code, on the whole archive of 416 files
  (section 3), under the criterion as revised on 3 October 2026 (section 3, step 4).

M1 is not merged until a person has reviewed the code.

## 1. What M1 delivers

| Module | Exported functions | Purpose |
| --- | --- | --- |
| `data_io` | `survey_schema()`, `read_biotic()`, `read_survey()`, `describe_biotic()`, `validate_survey()`, `check_biotic()`, `data_root()` | Read NMDBiotic v3 XML into four tables; describe a file's structure without values; validate any survey against the schema |
| `synth` | `synth_design()`, `synth_survey()`, `synth_density()`, `write_biotic()` | Generate synthetic surveys from known fields, with their true biomass and abundance; write them as NMDBiotic XML |
| `disclosure` | `estimate_schema()`, `check_disclosure()`, `stage_export()`, `synth_export()` | Check a candidate export against D-03 and stage it for a person to release |

Design points a reviewer should know:

- **Structure.** The four tables (`mission`, `station`, `catch`, `individual`) keep the
  NMDBiotic v3 field names and native keys, as documented in BAIT's data model and field
  glossary (MIT licence, (c) Mikko Vihtakari / IMR). Lengths are in metres, weights in kg.
  After the first real files, the schema also reads the swept-area fields (door spread,
  vessel speed, log, stop time, `haulvalidity`) and the catch fields that decide how catches
  are raised (`raisingfactor`, product types, `lengthmeasurement`). Free-text comment fields
  are never read.
- **NANSIS codes.** The meanings of `samplequality` 12 to 14 and the related station codes
  are recorded in [`nansis-codes.md`](nansis-codes.md).
- **Sanitised errors (CLAUDE.md, rule 8).** `read_survey()` and `describe_biotic()` write
  parser errors and warnings to `<NANSEN_DATA_ROOT>/logs/` and print only a code such as
  `IO-READ-01` and the log file's name. Messages never contain values or file paths.
- **Cloud guard.** When `CLAUDE_CODE_REMOTE` is set, the package reads and stages only
  under `tempdir()`, where synthetic tests write. On the laptop the variable is unset and
  the guard has no effect.
- **D-03, as updated in Section 15.** At least 5 stations, and at least 3 stations with a
  positive catch unless none is positive, behind every released cell; both minimums can be
  raised, never lowered; no release that lets a withheld stratum be recovered from the
  total. The rules restrict what is released. They do not remove any station from
  estimation.
- **Deferred.** The DuckDB input (D-06) and the reuse of BAIT's database layout (D-17);
  the survey configuration schema (M2).

## 2. Cloud check (synthetic data)

**Passed**, with `Status: OK` from `R CMD check`.

| Item | Value |
| --- | --- |
| Date | 29 September 2026; rerun 3 October 2026 after each change |
| Code | branch `claude/elegant-pascal-52da5l`, last code commit `4358fe4` |
| Cloud environment | `nansenbiomass-m0`; setup logs newer than the session, so the current `cloud/setup.sh` ran |
| R and packages | R 4.6.1; the versions recorded in the M0 record and pinned in `renv.lock` |
| New dependencies | None outside `renv.lock`: dplyr, rlang, sf, tibble, withr and xml2 moved to Imports |

`R CMD build` and `R CMD check --no-manual` ran in the session's scratchpad, not in the
working tree. Session charset UTF-8; **`Status: OK`**, no NOTE, WARNING or ERROR; examples
ran; tests `[ FAIL 0 | WARN 0 | SKIP 0 | PASS 165 ]` on 29 September and, after the
changes of 3 October, `[ FAIL 0 | WARN 0 | SKIP 0 | PASS 194 ]`. The first commit, checked on
its own, also passed its tests, with one NOTE: `utils` is declared before the airlock, which
uses it, arrives in the second.

**Check 1, synthetic side.** A synthetic survey (seed 1: 45 stations, 118 catch samples,
4,767 individuals, as generated since 3 October) written to NMDBiotic XML and read back
reproduces all four tables exactly. `validate_survey()` gives 49 checks, 0 fail and 1 warn;
the warning (`IO-CAT-01`) is intended, since the generator splits some catches into two
parts. (The number of checks varies slightly with the data, because the condition-factor
check reports one row per length-measurement type.)
Deliberately corrupted copies fail the expected checks (duplicate key, orphan catch,
negative weight, missing field, impossible latitude, date outside the mission year), and
lengths entered in centimetres raise the unit and plausibility warnings.

**Check 2.** Each deliberately disclosive synthetic export is rejected with its expected
code:

| Disclosive export | Rejected by |
| --- | --- |
| Coordinate and station identifier columns added | `DC-FLD-01`, `DC-FLD-04` |
| An `sf` object with point geometry | `DC-FLD-01`, `DC-FLD-03`, `DC-FLD-04` |
| One row per station | `DC-AGG-01`, or `DC-VAL-01` when rows carry station labels |
| A stratum with 3 stations, or a cell without support | `DC-AGG-02` |
| Total released with exactly one stratum withheld | `DC-AGG-03` |
| A cell resting on 2 positive stations | `DC-AGG-04` |
| Text resembling a coordinate | `DC-VAL-02` |
| Wrong type, missing required value, missing field | `DC-TYP-01`, `DC-TYP-02`, `DC-FLD-02` |

A clean synthetic export passes and is staged with `disclosure =
pass:rules-v1:min5:pos3`. Lowering either minimum is refused, staging into an `outbox`
folder is refused, and a failing rerun removes an earlier passing export.

**Rule 8.** A sentinel string placed in synthetic input appears in the local log but in no
error, warning, printed survey or report. Field names that could carry identifiers (not a
plain identifier, or three or more consecutive digits) are reported as `<name withheld>`.

## 3. Laptop check (real data): steps for a person

These steps run in R on the laptop, outside Claude Code. Nothing from them is committed.

1. Pull `claude/elegant-pascal-52da5l`. In R, in the repository:

   ```r
   renv::status()           # no change expected: no new packages
   testthat::test_local()   # the same tests, on Windows with the lockfile
   ```

   Report the `[ FAIL | WARN | SKIP | PASS ]` line.

2. For each real survey file under `NANSEN_DATA_ROOT` (the path relative to the root):

   ```r
   library(nansenbiomass)        # or pkgload::load_all()
   d <- describe_biotic("<relative path>/biotic.xml")
   d                              # structure and code counts, no values
   v <- read_survey("<relative path>/biotic.xml") |> validate_survey()
   v                              # check codes, status and counts, no values
   ```

3. Review the output, then report back, labelling surveys only as "real survey A", "real
   survey B" and so on:
   - from `describe_biotic`: the namespace version, the names of any fields with
     `in_schema = FALSE`, and, if you are content to share them, the code counts and the
     station code combinations (these inform D-09 at M2);
   - from `validate_survey`: `check_id`, `table`, `field`, `status` and `n_failed` for
     every row that is not `pass`;
   - any sanitised error code (for example `IO-READ-01`).

   Please do not paste log files, values, file paths or survey identifiers.

4. If the reader or a check fails, the code is adapted from those names and counts, and
   the steps are repeated. Warnings are reviewed case by case: some, such as several catch
   parts for one species, are expected in real data.

   **Criterion for check 1, as revised on 3 October 2026.** Check 1 passes when:

   - (a) `validate_survey()` runs the same checks on real and synthetic surveys;
   - (b) every real file is read without error;
   - (c) every structural check passes: `IO-STR-*`, `IO-TYP-*`, `IO-CMP-02`,
     `IO-KEY-01` to `IO-KEY-03` and `IO-REF-*`.

   Value checks (`IO-VAL-*`) that fail on real data are recorded as data-quality findings,
   by count, and handled by the M2 inclusion rules; they do not block M1.

   *History.* The criterion first read "real and synthetic surveys return the same set of
   checks with no `fail`". The archive run showed value failures in real surveys (towed
   distances, depths, door spreads and lengths that are not positive, and dates outside the
   mission year). These describe the source data, which the package must never alter, so
   no version of the code could meet the original wording. The purpose of the check is to
   show that the data layer handles real data correctly, and (a) to (c) test exactly that.
   The project lead approved the revision, and the reclassification of `IO-KEY-04` as a
   warning, on 3 October 2026, after seeing the results.

### Laptop result: whole archive (416 files)

Run by the project lead on 3 October 2026 at commit `866dee3`, with `check_biotic()` and a
resumable loop that saves each file's result in the data zone. Surveys are labelled
`real survey 001` to `416` in file-name order; the key from labels to files stays on the
laptop.

| Item | Result |
| --- | --- |
| Files | 416, all read without error |
| Format | NMDBiotic v3.1 in every file |
| Surveys with biomass stations (`samplequality` 12) | 170 |
| Structural checks | All pass in every file, except `IO-KEY-04` (repeated specimen numbers) in 11 surveys, now a warning (below) |

**Value checks that failed** (totals over all surveys):

| Check | Surveys | Records failing | Of records checked |
| --- | --- | --- | --- |
| `IO-VAL-04` towed distance not positive | 69 | 928 stations | 7,548 |
| `IO-VAL-06` bottom depth not positive | 24 | 90 stations | 1,924 |
| `IO-VAL-07` station date outside the mission year | 8 | 22 stations | 760 |
| `IO-VAL-08` door spread not positive | 4 | 13 stations | 253 |
| `IO-VAL-05` length not positive | 8 | 231 fish | 390,243 |

**Warnings** (totals over all surveys):

| Check | Surveys | Records flagged | Of records checked |
| --- | --- | --- | --- |
| `IO-CAT-01` several catch samples per species and station | 361 | 27,719 | 614,628 |
| `IO-CMP-01` catch sample without `catchweight` | 252 | 17,127 | 494,547 |
| `IO-CMP-01` catch sample without `catchcategory` | 1 | 1 | 624 |
| `IO-RAI-01` length-sample count above catch count | 192 | 1,064 | 76,828 |
| `IO-RAI-02` length-sample weight above catch weight | 156 | 995 | 61,979 |
| `IO-RAI-03` measured fish differ from length-sample count | 96 | 1,421 | 39,816 |
| `IO-RAI-04` raising factor other than 1 | 41 | 35,800 | 85,562 |
| `IO-UNIT-01` length above 3 m | 9 | 14 | 101,556 |
| `IO-KEY-04` repeated specimen numbers (failure at the time of the run) | 11 | 6,882 | 347,110 |

`IO-PLA-01` (condition factor outside 0.02 to 10), by length-measurement code:

| Code | Surveys flagged | Fish flagged | Of fish checked |
| --- | --- | --- | --- |
| E | 47 | 4,681 | 414,120 |
| no code | 36 | 47,725 | 149,509 |
| C | 32 | 29,899 | 31,405 |
| B | 34 | 4,437 | 19,130 |
| L | 10 | 347 | 359 |
| J | 4 | 27 | 1,886 |
| K, M, A, G, H, F, I, Z | 1 to 3 each | 69 in all | 3,030 |

The 14 lengths above 3 m are isolated (one to five per survey, among about 102,000 fish in
those surveys): they look like individual entry errors, not centimetres recorded as metres,
which would flag whole catch samples.

**Reader verification for `IO-KEY-04`.** For the survey with the most repeated specimen
numbers, each fish's catch sample and station were also read directly from its parent
elements, which is slow but cannot misassign a fish. Both methods gave 25,461 fish, no fish
assigned differently, and the same 1,363 repeated keys. The repetition is therefore in the
source files: in most affected surveys, distinct fish share a specimen number within a
catch sample, a numbering practice that also explains why few rows are fully identical.
Each record is kept as a separate fish; `IO-KEY-04` is now a warning.

**Duplicated catch samples.** If two catch samples ever share a key (an `IO-KEY-03`
failure, which did not occur in the archive), the condition-factor check does not choose
between them: fish whose duplicated catch samples disagree on the length-measurement code
are reported as `lengthmeasurement = ambiguous`. Choosing between duplicated records is a
question about the data; from M2, the estimation pipeline should refuse such a survey
rather than keep one of the rows.

**Outcome.** Under the revised criterion (step 4), acceptance check 1 is met: the same checks
run on real and synthetic data, all 416 files are read without error, and every structural
check passes. The value failures are carried into the M2 agenda (section 4).

### Laptop result: first two surveys

Reported by the project lead, up to 3 October 2026, run at commit `a3341c0` (48 checks at
that version). These runs came before the archive run and led to the schema additions.

| | Real survey A | Real survey B |
| --- | --- | --- |
| Type | Pelagic | Combined demersal and pelagic |
| Namespace | NMDBiotic v3.1 | NMDBiotic v3.1 |
| Stations, catch samples, individuals | 78, 755, 14,081 | 121, 3,435, 7,615 |
| Checks failed | 0 of 48 | 0 of 48 |
| `IO-PLA-01` condition factor outside 0.02 to 10 | warn, 27 of 14,079 | warn, 232 of 7,331 |
| `IO-CAT-01` several catch samples per species and station | warn, 10 | warn, 147 |
| `IO-RAI-03` measured fish differ from length-sample count | pass | warn, 1 of 777 |
| `IO-CMP-01` catch sample without `catchweight` | warn, 1 of 755 | warn, 9 of 3,435 |

Both surveys passed the same checks as the synthetic surveys, with no failure and no
change to the reader.

**Tests on the laptop.** `testthat::test_local()` at commit `03988a1` (Windows, lockfile
library): `[ FAIL 0 | WARN 0 | SKIP 0 | PASS 181 ]`. Later commits were tested in the cloud;
a final laptop run before merging is advisable.

**Adaptations made after these runs (3 October 2026).** No check failed, so nothing had to
be fixed. The warnings and the list of fields outside the schema led to these additions:

- the swept-area and raising fields listed in section 1 were added to the schema;
- `IO-PLA-01` now reports by `lengthmeasurement` type, since carapace, mantle and diameter
  lengths give extreme condition factors without being errors;
- a new warning, `IO-RAI-04`, counts catch samples with a raising factor other than 1, which
  the estimate must raise; a new check, `IO-VAL-08`, requires a positive door spread;
- `describe_biotic()` also counts `haulvalidity`, `lengthmeasurement`, `catchproducttype`
  and `sampleproducttype` codes and tabulates station code combinations;
- the synthetic generator now follows the NANSIS swept-area convention (`samplequality` 12).

**Rerun of real survey B at commit `03988a1`.** 54 checks, 0 fail, 7 warn. The extended
schema read every new field without conversion failures; the door spread was positive on
all stations.

- `IO-PLA-01` by length-measurement type (mapping to codes inferred from the report's sort
  order, to be confirmed): A 14 of 27 flagged, B 215 of 1,559, E 3 of 5,727, H 0 of 9,
  Y 0 of 9. The warnings sit almost entirely in two measurement types, consistent with
  non-fish lengths; type E, which covers most fish, has 3 flags, plausibly genuine entry
  errors. The meanings of the codes are still to be confirmed.
- `IO-RAI-04`: 1,138 of 3,435 catch samples (33%) have a raising factor other than 1.
- `haulvalidity`: code 1 on 120 stations and code 3 on 1, which is one of the three aborted
  tows; the other two aborted tows carry code 1. `samplequality` and `gearcondition` flag
  all three.
- `catchproducttype` is 1 on every catch sample, and `sampleproducttype` is 1 wherever it
  is filled.

**renv on the laptop.** `renv::status()` reported packages recorded but not used (the
Suggests and development stack, under `snapshot.type = "implicit"`), the sdmTMB stack not
installed, and patch-level differences in R's recommended packages and `s2`. None affects
M1. Decision of 3 October 2026: the lockfile is tidied at the start of M2 (keep the
Suggests stack pinned with `snapshot.dev`, install the sdmTMB stack, snapshot).

## 4. Data-quality agenda for M2

The archive run gives the starting points for the StoX pipeline. Each is to be settled
against the rules behind the official estimates (D-09, D-11), not decided here.

1. **Inclusion rule (D-09).** The archive mixes two station-code conventions: general NMD
   codes (`samplequality` 1 on about 29,800 stations in 348 surveys) and NANSIS codes
   (`samplequality` 12 in 170 surveys, 14 in 174). Deprecated codes still occur
   (`samplequality` 100; `gearcondition` 101 to 106 on about 2,700 stations), and gear-trial
   stations (`stationtype` 2) number about 1,200. See [`nansis-codes.md`](nansis-codes.md).
2. **Towed distance.** 928 stations in 69 surveys have a distance that is not positive. Check
   whether any are biomass stations; if so, whether the distance can be recovered from the
   log or the positions.
3. **Raising.** 41 surveys raise catches (42% of their catch samples). Establish whether
   `catchweight` holds the raised catch or the subsample weight to be multiplied by
   `raisingfactor`.
4. **Catch parts.** 4.5% of species-by-station combinations, in 361 surveys, have several
   catch samples. Establish when parts are disjoint (to be summed) and when they overlap.
5. **Missing catch weights.** 3.5% of catch samples (252 surveys) have no `catchweight`.
   Decide how they enter the totals (excluded, or weight from count).
6. **Condition-factor check.** Restrict it to the length-measurement type for fish total
   length (probably E) once the NMD meanings of the codes are confirmed; report the others
   for information.
7. **Minor.** Depths, door spreads, dates and lengths that fail the value checks; the raising
   inputs that disagree (1 to 3% of records checked); 14 lengths above 3 m; repeated
   specimen numbers in 11 surveys. To be reviewed with the people who maintain the data.

## 5. Limitations

- **Real data checked once, with this code version.** All 416 files read without
  adaptation; new files, or files from before the current archive, may still differ.
  Section 3 can be rerun at any time without the data leaving the laptop.
- **No schema (XSD) validation.** Files are read by element name. A file that is valid XML
  but departs from the XSD is caught only through the checks above.
- **Reading speed.** The reader was made about seven times faster on 3 October 2026, after
  a first archive run took over five hours; a large synthetic file (about 9,000 fish) now
  reads in under a second in the cloud. The duration of the archive run with the faster
  reader was not recorded.
- **The positive-station minimum of 3** was proposed with this milestone and confirmed by
  the project lead on 3 October 2026; configurations may raise it. Positive stations are
  counted by species code (`catchcategory`), and the same counts apply to biomass and
  abundance cells.
- **Differencing** is checked within one survey, year, species, method and quantity.
  Differencing across quantities, methods or later releases is not checked.
- **The support table** (stations and positive stations per cell) is produced by
  `synth_export()` for synthetic data only; the pipelines produce it from M2 onwards.
- **The synthetic truth** is integrated on a 1 km grid. For a constant field it equals
  density times area exactly; otherwise the grid error is small but not zero.
