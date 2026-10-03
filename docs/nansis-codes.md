# NANSIS reference codes in Nansen biotic files

Nansen programme biotic files (NMDBiotic v3.1) use NANSIS-specific values in some coded
fields. BAIT's reference tables list these codes, but do not explain the `samplequality`
ones, so they are recorded here. This note holds code meanings only (class C3); it contains
no survey data.

## `samplequality`

Source: the project lead, 3 October 2026, from the programme's coding conventions.

| Code | Meaning | Convention |
| --- | --- | --- |
| 12 | Station can be used for species identification and biomass analysis (Nansis) | Default for demersal surveys, and for bottom trawls in ecosystem or combined surveys |
| 13 | Station can be used for species identification and catch/effort analysis (Nansis) | Not used on standard pelagic, demersal or combined surveys |
| 14 | Station can be used for species identification of target (Nansis) | Default for pelagic surveys, whichever gear is used (pelagic or demersal trawl) |

Codes 1 to 6 keep their general NMD meanings (BAIT, `knowledge/quality-codes.md`); code 5
means that the gear did not fish correctly.

## Related codes

From BAIT's reference tables (`knowledge/quality-codes.md`):

| Field | Code | Meaning |
| --- | --- | --- |
| `stationtype` | 11 | Station to identify an acoustic registration (Nansis) |
| `stationtype` | 12 | Preselected station for swept-area analysis (Nansis) |
| `gearcondition` | 8 | Rigging or deployment problems (NANSIS) |
| `gearcondition` | 9 | Fishing operation aborted (NANSIS) |

## What the first real files show

Counts reported from the laptop, without survey names:

- **Real survey A (pelagic).** Every station is `stationtype` 11 and `samplequality` 14:
  identification stations only, none for biomass analysis. Outside version 1's scope.
- **Real survey B (combined demersal and pelagic).** The three fields agree on every
  station: all `stationtype` 11 stations are `samplequality` 14; `stationtype` 12 stations are
  `samplequality` 12 with `gearcondition` 1, or `samplequality` 5 with `gearcondition` 9
  (aborted tows).
- **Real survey B, further codes** (rerun at commit `03988a1`). `haulvalidity` is 1 on 120
  stations and 3 on one station, one of the three aborted tows; the other two aborted tows
  carry 1. `lengthmeasurement` codes A, B, E, H and Y occur, E on most catch samples with
  lengths; their NMD meanings are still to be confirmed. `catchproducttype` and
  `sampleproducttype` are 1 throughout.

## The whole archive (416 files, 3 October 2026)

Code counts summed over all files, as stations (or catch samples) and the number of surveys
in which each code occurs:

| Field | Code | Stations | Surveys | Note |
| --- | --- | --- | --- | --- |
| `samplequality` | 1 | 29,828 | 348 | General NMD code: design station, normal gear operation |
| `samplequality` | 12 | 5,803 | 170 | NANSIS: biomass analysis |
| `samplequality` | 14 | 2,347 | 174 | NANSIS: species identification of target |
| `samplequality` | 13 | 351 | 50 | NANSIS: catch/effort analysis |
| `samplequality` | 5 | 467 | 143 | Gear did not fish correctly |
| `samplequality` | 100 | 92 | 47 | Deprecated (former experimental-trawl code) |
| `samplequality` | 2, 3, 6, 9, 10 | 187 | up to 10 each | Targeted, gear problems, non-representative, other |
| `stationtype` | 12 | 22,887 | 222 | Preselected swept-area station |
| `stationtype` | 11 | 14,385 | 321 | Identification of an acoustic registration |
| `stationtype` | 2 | 1,189 | 71 | Gear trial |
| `stationtype` | missing | 622 | 52 | |
| `gearcondition` | 1 | 34,953 | 387 | Gear OK |
| `gearcondition` | 101, 102, 103, 106 | 2,722 | up to 90 each | Deprecated acoustic-registration codes |
| `gearcondition` | 9, 8, 5 | 1,289 | up to 132 each | Aborted, rigging problems, codend torn |
| `haulvalidity` | 1, 2, 3 | 3,066 | 41 | Filled in only 41 surveys |

Two conventions therefore coexist: general NMD codes (`samplequality` 1) and NANSIS codes
(12 to 14). Both occur with `stationtype` 12, and `samplequality` 12 occurs with
`stationtype` 11 on some stations. Length-measurement codes E (most fish), B, C, I, J, L, Z
and others occur; their NMD meanings are still to be confirmed.

## Consequence for the inclusion rules (D-09, M2)

A swept-area estimate would select stations with `samplequality` 12 and a normal
`gearcondition`, possibly also requiring `stationtype` 12. D-09 requires the rules behind the
official estimates to be replicated, so the final rule is confirmed against them at M2,
including whether `haulvalidity` (which BAIT describes as conflated with `samplequality`)
plays any part. In survey B it flags only one of the three aborted tows, so on its own it
would not be a sufficient exclusion rule. Across the archive, the rule must also cover
surveys coded with the general NMD convention (`samplequality` 1), the deprecated
`samplequality` 100 and `gearcondition` 101 to 106, and gear-trial stations
(`stationtype` 2).

The synthetic generator uses `stationtype` 12, `samplequality` 12 and `gearcondition` 1 for
its stations.
