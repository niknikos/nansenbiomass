# StoX project templates

This folder holds the versioned StoX project templates from which the
`stox_sweptarea` module generates a project for each survey and run
(docs/spec.md, Section 6). Projects are generated from a template and a survey
configuration in `configs/`, never edited by hand.

Templates are structural (class C3): they contain no survey data.

## `sweptarea/template.json` (version 2.1.0)

The swept-area chain for StoX 4.2 (RstoxFramework 4.2.1): an ordered list of
processes, each with its model, StoX function, inputs and fixed parameters.
Parameters written as `{{name}}` are filled from the survey configuration by
`build_stox_project()`; a placeholder left unfilled is an error. A process with
a `when` key applies only when that condition holds.

- **Stations and species.** One filter keeps the stations selected by the shared
  inclusion rules, by key, with `FilterUpwards` so that PSUs exist only for kept
  stations. A second filter keeps the configured species with `FilterUpwards`
  false, so that hauls without the species remain in the stratum means (zero
  catch).
- **Abundance** comes from length distributions (optionally regrouped to the
  configured length group), with one PSU per station.
- **Biomass has two routes**, chosen by `biomass.method`: `total_catch` (catch
  weight, with its own bootstrap) and `super_individuals` (`Individuals`,
  `SuperIndividuals`, `ImputeSuperIndividuals`, as in many StoX projects; the bootstrap
  resamples the mean length distribution and the imputation has its own seed).
  Changing how either route works means a new template version, which is recorded
  in `code_version`.
- **Constant sweep width.** StoX 4.2.1 takes a constant sweep width through
  `SweptAreaDensity`; a haul-specific door spread is not supported.
- **Totals.** Strata outside the survey definition (`includeintotal = false`) carry
  no `Survey` label and drop out of the totals, which are grouped by `Survey`.
- **Missing values.** The reports of the super-individual route remove missing values
  (individuals without a weight), as the official reports do.
- **Units.** Catch-weight biomass is in kg and super-individual biomass in grams;
  the report conversion gives tonnes.
