# StoX project templates

This folder holds the versioned StoX project templates from which the
`stox_sweptarea` module generates a project for each survey and run
(docs/spec.md, Section 6). Projects are generated from a template and a survey
configuration in `configs/`, never edited by hand.

Templates are structural (class C3): they contain no survey data.

## `sweptarea/template.json`

The swept-area chain for StoX 4.2 (RstoxFramework 4.2.1): an ordered list of
processes, each with its model, StoX function, inputs and fixed parameters.
Parameters written as `{{name}}` are filled from the survey configuration by
`build_stox_project()`; a placeholder left unfilled is an error. A process with
a `when` key applies only when that condition holds (`biomass`, `abundance`,
`distance_translation`).

- **Two branches.** Biomass comes from catch weight (`SpeciesCategoryCatch` with a
  total-catch swept-area density) and abundance from length distributions.
  Changing how either quantity is obtained means a new template version, which
  is recorded in `code_version`.
- **Constant sweep width.** StoX 4.2.1 takes a constant sweep width through
  `SweptAreaDensity`; a haul-specific door spread is not supported.
- **Stations.** The filter keeps the stations selected by the shared inclusion
  rules, by key, with `FilterUpwards` so that PSUs exist only for kept stations.
- **Totals.** Strata outside the survey definition (`includeintotal = false`) carry
  no `Survey` label and drop out of the totals, which are grouped by `Survey`.
