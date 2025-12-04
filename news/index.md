# Changelog

## gsDesign2 1.1.7

CRAN release: 2025-11-19

### Documentation

- The vignette discussing the futility boundary design is finalized
  ([\#583](https://github.com/Merck/gsDesign2/issues/583), thanks to
  [@LittleBeannie](https://github.com/LittleBeannie))
- An example calculating expected events for multiple strata is added
  ([\#597](https://github.com/Merck/gsDesign2/issues/597), thanks to
  [@LittleBeannie](https://github.com/LittleBeannie))
- The documentation of common parameters is unified by `@inheritparams`
  ([\#598](https://github.com/Merck/gsDesign2/issues/598), thanks to
  [@LittleBeannie](https://github.com/LittleBeannie))
- A contributing guide with overview of package architecture is added
  ([\#596](https://github.com/Merck/gsDesign2/issues/596), thanks to
  [@jdblischak](https://github.com/jdblischak))

### Programming improvements

- The S3 classes are re-factored
  ([\#584](https://github.com/Merck/gsDesign2/issues/584), thanks to
  [@jdblischak](https://github.com/jdblischak))
- The input parameters are not displayed when printing the objects
  ([\#586](https://github.com/Merck/gsDesign2/issues/586), thanks to
  [@jdblischak](https://github.com/jdblischak))
- [`text_summary()`](https://merck.github.io/gsDesign2/reference/text_summary.md)
  supports design objects with spending functions specified as character
  strings ([\#587](https://github.com/Merck/gsDesign2/issues/587),
  thanks to [@yihui](https://github.com/yihui)).
- The attribute assignment `uninteger_is_from` is fixed in
  [`to_integer()`](https://merck.github.io/gsDesign2/reference/to_integer.md)
  ([\#591](https://github.com/Merck/gsDesign2/issues/591), thanks to
  [@jdblischak](https://github.com/jdblischak))
- [`text_summary()`](https://merck.github.io/gsDesign2/reference/text_summary.md)
  is updated to support fixed designs from
  [`gs_design_ahr()`](https://merck.github.io/gsDesign2/reference/gs_design_ahr.md)
  ([\#592](https://github.com/Merck/gsDesign2/issues/592), thanks to
  [@jdblischak](https://github.com/jdblischak))

## gsDesign2 1.1.6

CRAN release: 2025-09-11

### Statistical improvements

- The
  [`gs_power_wlr()`](https://merck.github.io/gsDesign2/reference/gs_power_wlr.md)
  function now includes an `h1_spending` argument, allowing users to
  specify a spending under the alternative hypothesis
  ([\#565](https://github.com/Merck/gsDesign2/issues/565), thanks to
  [@LittleBeannie](https://github.com/LittleBeannie)).
- The following functions now support `info_scale` argument:
  [`fixed_design_ahr()`](https://merck.github.io/gsDesign2/reference/fixed_design.md),
  [`fixed_design_fh()`](https://merck.github.io/gsDesign2/reference/fixed_design.md),
  [`fixed_design_mb()`](https://merck.github.io/gsDesign2/reference/fixed_design.md),
  and
  [`fixed_design_rd()`](https://merck.github.io/gsDesign2/reference/fixed_design.md)
  ([\#571](https://github.com/Merck/gsDesign2/issues/571), thanks to
  [@LittleBeannie](https://github.com/LittleBeannie)).
- Functions for fixed designs with integer sample sizes now return the
  average HR in their output, providing a more complete summary of the
  design characteristics.
  ([\#572](https://github.com/Merck/gsDesign2/issues/572), thanks to
  [@LittleBeannie](https://github.com/LittleBeannie)).

### Documentation

- The documentation for
  [`gs_design_npe()`](https://merck.github.io/gsDesign2/reference/gs_power_design_npe.md)
  and
  [`gs_power_npe()`](https://merck.github.io/gsDesign2/reference/gs_power_design_npe.md)
  has been consolidated into a single topic for improved clarity and
  easier navigation.
  ([\#567](https://github.com/Merck/gsDesign2/issues/567), thanks to
  [@LittleBeannie](https://github.com/LittleBeannie)).
- The package codebase has been updated to use the native R pipe (`|>`)
  exclusively, removing the `magrittr` dependency and aligning with
  modern R practices.
  ([\#577](https://github.com/Merck/gsDesign2/issues/577), thanks to
  [@jdblischak](https://github.com/jdblischak)).

## gsDesign2 1.1.5

CRAN release: 2025-06-27

### Bug fixes

- The spending of the WLR design has been corrected. The default
  spending of a WLR design is information fraction.
  ([\#557](https://github.com/Merck/gsDesign2/issues/557), thanks to
  [@LittleBeannie](https://github.com/LittleBeannie)).

## gsDesign2 1.1.4

CRAN release: 2025-06-06

### New functions

- A new function,
  [`gs_cp_npe()`](https://merck.github.io/gsDesign2/reference/gs_cp_npe.md),
  is now available for calculating simple conditional power under NPH. A
  vignette has been published on the pkgdown website
  ([\#510](https://github.com/Merck/gsDesign2/issues/510),
  [\#539](https://github.com/Merck/gsDesign2/issues/539),
  [\#545](https://github.com/Merck/gsDesign2/issues/545), thanks to
  [@LittleBeannie](https://github.com/LittleBeannie)).
- A new design summary function
  [`gs_bound_summary()`](https://merck.github.io/gsDesign2/reference/gs_bound_summary.md)
  is available with similar output structure as
  [`gsDesign::gsBoundSummary()`](https://keaven.github.io/gsDesign/reference/gsBoundSummary.html)
  and supporting for multiple alpha
  ([\#468](https://github.com/Merck/gsDesign2/issues/468),
  [\#522](https://github.com/Merck/gsDesign2/issues/522),
  [\#537](https://github.com/Merck/gsDesign2/issues/537), thanks to
  [@jdblischak](https://github.com/jdblischak)).
- Textual summary of AHR designs are available via the
  [`text_summary()`](https://merck.github.io/gsDesign2/reference/text_summary.md)
  ([\#526](https://github.com/Merck/gsDesign2/issues/526), thanks to
  [@LittleBeannie](https://github.com/LittleBeannie)).

### User interface improvements

- The integer event is rounded in IAs and rounded up in FA as defaults
  in
  [`to_integer()`](https://merck.github.io/gsDesign2/reference/to_integer.md)
  ([\#483](https://github.com/Merck/gsDesign2/issues/483), thanks to
  [@LittleBeannie](https://github.com/LittleBeannie)).
- The integer sample size is rounded to the nearest multiple of
  randomization ratio + 1 when `round_up_final = TRUE`
  ([\#488](https://github.com/Merck/gsDesign2/issues/488), thanks to
  [@LittleBeannie](https://github.com/LittleBeannie))..
- The updated design can be derived with the input of events per
  analysis per interval
  ([\#499](https://github.com/Merck/gsDesign2/issues/499), thanks to
  [@LittleBeannie](https://github.com/LittleBeannie)).
- Bound functions and spending functions can be provided through their
  names (character strings) now, e.g.,
  `gs_design_ahr(..., upper = "gs_spending_bound", upar = list(sf = "sfLDOF", ...))`
  ([\#509](https://github.com/Merck/gsDesign2/issues/509), thanks to
  [@yihui](https://github.com/yihui)).
- The `footnote` argument of
  [`as_gt()`](https://merck.github.io/gsDesign2/reference/as_gt.md) can
  take the value `FALSE` to disable footnotes
  ([\#514](https://github.com/Merck/gsDesign2/issues/514), thanks to
  [@yihui](https://github.com/yihui)).
- Optimized the functions `expected_accural()`,
  [`expected_time()`](https://merck.github.io/gsDesign2/reference/expected_time.md),
  [`gs_design_ahr()`](https://merck.github.io/gsDesign2/reference/gs_design_ahr.md),
  [`gs_design_combo()`](https://merck.github.io/gsDesign2/reference/gs_design_combo.md),
  [`gs_design_npe()`](https://merck.github.io/gsDesign2/reference/gs_power_design_npe.md),
  [`gs_design_wlr()`](https://merck.github.io/gsDesign2/reference/gs_design_wlr.md),
  [`pw_info()`](https://merck.github.io/gsDesign2/reference/pw_info.md),
  [`ppwe()`](https://merck.github.io/gsDesign2/reference/ppwe.md),
  `s2pe()`, and `gs_bound()`
  ([\#528](https://github.com/Merck/gsDesign2/issues/528), thanks to
  [@yihui](https://github.com/yihui)).
- Weights of a WLR design functions can be provided as string now
  ([\#533](https://github.com/Merck/gsDesign2/issues/533), thanks to
  [@LittleBeannie](https://github.com/LittleBeannie)).

### Bug fixes

- Correct the statistical information of WLR-integer designs in
  [`to_integer()`](https://merck.github.io/gsDesign2/reference/to_integer.md)
  ([\#478](https://github.com/Merck/gsDesign2/issues/478), thanks to
  [@LittleBeannie](https://github.com/LittleBeannie)).
- Correct the calculation `info_frac` in
  [`gs_design_wlr()`](https://merck.github.io/gsDesign2/reference/gs_design_wlr.md)
  when `info_scale = "h0_info"`
  ([\#485](https://github.com/Merck/gsDesign2/issues/485),
  [\#486](https://github.com/Merck/gsDesign2/issues/486), thanks to
  [@LittleBeannie](https://github.com/LittleBeannie)).
- Add the `h1_spending` argument to
  [`gs_power_ahr()`](https://merck.github.io/gsDesign2/reference/gs_power_ahr.md)
  ([\#518](https://github.com/Merck/gsDesign2/issues/518), thanks to
  [@LittleBeannie](https://github.com/LittleBeannie))

### Testing

- Add the developer tests of
  [`gs_cp_npe()`](https://merck.github.io/gsDesign2/reference/gs_cp_npe.md)
  ([\#519](https://github.com/Merck/gsDesign2/issues/519), thanks to
  [@shiyuskaya](https://github.com/shiyuskaya))

## gsDesign2 1.1.3

CRAN release: 2024-11-15

### Bug fixes

- Fix the bug of
  [`gs_design_rd()`](https://merck.github.io/gsDesign2/reference/gs_design_rd.md)
  when `info_scale = "h0_h1_info"`
  ([\#402](https://github.com/Merck/gsDesign2/issues/402), thanks to
  [@LittleBeannie](https://github.com/LittleBeannie)).
- Fix the bug of
  [`gs_spending_combo()`](https://merck.github.io/gsDesign2/reference/gs_spending_combo.md)
  to enable HSD spending function
  ([\#444](https://github.com/Merck/gsDesign2/issues/444), thanks to
  [@LittleBeannie](https://github.com/LittleBeannie)).
- Fix the bug of
  [`fixed_design_maxcombo()`](https://merck.github.io/gsDesign2/reference/fixed_design.md)
  regarding the upper bounds
  ([\#445](https://github.com/Merck/gsDesign2/issues/445), thanks to
  [@elong0527](https://github.com/elong0527)).
- Fix the bug of
  [`gs_design_wlr()`](https://merck.github.io/gsDesign2/reference/gs_design_wlr.md)
  when the design is driven by information fraction only
  ([\#446](https://github.com/Merck/gsDesign2/issues/446), thanks to
  [@LittleBeannie](https://github.com/LittleBeannie)).
- Fix the bug of
  [`pw_info()`](https://merck.github.io/gsDesign2/reference/pw_info.md)
  when there are many piecewise HRs
  ([\#460](https://github.com/Merck/gsDesign2/issues/460), thanks to
  [@LittleBeannie](https://github.com/LittleBeannie)).

### Statistical improvements

- The
  [`gs_update_ahr()`](https://merck.github.io/gsDesign2/reference/gs_update_ahr.md)
  function (test version) is updated to

  - Enable unchanged futility bounds when it is fixed at the original
    design ([\#408](https://github.com/Merck/gsDesign2/issues/408),
    thanks to [@LittleBeannie](https://github.com/LittleBeannie)).
  - Allow boundary updates when only certain interim analysis data is
    available ([\#436](https://github.com/Merck/gsDesign2/issues/436),
    thanks to [@LittleBeannie](https://github.com/LittleBeannie)).
  - Allow boundary updates when alpha changes by keeping the same
    `info_scale` as the original design
    ([\#470](https://github.com/Merck/gsDesign2/issues/470),
    [@LittleBeannie](https://github.com/LittleBeannie)).

- Rounding of integer design is updated
  ([\#488](https://github.com/Merck/gsDesign2/issues/488),
  [\#484](https://github.com/Merck/gsDesign2/issues/484),
  [\#486](https://github.com/Merck/gsDesign2/issues/486), thanks to
  [@LittleBeannie](https://github.com/LittleBeannie)).

- Integer design (i.e., integer sample size and events) is updated to
  ensure exact integer sample size and events
  ([\#452](https://github.com/Merck/gsDesign2/issues/452),
  [\#460](https://github.com/Merck/gsDesign2/issues/460), thanks to
  [@LittleBeannie](https://github.com/LittleBeannie) and
  [@yihui](https://github.com/yihui)).

- Change the information fraction displayed at the summary-gt table from
  under H1 to H0 for logrank tests
  ([\#439](https://github.com/Merck/gsDesign2/issues/439), thanks to
  [@LittleBeannie](https://github.com/LittleBeannie)).

- Add the sample size as the output of
  [`ahr()`](https://merck.github.io/gsDesign2/reference/ahr.md) and
  [`pw_info()`](https://merck.github.io/gsDesign2/reference/pw_info.md)
  ([\#427](https://github.com/Merck/gsDesign2/issues/427),
  [\#433](https://github.com/Merck/gsDesign2/issues/433), thanks to
  [@LittleBeannie](https://github.com/LittleBeannie)).

### User interface improvements

- Enable passing named vector of `col_decimals` and `analysis_decimals`
  to
  [`summary.gs_design()`](https://merck.github.io/gsDesign2/reference/summary.md)
  ([\#403](https://github.com/Merck/gsDesign2/issues/403),
  [\#431](https://github.com/Merck/gsDesign2/issues/431),
  [@jdblischak](https://github.com/jdblischak)).

### Coding practice improvements

- Add robust check if `lower` is equivalent to `gs_b`
  ([\#413](https://github.com/Merck/gsDesign2/issues/413), thanks to
  [@jdblischak](https://github.com/jdblischak) )
- The [`summary()`](https://rdrr.io/r/base/summary.html),
  [`as_gt()`](https://merck.github.io/gsDesign2/reference/as_gt.md)
  [`as_rtf()`](https://merck.github.io/gsDesign2/reference/as_rtf.md),
  and
  [`to_integer()`](https://merck.github.io/gsDesign2/reference/to_integer.md)
  functions are refactored
  ([\#448](https://github.com/Merck/gsDesign2/issues/448),
  [\#449](https://github.com/Merck/gsDesign2/issues/449),
  [\#450](https://github.com/Merck/gsDesign2/issues/450),
  [\#465](https://github.com/Merck/gsDesign2/issues/465),
  [\#461](https://github.com/Merck/gsDesign2/issues/461), thanks to
  [@yihui](https://github.com/yihui)).
- Remove the `full_alpha` argument from `as_rtf.gs_design()`
  ([\#458](https://github.com/Merck/gsDesign2/issues/458), thanks to
  [@yihui](https://github.com/yihui)).
- Other coding practice improvement worth mentioning
  ([\#409](https://github.com/Merck/gsDesign2/issues/409),
  [\#412](https://github.com/Merck/gsDesign2/issues/412),
  [\#429](https://github.com/Merck/gsDesign2/issues/429),
  [\#437](https://github.com/Merck/gsDesign2/issues/437),
  [\#440](https://github.com/Merck/gsDesign2/issues/440),
  [\#447](https://github.com/Merck/gsDesign2/issues/447),
  [\#453](https://github.com/Merck/gsDesign2/issues/453),
  [\#464](https://github.com/Merck/gsDesign2/issues/464),
  [\#467](https://github.com/Merck/gsDesign2/issues/467), \$475, thanks
  to [@yihui](https://github.com/yihui),
  [@nanxstats](https://github.com/nanxstats), and
  [@jdblischak](https://github.com/jdblischak)).

### Documentation

- Update the documentation of
  [`gs_b()`](https://merck.github.io/gsDesign2/reference/gs_b.md)
  ([\#415](https://github.com/Merck/gsDesign2/issues/415),
  [@jdblischak](https://github.com/jdblischak))

### Testing

- More developer tests of
  [`gs_power_ahr()`](https://merck.github.io/gsDesign2/reference/gs_power_ahr.md)
  are added ([\#420](https://github.com/Merck/gsDesign2/issues/420),
  [@LittleBeannie](https://github.com/LittleBeannie)).
- More developer tests of
  [`summary()`](https://rdrr.io/r/base/summary.html) are added
  ([\#422](https://github.com/Merck/gsDesign2/issues/422),
  [\#426](https://github.com/Merck/gsDesign2/issues/426), thanks to
  [@yuliasidi](https://github.com/yuliasidi),
  [@jdblischak](https://github.com/jdblischak) and
  [@LittleBeannie](https://github.com/LittleBeannie)).
- Independent tests of
  [`ahr_blinded()`](https://merck.github.io/gsDesign2/reference/ahr_blinded.md)
  are added ([\#435](https://github.com/Merck/gsDesign2/issues/435),
  thanks to [@DMuriuki](https://github.com/DMuriuki)).
- More developer tests of
  [`to_integer()`](https://merck.github.io/gsDesign2/reference/to_integer.md)
  are added ([\#476](https://github.com/Merck/gsDesign2/issues/476),
  thanks to [@LittleBeannie](https://github.com/LittleBeannie)).

## gsDesign2 1.1.2

CRAN release: 2024-04-09

### New features

- The
  [`gs_update_ahr()`](https://merck.github.io/gsDesign2/reference/gs_update_ahr.md)
  function is now available for efficacy and futility boundary update
  based on blinded estimation of treatment effect
  ([\#370](https://github.com/Merck/gsDesign2/issues/370)).

### Bug fixes

- Fix the accrual parameters bugs in
  [`gs_design_wlr()`](https://merck.github.io/gsDesign2/reference/gs_design_wlr.md)
  depending on npsurvSS
  ([\#344](https://github.com/Merck/gsDesign2/issues/344),
  [\#356](https://github.com/Merck/gsDesign2/issues/356)).
- Fix
  [`gs_design_ahr()`](https://merck.github.io/gsDesign2/reference/gs_design_ahr.md)
  to incorporate information fraction driven design when number of
  analyses \>= 4
  ([\#358](https://github.com/Merck/gsDesign2/issues/358)).

### Improvements

- Zero failure rate in some but not all intervals is acceptable as input
  ([\#360](https://github.com/Merck/gsDesign2/issues/360)).
- Study with duration \> 100 units are executable when event accrual is
  slow ([\#368](https://github.com/Merck/gsDesign2/issues/368)).

### Documentation

- A new
  [vignette](https://merck.github.io/gsDesign2/articles/story-update-boundary.html)
  introducing how to do the boundary update is available
  ([\#278](https://github.com/Merck/gsDesign2/issues/278),
  [\#364](https://github.com/Merck/gsDesign2/issues/364),
  [\#366](https://github.com/Merck/gsDesign2/issues/366)).
- A new
  [vignette](https://merck.github.io/gsDesign2/articles/story-seven-test-types.html)
  bridging gsDesign2 to the 6 test types of gsDesign is available.
- The pkgdown website is re-organized to providing better view for users
  ([\#341](https://github.com/Merck/gsDesign2/issues/341)).

### Testing

- Independent testing of
  [`as_gt()`](https://merck.github.io/gsDesign2/reference/as_gt.md) is
  added ([\#337](https://github.com/Merck/gsDesign2/issues/337)).
- Restructure tests to make them self-contained
  ([\#347](https://github.com/Merck/gsDesign2/issues/347)).

## gsDesign2 1.1.1

CRAN release: 2024-02-09

### New features

- The
  [`as_rtf()`](https://merck.github.io/gsDesign2/reference/as_rtf.md)
  method is now available for `fixed_design` and `gs_design` objects for
  generating RTF table outputs
  ([\#278](https://github.com/Merck/gsDesign2/issues/278)).

### Bug fixes

- [`gs_power_wlr()`](https://merck.github.io/gsDesign2/reference/gs_power_wlr.md)
  and
  [`to_integer()`](https://merck.github.io/gsDesign2/reference/to_integer.md)
  now check and convert integer sample size more rigorously
  ([\#322](https://github.com/Merck/gsDesign2/issues/322)).
- `gs_design_*()` now handle exceptions explicitly when all hazard ratio
  is set to 1 throughout the study
  ([\#301](https://github.com/Merck/gsDesign2/issues/301)).
- [`fixed_design_rd()`](https://merck.github.io/gsDesign2/reference/fixed_design.md)
  will not generate warnings due to the previous default value change of
  `h1_spending`
  ([\#296](https://github.com/Merck/gsDesign2/issues/296)).

### Improvements

- [`gs_power_ahr()`](https://merck.github.io/gsDesign2/reference/gs_power_ahr.md)
  now runs twice as fast by using data.table and other performance
  optimizations
  ([\#295](https://github.com/Merck/gsDesign2/issues/295)), enhanced by
  similar improvements in
  [`gs_info_ahr()`](https://merck.github.io/gsDesign2/reference/gs_info_ahr.md)
  and
  [`pw_info()`](https://merck.github.io/gsDesign2/reference/pw_info.md)
  ([\#300](https://github.com/Merck/gsDesign2/issues/300)).
- Enrollment and failure rate input constructors and validators are
  refactored to check only the format instead of the class. This change
  reduces the number of warning messages and catches real exceptions as
  errors properly
  ([\#316](https://github.com/Merck/gsDesign2/issues/316)).
- Nested functions are refactored into reusable internal functions, to
  improve code rigor, avoid potential scoping pitfalls, and facilitate
  debugging ([\#235](https://github.com/Merck/gsDesign2/issues/235)).
- For fixed designs, the variable names of the table outputs from
  [`to_integer()`](https://merck.github.io/gsDesign2/reference/to_integer.md)
  and [`summary()`](https://rdrr.io/r/base/summary.html) are updated
  ([\#292](https://github.com/Merck/gsDesign2/issues/292)).

### Documentation

- Add a new vignette [statistical information under null and alternative
  hypothesis](https://merck.github.io/gsDesign2/articles/story-info-formula.html)
  ([\#289](https://github.com/Merck/gsDesign2/issues/289)).
- Improve
  [`define_enroll_rate()`](https://merck.github.io/gsDesign2/reference/define_enroll_rate.md)
  and
  [`define_fail_rate()`](https://merck.github.io/gsDesign2/reference/define_fail_rate.md)
  documentation by adding detailed descriptions and improving code
  examples ([\#302](https://github.com/Merck/gsDesign2/issues/302)).
- The function reference page now has dedicated sections for piecewise
  exponential distributions and computing trial events
  ([\#258](https://github.com/Merck/gsDesign2/issues/258)).
- Use the four trailing dashes convention to standardize code comment
  section format
  ([\#308](https://github.com/Merck/gsDesign2/issues/308)).

### Namespace and testing

- Tidy up namespace by removing rlang from and adding stats to `Imports`
  ([\#307](https://github.com/Merck/gsDesign2/issues/307),
  [\#325](https://github.com/Merck/gsDesign2/issues/325)).
- Qualify namespaces in tests to avoid
  [`library()`](https://rdrr.io/r/base/library.html) calls
  ([\#332](https://github.com/Merck/gsDesign2/issues/332)).
- Fortify the GitHub Actions workflows by limiting the token usage only
  when necessary and enabling manual trigger of workflow runs
  ([\#326](https://github.com/Merck/gsDesign2/issues/326)).
- Update GitHub Actions workflows to the latest versions from upstream
  ([\#330](https://github.com/Merck/gsDesign2/issues/330)).

## gsDesign2 1.1.0

CRAN release: 2023-08-23

### Breaking changes

- Split `fixed_design()` into a group of `fixed_design_*()` functions
  for enhanced modularity
  ([\#263](https://github.com/Merck/gsDesign2/issues/263)).
- [`gs_design_rd()`](https://merck.github.io/gsDesign2/reference/gs_design_rd.md)
  and
  [`gs_power_rd()`](https://merck.github.io/gsDesign2/reference/gs_power_rd.md)
  now have updated options of weighting for stratified design
  ([\#276](https://github.com/Merck/gsDesign2/issues/276)).
- [`ppwe()`](https://merck.github.io/gsDesign2/reference/ppwe.md) now
  accepts two arguments `duration` and `rate` instead of a data frame
  `fail_rate` ([\#254](https://github.com/Merck/gsDesign2/issues/254)).
- Unexport helper functions `gridpts()`, `h1()`, and `hupdate()`
  ([\#253](https://github.com/Merck/gsDesign2/issues/253)).

### New features

- Introduce
  [`define_enroll_rate()`](https://merck.github.io/gsDesign2/reference/define_enroll_rate.md)
  and
  [`define_fail_rate()`](https://merck.github.io/gsDesign2/reference/define_fail_rate.md)
  as new input constructor functions to replace the tibble inputs
  ([\#238](https://github.com/Merck/gsDesign2/issues/238)).
- Add a new function
  [`pw_info()`](https://merck.github.io/gsDesign2/reference/pw_info.md)
  which calculates the statistical information under the piecewise model
  ([\#262](https://github.com/Merck/gsDesign2/issues/262)).

### Improvements

- Add a
  [vignette](https://merck.github.io/gsDesign2/articles/story-canonical-h0-h1.html)
  showing the canonical joint distribution of Z-score and B-values under
  null and alternative hypothesis for the AHR test
  ([\#246](https://github.com/Merck/gsDesign2/issues/246)).
- Refactor
  [`expected_event()`](https://merck.github.io/gsDesign2/reference/expected_event.md)
  to improve computational performance
  ([@jdblischak](https://github.com/jdblischak),
  [\#250](https://github.com/Merck/gsDesign2/issues/250)).
- Move the source code of the legacy version from `inst/` to
  `tests/testthat/` as developer tests
  ([\#269](https://github.com/Merck/gsDesign2/issues/269)).

## gsDesign2 1.0.9

CRAN release: 2023-06-20

### Improvements

- Add CRAN download counts badge
  ([\#215](https://github.com/Merck/gsDesign2/issues/215)).
- Update documentation of
  [`gs_design_rd()`](https://merck.github.io/gsDesign2/reference/gs_design_rd.md)
  ([\#220](https://github.com/Merck/gsDesign2/issues/220)).
- Format footnote numbers using decimal notation
  ([\#222](https://github.com/Merck/gsDesign2/issues/222)).
- Split C++ functions into individual `.cpp` and header files
  ([\#224](https://github.com/Merck/gsDesign2/issues/224)).

### Bug fixes

- Fix the digits display in
  [`summary()`](https://rdrr.io/r/base/summary.html)
  ([\#231](https://github.com/Merck/gsDesign2/issues/231)).

## gsDesign2 1.0.8

CRAN release: 2023-05-01

### Improvements

- Update the calculation of upper/lower bounds at the final analysis in
  MaxCombo tests
  ([\#217](https://github.com/Merck/gsDesign2/issues/217)).
- Update the `fixed_design()` function in the application of stratified
  design when using the Lachin and Foulkes method
  ([\#211](https://github.com/Merck/gsDesign2/issues/211)).
- Correct the `fixed_design()` function in the application of `rmst`
  ([\#212](https://github.com/Merck/gsDesign2/issues/212)).
- Rename the `info_scale` argument options from `c(0, 1, 2)` to
  `c("h0_h1_info", "h0_info", "h1_info")` to be more informative and
  make the default value (`"h0_h1_info"`) clear
  ([\#203](https://github.com/Merck/gsDesign2/issues/203)).
- Add missing global functions/variables
  ([\#213](https://github.com/Merck/gsDesign2/issues/213)).
- Fix outdated argument names and use canonical style for text elements
  in `README.md`
  ([\#198](https://github.com/Merck/gsDesign2/issues/198)).
- Add a CRAN downloads badge to `README.md` to show the monthly
  downloads ([\#216](https://github.com/Merck/gsDesign2/issues/216)).

### Bug fixes

- Fix the calculation of the futility bounds in
  [`gs_power_ahr()`](https://merck.github.io/gsDesign2/reference/gs_power_ahr.md)
  ([\#202](https://github.com/Merck/gsDesign2/issues/202)).

## gsDesign2 1.0.7

CRAN release: 2023-03-20

### Improvements

- Move imported dependencies from `Suggests` to `Imports`.
- Remove redundant dependencies from `Suggests`.
- Update the GitHub Actions workflows to their latest versions from
  upstream.
- Add a rule to `.gitattributes` for GitHub Linguist to keep the
  repository’s language statistics accurate.

## gsDesign2 1.0.6

### Improvements

- Export functions `gridpts()`, `h1()`, `hupdate()`, and
  [`gs_create_arm()`](https://merck.github.io/gsDesign2/reference/gs_create_arm.md)
  to avoid the use of `:::` in code examples.
- Fix the write path issue by moving the test fixture generation script
  to `data-raw/` which is not included in the package.

## gsDesign2 1.0.5

First submission to CRAN in March 2023.

### Breaking changes

- Passes lintr check for the entire package
  ([\#150](https://github.com/Merck/gsDesign2/issues/150),
  [\#151](https://github.com/Merck/gsDesign2/issues/151),
  [\#171](https://github.com/Merck/gsDesign2/issues/171)).
- Improve the documentation
  ([\#161](https://github.com/Merck/gsDesign2/issues/161),
  [\#163](https://github.com/Merck/gsDesign2/issues/163),
  [\#168](https://github.com/Merck/gsDesign2/issues/168),
  [\#176](https://github.com/Merck/gsDesign2/issues/176)).

### Bug fixes

- `check_fail_rate()` when only 1 number in `fail_rate` is \> 0
  ([\#132](https://github.com/Merck/gsDesign2/issues/132)).
- [`gs_power_ahr()`](https://merck.github.io/gsDesign2/reference/gs_power_ahr.md)
  when study duration is \> 48 months
  ([\#141](https://github.com/Merck/gsDesign2/issues/141)).
- `fixed_design()` for event-based design
  ([\#143](https://github.com/Merck/gsDesign2/issues/143)).
- [`gs_design_combo()`](https://merck.github.io/gsDesign2/reference/gs_design_combo.md)
  when test only applies to part of the analysis
  ([\#148](https://github.com/Merck/gsDesign2/issues/148)).
- [`gs_info_rd()`](https://merck.github.io/gsDesign2/reference/gs_info_rd.md)
  for variance calculation
  ([\#153](https://github.com/Merck/gsDesign2/issues/153)).
- [`summary()`](https://rdrr.io/r/base/summary.html) for capitalized
  first letter in the summary header
  ([\#164](https://github.com/Merck/gsDesign2/issues/164)).

## gsDesign2 1.0.0

GitHub release in December 2022.

### Breaking changes

- Merges [gsDesign2
  v0.2.1](https://github.com/Merck/gsDesign2/tree/v0.2.1) and
  [gsdmvn](https://github.com/Merck/gsdmvn).
- Updates API to follow the new style guide in `vignette("style")`. See
  the detailed mapping between the old API and new API in
  [\#84](https://github.com/Merck/gsDesign2/issues/84).

### New features

- Supports organized summary tables and gt tables.
- Power/sample size calculation for risk difference.
- Integer sample size support
  ([\#116](https://github.com/Merck/gsDesign2/issues/116),
  [\#125](https://github.com/Merck/gsDesign2/issues/125)).
- Adds `fixed_design()` to implement different methods for power/sample
  size calculation.
- Adds `info_scale` arguments to `gs_design_*()` and `gs_power_*()`.
- Adds RMST and milestone methods to fixed design.

### Bug fixes

- [`expected_accrual()`](https://merck.github.io/gsDesign2/reference/expected_accrual.md)
  for stratified population.
- [`gs_spending_bound()`](https://merck.github.io/gsDesign2/reference/gs_spending_bound.md)
  when IA is close to FA
  ([\#40](https://github.com/Merck/gsDesign2/issues/40)).
- `gs_power_bound()` when applied in the MaxCombo test
  ([\#62](https://github.com/Merck/gsDesign2/issues/62)).
- [`gs_design_npe()`](https://merck.github.io/gsDesign2/reference/gs_power_design_npe.md)
  for type I error
  ([\#59](https://github.com/Merck/gsDesign2/issues/59)).

### Minor improvements

- Adds and re-organizes vignettes.

## gsDesign2 0.2.1

GitHub release in August 2022.

- The release before merging with `Merck/gsdmvn`.

## gsDesign2 0.2.0

GitHub release in May 2022.

- Supports the *Biometrical Journal* paper “A unified framework for
  weighted parametric group sequential design” by Keaven M. Anderson,
  Zifang Guo, Jing Zhao, and Linda Z. Sun.

## gsDesign2 0.1.0

GitHub release in May 2021.

- Updated AHR vignette to introduce average hazard ratio concept
  properly.
- Added arbitrary distribution vignette to demonstrate
  [`s2pwe()`](https://merck.github.io/gsDesign2/reference/s2pwe.md).
- Corrected calculations in `AHR()` when using stratified population.
- Release for Regulatory/Industry Symposium training.

## gsDesign2 0.0.0.9006

GitHub release in December 2019.

- Added vignette for `eEvents_df()` explaining the methods thoroughly.
- Updated `eEvents_df()` to simplify output under option
  `simple = FALSE`.

## gsDesign2 0.0.0.9005

GitHub release in December 2019.

- Updated `docs/` directory to correct the reference materials on the
  website.
- Minor fixes in `eAccrual()`.

## gsDesign2 0.0.0.9004

GitHub release in November 2019.

- Moved new simulation functions to the simtrial package (`simfix()`,
  `simfix2simPWSurv()`, `pMaxCombo()`).

## gsDesign2 0.0.0.9003

GitHub release in November 2019.

- Tried to make `AHR()` and `simfix()` more compatible with each other.
- Improved vignette for group sequential design.
- Added pkgdown website for documentation and vignettes.
- Added support functions for to support approximation using and
  visualization of the piecewise model.

## gsDesign2 0.0.0.2

GitHub release in October 2019.

- Update `AHR()` to output trial duration, expected events and average
  hazard ratio in a tibble.
- Vignette AHRvignette demonstrating sample size computations for fixed
  design under non-proportional hazards assumptions.
- Vignette gsNPH demonstrating sample size computations for group
  sequential design under non-proportional hazards assumptions.
- Initial implementation of `pMaxCombo()` to compute p-value for
  MaxCombo test; pMaxComboVignette demonstrates this capability.

## gsDesign2 0.0.0.1

GitHub release in September 2019.

- Computations based on piecewise constant enrollment and piecewise
  exponential failure rate.
- Expected event count calculation for each different hazard ratios in
  `eEvents_df()`.
- Average hazard ratio computation based on expected event counts in
  `AHR()`.
- Vignette demonstrating fixed sample size computation with simulation
  to verify power.
