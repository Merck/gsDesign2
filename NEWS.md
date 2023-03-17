# gsDesign2 1.0.7

## Improvements

- Move imported dependencies from `Suggests` to `Imports`.
- Remove redundant dependencies from `Suggests`.
- Update the GitHub Actions workflows to their latest versions from upstream.
- Add a rule to `.gitattributes` for GitHub Linguist to keep the repository's
  language statistics accurate.

# gsDesign2 1.0.6

## Improvements

- Export functions `gridpts()`, `h1()`, `hupdate()`, and `gs_create_arm()`
  to avoid the use of `:::` in code examples.
- Fix the write path issue by moving the test fixture generation script to
  `data-raw/` which is not included in the package.

# gsDesign2 1.0.5

First submission to CRAN in March 2023.

## Breaking changes

- Passes lintr check for the entire package (#150, #151, #171).
- Improve the documentation (#161, #163, #168, #176).

## Bug fixes

- `check_fail_rate()` when only 1 number in `fail_rate` is > 0 (#132).
- `gs_power_ahr()` when study duration is > 48 months (#141).
- `fixed_design()` for event-based design (#143).
- `gs_design_combo()` when test only applies to part of the analysis (#148).
- `gs_info_rd()` for variance calculation (#153).
- `summary()` for capitalized first letter in the summary header (#164).

# gsDesign2 1.0.0

GitHub release in December 2022.

## Breaking changes

- Merges [gsDesign2 v0.2.1](https://github.com/Merck/gsDesign2/tree/v0.2.1)
  and [gsdmvn](https://github.com/Merck/gsdmvn).
- Updates API to follow the new style guide in `vignette("style")`.
  See the detailed mapping between the old API and new API in #84.

## New features

- Supports organized summary tables and gt tables.
- Power/sample size calculation for risk difference.
- Integer sample size support (#116, #125).
- Adds `fixed_design()` to implement different methods for power/sample size calculation.
- Adds `info_scale` arguments to `gs_design_*()` and `gs_power_*()`.
- Adds RMST and milestone methods to fixed design.

## Bug fixes

- `expected_accrual()` for stratified population.
- `gs_spending_bound()` when IA is close to FA (#40).
- `gs_power_bound()` when applied in the MaxCombo test (#62).
- `gs_design_npe()` for type I error (#59).

## Minor improvements

- Adds and re-organizes vignettes.

# gsDesign2 0.2.1

GitHub release in August 2022.

- The release before merging with `Merck/gsdmvn`.

# gsDesign2 0.2.0

GitHub release in May 2022.

- Supports the _Biometrical Journal_ paper "A unified framework for weighted parametric group sequential design" by Keaven M. Anderson, Zifang Guo, Jing Zhao, and Linda Z. Sun.

# gsDesign2 0.1.0

GitHub release in May 2021.

- Updated AHR vignette to introduce average hazard ratio concept properly.
- Added arbitrary distribution vignette to demonstrate `s2pwe()`.
- Corrected calculations in `AHR()` when using stratified population.
- Release for Regulatory/Industry Symposium training.

# gsDesign2 0.0.0.9006

GitHub release in December 2019.

- Added vignette for `eEvents_df()` explaining the methods thoroughly.
- Updated `eEvents_df()` to simplify output under option `simple = FALSE`.

# gsDesign2 0.0.0.9005

GitHub release in December 2019.

- Updated `docs/` directory to correct the reference materials on the website.
- Minor fixes in `eAccrual()`.

# gsDesign2 0.0.0.9004

GitHub release in November 2019.

- Moved new simulation functions to the simtrial package
  (`simfix()`, `simfix2simPWSurv()`, `pMaxCombo()`).

# gsDesign2 0.0.0.9003

GitHub release in November 2019.

- Tried to make `AHR()` and `simfix()` more compatible with each other.
- Improved vignette for group sequential design.
- Added pkgdown website for documentation and vignettes.
- Added support functions for to support approximation using and visualization
  of the piecewise model.

# gsDesign2 0.0.0.2

GitHub release in October 2019.

- Update `AHR()` to output trial duration, expected events and average hazard ratio in a tibble.
- Vignette AHRvignette demonstrating sample size computations for fixed design under non-proportional hazards assumptions.
- Vignette gsNPH demonstrating sample size computations for group sequential design under non-proportional hazards assumptions.
- Initial implementation of `pMaxCombo()` to compute p-value for MaxCombo test; pMaxComboVignette demonstrates this capability.

# gsDesign2 0.0.0.1

GitHub release in September 2019.

- Computations based on piecewise constant enrollment and piecewise exponential failure rate.
- Expected event count calculation for each different hazard ratios in `eEvents_df()`.
- Average hazard ratio computation based on expected event counts in `AHR()`.
- Vignette demonstrating fixed sample size computation with simulation to verify power.
