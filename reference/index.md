# Package index

## Fixed design

Functions to calculate power/sample size in fixed designs.

- [`fixed_design_ahr()`](https://merck.github.io/gsDesign2/reference/fixed_design.md)
  [`fixed_design_fh()`](https://merck.github.io/gsDesign2/reference/fixed_design.md)
  [`fixed_design_lf()`](https://merck.github.io/gsDesign2/reference/fixed_design.md)
  [`fixed_design_maxcombo()`](https://merck.github.io/gsDesign2/reference/fixed_design.md)
  [`fixed_design_mb()`](https://merck.github.io/gsDesign2/reference/fixed_design.md)
  [`fixed_design_milestone()`](https://merck.github.io/gsDesign2/reference/fixed_design.md)
  [`fixed_design_rd()`](https://merck.github.io/gsDesign2/reference/fixed_design.md)
  [`fixed_design_rmst()`](https://merck.github.io/gsDesign2/reference/fixed_design.md)
  : Fixed design under non-proportional hazards

## Average hazard ratio

Functions for the average hazard ratio (AHR) method.

- [`ahr()`](https://merck.github.io/gsDesign2/reference/ahr.md) :
  Average hazard ratio under non-proportional hazards
- [`expected_time()`](https://merck.github.io/gsDesign2/reference/expected_time.md)
  : Predict time at which a targeted event count is achieved
- [`expected_event()`](https://merck.github.io/gsDesign2/reference/expected_event.md)
  : Expected events observed under piecewise exponential model
- [`gs_info_ahr()`](https://merck.github.io/gsDesign2/reference/gs_info_ahr.md)
  : Information and effect size based on AHR approximation
- [`gs_power_ahr()`](https://merck.github.io/gsDesign2/reference/gs_power_ahr.md)
  : Group sequential design power using average hazard ratio under
  non-proportional hazards
- [`gs_design_ahr()`](https://merck.github.io/gsDesign2/reference/gs_design_ahr.md)
  : Calculate sample size and bounds given targeted power and Type I
  error in group sequential design using average hazard ratio under
  non-proportional hazards
- [`gs_update_ahr()`](https://merck.github.io/gsDesign2/reference/gs_update_ahr.md)
  : Group sequential design using average hazard ratio under
  non-proportional hazards
- [`ahr_blinded()`](https://merck.github.io/gsDesign2/reference/ahr_blinded.md)
  : Blinded estimation of average hazard ratio

## Weighted logrank

Functions for the weighted logrank test (WLR) method.

- [`wlr_weight_fh()`](https://merck.github.io/gsDesign2/reference/wlr_weight.md)
  [`wlr_weight_1()`](https://merck.github.io/gsDesign2/reference/wlr_weight.md)
  [`wlr_weight_n()`](https://merck.github.io/gsDesign2/reference/wlr_weight.md)
  [`wlr_weight_mb()`](https://merck.github.io/gsDesign2/reference/wlr_weight.md)
  : Weight functions for weighted log-rank test
- [`gs_info_wlr()`](https://merck.github.io/gsDesign2/reference/gs_info_wlr.md)
  : Information and effect size for weighted log-rank test
- [`gs_power_wlr()`](https://merck.github.io/gsDesign2/reference/gs_power_wlr.md)
  : Group sequential design power using weighted log rank test under
  non-proportional hazards
- [`gs_design_wlr()`](https://merck.github.io/gsDesign2/reference/gs_design_wlr.md)
  : Group sequential design using weighted log-rank test under
  non-proportional hazards

## MaxCombo

Functions for the MaxCombo method.

- [`gs_info_combo()`](https://merck.github.io/gsDesign2/reference/gs_info_combo.md)
  : Information and effect size for MaxCombo test
- [`gs_spending_combo()`](https://merck.github.io/gsDesign2/reference/gs_spending_combo.md)
  : Derive spending bound for MaxCombo group sequential boundary
- [`gs_power_combo()`](https://merck.github.io/gsDesign2/reference/gs_power_combo.md)
  : Group sequential design power using MaxCombo test under
  non-proportional hazards
- [`gs_design_combo()`](https://merck.github.io/gsDesign2/reference/gs_design_combo.md)
  : Group sequential design using MaxCombo test under non-proportional
  hazards

## Risk difference

Functions for risk differences.

- [`gs_info_rd()`](https://merck.github.io/gsDesign2/reference/gs_info_rd.md)
  : Information and effect size under risk difference
- [`gs_power_rd()`](https://merck.github.io/gsDesign2/reference/gs_power_rd.md)
  : Group sequential design power of binary outcome measuring in risk
  difference
- [`gs_design_rd()`](https://merck.github.io/gsDesign2/reference/gs_design_rd.md)
  : Group sequential design of binary outcome measuring in risk
  difference

## Conditional power

Functions for conditional power.

- [`gs_cp_npe()`](https://merck.github.io/gsDesign2/reference/gs_cp_npe.md)
  : Conditional power computation with non-constant effect size

## Input definition

Helper functions to define inputs for study design.

- [`define_enroll_rate()`](https://merck.github.io/gsDesign2/reference/define_enroll_rate.md)
  : Define enrollment rate
- [`define_fail_rate()`](https://merck.github.io/gsDesign2/reference/define_fail_rate.md)
  : Define failure rate

## Summary and display tables

Functions to summarize fixed / group sequential design results.

- [`summary(`*`<fixed_design>`*`)`](https://merck.github.io/gsDesign2/reference/summary.md)
  [`summary(`*`<gs_design>`*`)`](https://merck.github.io/gsDesign2/reference/summary.md)
  : Summary for fixed design or group sequential design objects
- [`text_summary()`](https://merck.github.io/gsDesign2/reference/text_summary.md)
  : Generates a textual summary of a group sequential design using the
  AHR method.
- [`as_gt()`](https://merck.github.io/gsDesign2/reference/as_gt.md) :
  Convert summary table of a fixed or group sequential design object to
  a gt object
- [`as_rtf()`](https://merck.github.io/gsDesign2/reference/as_rtf.md) :
  Write summary table of a fixed or group sequential design object to an
  RTF file
- [`gs_bound_summary()`](https://merck.github.io/gsDesign2/reference/gs_bound_summary.md)
  : Bound summary table
- [`to_integer()`](https://merck.github.io/gsDesign2/reference/to_integer.md)
  : Round sample size and events

## Boundary functions

Functions to specify the upper and lower bound in group sequential
designs. They are not recommended to use alone. Instead, they should be
used companied with gs_design_npe, gs_power_npe, ect..

- [`gs_b()`](https://merck.github.io/gsDesign2/reference/gs_b.md) :
  Default boundary generation
- [`gs_spending_bound()`](https://merck.github.io/gsDesign2/reference/gs_spending_bound.md)
  : Derive spending bound for group sequential boundary

## Expected …

Functions for computing trial events.

- [`expected_event()`](https://merck.github.io/gsDesign2/reference/expected_event.md)
  : Expected events observed under piecewise exponential model
- [`expected_time()`](https://merck.github.io/gsDesign2/reference/expected_time.md)
  : Predict time at which a targeted event count is achieved
- [`expected_accrual()`](https://merck.github.io/gsDesign2/reference/expected_accrual.md)
  : Piecewise constant expected accrual

## Piecewise exponential

Functions for computing piecewise exponential distributions.

- [`ppwe()`](https://merck.github.io/gsDesign2/reference/ppwe.md) :
  Piecewise exponential cumulative distribution function
- [`s2pwe()`](https://merck.github.io/gsDesign2/reference/s2pwe.md) :
  Approximate survival distribution with piecewise exponential
  distribution

## Low-level helpers

Functions to calculate sample size or number of events under
non-constant treatment effect over time.

- [`gs_design_npe()`](https://merck.github.io/gsDesign2/reference/gs_power_design_npe.md)
  [`gs_power_npe()`](https://merck.github.io/gsDesign2/reference/gs_power_design_npe.md)
  : Group sequential design computation with non-constant effect and
  information.
- [`gs_create_arm()`](https://merck.github.io/gsDesign2/reference/gs_create_arm.md)
  : Create npsurvSS arm object
- [`pw_info()`](https://merck.github.io/gsDesign2/reference/pw_info.md)
  : Average hazard ratio under non-proportional hazards
