# Release history of `gsDesign2`
The latest version of `gsDesign2` is `v1.0.1` as a 2022 Christmas gift. Please install it by

```
devtools::install_github("Merck/gsDesign2@v1.0.0")
```

## v1.0.0, Dec, 2022
- This version merges the [gsDesign2 v0.2.1](https://github.com/Merck/gsDesign2/tree/v0.2.1) and [gsdmvn](https://github.com/Merck/gsdmvn).
- It supports organized summary tables and gt tables;
- It has power/sample size calculation for risk difference;
- It has functionality for integer sample size;
- It updates API (see the 1:1 map between the old API vs. new API in https://github.com/Merck/gsDesign2/issues/84);
- It fixes historical small bugs like 
  - `expected_accrual` for the stratified population;
  - `gs_spending_bound` when [IA is close to FA](https://github.com/Merck/gsDesign2/issues/40);
  - `gs_power_bound` when [applied in the max combo test](https://github.com/Merck/gsDesign2/issues/62); 
  - `gs_design_npe` for [type I error](https://github.com/Merck/gsDesign2/issues/59);
  - ...
- It develops `fixed_design` to implement different methods for power/sample size calculation;
- It adds `info_scale` arguments to `gs_design_xxx` and `gs_power_xxx`;
- It adds RMST and milstone methods into fixed design;
- It adds and re-organizes vignettes.

## v0.2.1, Aug, 2022

- This version is the one before the big merge with `Merck/gsdmvn`.

## v0.2.0, May, 2022

- This version supports the Biometrical Journal paper "A unified framework for weighted parametric group sequential design (WPGSD)" by Keaven M. Anderson, Zifang Guo, Jing Zhao, and Linda Z. Sun.

## v0.1.0, May, 2021

- Updated AHR vignette to introduce average hazard ratio concept well
- Added arbitrary distribution vignette to demonstrate s2pwe() function
- Corrected calculations in AHR() when using stratified population
- Release for Regulatory/Industry Symposium training

## v0.0.0.9006, December, 2019

- Added eEvents_df() vignette explaining methods thoroughly
- Updated eEvents_df() to simplify output under simple=FALSE option

## v0.0.0.9005, December, 2019

- Updated docs directory to correct reference materials in web site
- Minor fix in eAccrual

## v0.0.0.9004, November, 2019

- Moved new simulation functions to simtrial package (simfix, simfix2simPWSurv, pMaxCombo).

## v0.0.0.9003, November, 2019

- Tried to make AHR and simfix more compatible with each other.
- Improved vignette for group sequential design.
- Added web site for documentation and vignettes in docs/index.html.
- Added support functions for to support approximation using and visualization of the piecewise model.

## v0.0.0.2, October, 2019

- Update to AHR() to output trial duration, expected events and average hazard ratio in a tibble.
- Vignette AHRvignette demonstrating sample size computations for fixed design under non-proportional hazards assumptions.
- Vignette gsNPH demonstrating sample size computations for group sequential design under non-proportional hazards assumptions.
- Initial implementation of pMaxCombo() to compute p-value for MaxCombo test; pMaxComboVignette demonstrates this capability.

## v0.0.0.1, September, 2019

- Computations based on piecewise constant enrollment and piecewise exponential failure rate
- Expected event count calculation for each different hazard ratios in eEvents_df()
- Average hazard ratio computation based on expected event counts in AHR()
- Vignette demonstrating fixed sample size computation with simulation to verify power
