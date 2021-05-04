# gsDesign2 0.1

- Updated AHR vignette to introduce average hazard ratio concept well
- Added arbitrary distribution vignette to demonstrate s2pwe() function
- Corrected calculations in AHR() when using stratified population
- Release for Regulatory/Industry Symposium training

# gsDesign2 0.0.0.9006, December, 2019

- Added eEvents_df() vignette explaining methods thoroughly
- Updated eEvents_df() to simplify output under simple=FALSE option

# gsDesign2 0.0.0.9005, December, 2019

- Updated docs directory to correct reference materials in web site
- Minor fix in eAccrual

# gsDesign2 0.0.0.9004, November, 2019

- Moved new simulation functions to simtrial package (simfix, simfix2simPWSurv, pMaxCombo).

# gsDesign2 0.0.0.9003, November, 2019

- Tried to make AHR and simfix more compatible with each other.
- Improved vignette for group sequential design.
- Added web site for documentation and vignettes in docs/index.html.
- Added support functions for to support approximation using and visualization of the piecewise model.

# gsDesign2 0.0.0.2, October, 2019

- Update to AHR() to output trial duration, expected events and average hazard ratio in a tibble.
- Vignette AHRvignette demonstrating sample size computations for fixed design under non-proportional hazards assumptions.
- Vignette gsNPH demonstrating sample size computations for group sequential design under non-proportional hazards assumptions.
- Initial implementation of pMaxCombo() to compute p-value for MaxCombo test; pMaxComboVignette demonstrates this capability.

# gsDesign2 0.0.0.1, September, 2019

- Computations based on piecewise constant enrollment and piecewise exponential failure rate
- Expected event count calculation for each different hazard ratios in eEvents_df()
- Average hazard ratio computation based on expected event counts in AHR()
- Vignette demonstrating fixed sample size computation with simulation to verify power

