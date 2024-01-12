#  Copyright (c) 2024 Merck & Co., Inc., Rahway, NJ, USA and its affiliates.
#  All rights reserved.
#
#  This file is part of the gsDesign2 program.
#
#  gsDesign2 is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <http://www.gnu.org/licenses/>.

#' Default boundary generation
#'
#' `gs_b()` is the simplest version of a function to be used with the
#' `upper` and `lower` arguments in `gs_prob()`,
#' `gs_power_nph()` and `gs_design_nph()`;
#' it simply returns the vector input in the input vector `Z` or,
#' if `k` is specified, `par[k]j` is returned.
#' Note that if bounds need to change with changing information at analyses,
#' `gs_b()` should not be used.
#' For instance, for spending function bounds use.
#'
#' @param par For `gs_b()`, this is just Z-values for the boundaries;
#'   can include infinite values.
#' @param k Is `NULL` (default), return `par`, else return `par[k]`.
#' @param ... Further arguments passed to or from other methods.
#'
#' @return Returns the vector input `par` if `k` is `NULL`, otherwise, `par[k]`.
#'
#' @section Specification:
#' \if{latex}{
#'  \itemize{
#'    \item Validate if the input k is null as default.
#'    \itemize{
#'      \item If the input k is null as default, return the whole vector of
#'      Z-values of the boundaries.
#'      \item If the input k is not null, return the corresponding boundary
#'      in the vector of Z-values.
#'      }
#'    \item Return a vector of boundaries.
#'   }
#' }
#' \if{html}{The contents of this section are shown in PDF user manual only.}
#'
#' @export
#'
#' @examples
#' # Simple: enter a vector of length 3 for bound
#' gs_b(par = 4:2)
#'
#' # 2nd element of par
#' gs_b(par = 4:2, k = 2)
#'
#' # Generate an efficacy bound using a spending function
#' # Use Lan-DeMets spending approximation of O'Brien-Fleming bound
#' # as 50%, 75% and 100% of final spending
#' # Information fraction
#' IF <- c(.5, .75, 1)
#' gs_b(par = gsDesign::gsDesign(
#'   alpha = .025, k = length(IF),
#'   test.type = 1, sfu = gsDesign::sfLDOF,
#'   timing = IF
#' )$upper$bound)
gs_b <- function(par = NULL, k = NULL, ...) {
  if (is.null(k)) {
    return(par)
  } else {
    return(par[k])
  }
}
