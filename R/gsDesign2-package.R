#  Copyright (c) 2025 Merck & Co., Inc., Rahway, NJ, USA and its affiliates.
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

#' @importFrom data.table ":=" as.data.table copy first last rbindlist setDF
#'   setDT setcolorder setorderv
#' @importFrom dplyr "%>%" all_of arrange desc filter full_join group_by lead
#'   left_join mutate one_of rename row_number select summarize ungroup
#' @importFrom gsDesign gsDesign sfLDOF
#' @importFrom stats pnorm qnorm setNames uniroot
#' @importFrom tibble tibble
#' @import utils
#' @importFrom Rcpp sourceCpp
#' @useDynLib gsDesign2, .registration = TRUE
#' @keywords internal
"_PACKAGE"
