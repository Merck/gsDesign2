% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ahr_blinded.R
\name{ahr_blinded}
\alias{ahr_blinded}
\title{Blinded estimation of average hazard ratio}
\usage{
ahr_blinded(
  surv = survival::Surv(time = simtrial::ex1_delayed_effect$month, event =
    simtrial::ex1_delayed_effect$evntd),
  intervals = c(3, Inf),
  hr = c(1, 0.6),
  ratio = 1
)
}
\arguments{
\item{surv}{Input survival object (see \code{\link[survival:Surv]{survival::Surv()}});
note that only 0 = censored, 1 = event for \code{\link[survival:Surv]{survival::Surv()}}.}

\item{intervals}{Vector containing positive values indicating
interval lengths where the exponential rates are assumed.
Note that a final infinite interval is added if any events occur
after the final interval specified.}

\item{hr}{Vector of hazard ratios assumed for each interval.}

\item{ratio}{Ratio of experimental to control randomization.}
}
\value{
A \code{tibble} with one row containing
\itemize{
\item \code{ahr} - Blinded average hazard ratio based on assumed period-specific
hazard ratios input in \code{fail_rate} and observed events in the
corresponding intervals.
\item \code{event} - Total observed number of events.
\item \code{info0} - Information under related null hypothesis.
\item \code{theta} - Natural parameter for group sequential design representing
expected incremental drift at all analyses.
}
}
\description{
Based on blinded data and assumed hazard ratios in different intervals,
compute a blinded estimate of average hazard ratio (AHR) and corresponding
estimate of statistical information.
This function is intended for use in computing futility bounds based on
spending assuming the input hazard ratio (hr) values for intervals
specified here.
}
\section{Specification}{

\if{latex}{
 \itemize{
   \item Validate input hr is a numeric vector.
   \item Validate input hr is non-negative.
   \item Validate input intervals is a numeric vector > 0.
   \item Set final value in intervals to Inf
   \item Validate that hr and intervals have same length.
   \item For input time-to-event data, count number of events in each input interval by stratum.
   \item Compute the blinded estimate of average hazard ratio.
   \item Compute adjustment for information.
   \item Return a tibble of the sum of events, average hazard ratio,
   blinded average hazard ratio, and the information.
  }
}
\if{html}{The contents of this section are shown in PDF user manual only.}
}

\examples{
ahr_blinded(
  surv = survival::Surv(
    time = simtrial::ex2_delayed_effect$month,
    event = simtrial::ex2_delayed_effect$evntd
  ),
  intervals = c(4, 100),
  hr = c(1, .55),
  ratio = 1
)
}
