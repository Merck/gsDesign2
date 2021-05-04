Installation
------------

You can install `gsDesign2` with:

    remotes::install_git(
      "https://stash.merck.com/scm/bards/gsdesign2.git",
      credentials = git2r::cred_user_pass(
        rstudioapi::showPrompt("ISID", message = "Enter your ISID"),
        rstudioapi::askForPassword()
      )
    )

Overview
--------

The gsDesign2 package supports recent innovations group sequential
clinical trial design including non-proportional hazards and graphical
multiplicity control with group sequential design. Computations are
based on piecewise constant enrollment and piecewise exponential failure
rates. Stratified populations are supported. Power and sample size
calculations based on using testing based on the logrank test.
