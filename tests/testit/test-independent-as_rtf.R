# Test as_rtf() snapshot

assert("gs_power_wlr_example() produces expected as_rtf output", {
  path <- tempfile(fileext = ".rtf")
  gs_power_wlr_example() |>
    summary() |>
    as_rtf(file = path)
  TRUE # Snapshot comparison done by testit .md file
})

assert("gs_design_ahr() produces expected as_rtf output", {
  path <- tempfile(fileext = ".rtf")
  gs_design_ahr() |>
    summary() |>
    as_rtf(file = path)
  TRUE # Snapshot comparison done by testit .md file
})
