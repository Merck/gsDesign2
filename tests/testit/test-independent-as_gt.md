## `fixed_design()` summary `as_gt()`

```r
enroll_rate <- define_enroll_rate(duration = 18, rate = 20)
  fail_rate <- define_fail_rate(
    duration = c(4, 100),
    fail_rate = log(2) / 12,
    dropout_rate = .001, hr = c(1, .6)
  )

  output <- fixed_design_ahr(
    alpha = 0.025,
    power = 1 - 0.1,
    enroll_rate = enroll_rate,
    fail_rate = fail_rate,
    study_duration = 36,
    ratio = 1
  ) |>
    summary() |>
    as_gt()
gt_to_latex(output)
```

```
\begin{table}[t]
\caption*{
{\fontsize{20}{25}\selectfont  Fixed Design under AHR Method\textsuperscript{\textit{1}}\fontsize{12}{15}\selectfont }
} 
\fontsize{12.0pt}{14.0pt}\selectfont
\begin{tabular*}{\linewidth}{@{\extracolsep{\fill}}lrrrrrrr}
\toprule
Design & N & Events & Time & AHR & Bound & alpha & Power \\ 
\midrule\addlinespace[2.5pt]
Average hazard ratio & 463.078 & 324.7077 & 36 & 0.697102 & 1.959964 & 0.025 & 0.9 \\ 
\bottomrule
\end{tabular*}
\begin{minipage}{\linewidth}
\vspace{.05em}
\textsuperscript{\textit{1}}Power computed with average hazard ratio method.\\
\end{minipage}
\end{table}
```

## `fixed_design()` summary `as_gt()` with custom title and footnote

```r
enroll_rate <- define_enroll_rate(duration = 18, rate = 20)
  fail_rate <- define_fail_rate(
    duration = c(4, 100),
    fail_rate = log(2) / 12,
    dropout_rate = .001, hr = c(1, .6)
  )

  output <- fixed_design_ahr(
    alpha = 0.025,
    power = 1 - 0.1,
    enroll_rate = enroll_rate,
    fail_rate = fail_rate,
    study_duration = 36,
    ratio = 1
  ) |>
    summary() |>
    as_gt(title = "Custom Title", footnote = "Custom footnote.")
gt_to_latex(output)
```

```
\begin{table}[t]
\caption*{
{\fontsize{20}{25}\selectfont  Custom Title\textsuperscript{\textit{1}}\fontsize{12}{15}\selectfont }
} 
\fontsize{12.0pt}{14.0pt}\selectfont
\begin{tabular*}{\linewidth}{@{\extracolsep{\fill}}lrrrrrrr}
\toprule
Design & N & Events & Time & AHR & Bound & alpha & Power \\ 
\midrule\addlinespace[2.5pt]
Average hazard ratio & 463.078 & 324.7077 & 36 & 0.697102 & 1.959964 & 0.025 & 0.9 \\ 
\bottomrule
\end{tabular*}
\begin{minipage}{\linewidth}
\vspace{.05em}
\textsuperscript{\textit{1}}Custom footnote.\\
\end{minipage}
\end{table}
```

## `fixed_design_fh()` summary `as_gt()`

```r
enroll_rate <- define_enroll_rate(
    duration = 18,
    rate = 20
  )
  fail_rate <- define_fail_rate(
    duration = c(4, 100),
    fail_rate = log(2) / 12,
    dropout_rate = .001,
    hr = c(1, .6)
  )

  output <- fixed_design_fh(
    alpha = 0.025,
    power = 1 - 0.1,
    enroll_rate = enroll_rate,
    fail_rate = fail_rate,
    study_duration = 36,
    ratio = 1
  ) |>
    summary() |>
    as_gt()
gt_to_latex(output)
```

```
\begin{table}[t]
\caption*{
{\fontsize{20}{25}\selectfont  Fixed Design under Fleming-Harrington Method\textsuperscript{\textit{1}}\fontsize{12}{15}\selectfont }
} 
\fontsize{12.0pt}{14.0pt}\selectfont
\begin{tabular*}{\linewidth}{@{\extracolsep{\fill}}lrrrrrrr}
\toprule
Design & N & Events & Time & AHR & Bound & alpha & Power \\ 
\midrule\addlinespace[2.5pt]
Fleming-Harrington FH(0, 0) (logrank) & 458.3509 & 321.3931 & 36 & 0.6969049 & 1.959964 & 0.025 & 0.9 \\ 
\bottomrule
\end{tabular*}
\begin{minipage}{\linewidth}
\vspace{.05em}
\textsuperscript{\textit{1}}Power for Fleming-Harrington test  FH(0, 0) (logrank) using method of Yung and Liu.\\
\end{minipage}
\end{table}
```

## `gs_design_ahr()` summary `as_gt()`

```r
output <- gs_design_ahr() |>
    summary() |>
    as_gt()
gt_to_latex(output)
```

```
\begin{table}[t]
\caption*{
{\fontsize{20}{25}\selectfont  Bound summary for AHR design\fontsize{12}{15}\selectfont } \\ 
{\fontsize{14}{17}\selectfont  AHR approximations of \textasciitilde{}HR at bound\fontsize{12}{15}\selectfont }
} 
\fontsize{12.0pt}{14.0pt}\selectfont
\begin{tabular*}{\linewidth}{@{\extracolsep{\fill}}crrrrr}
\toprule
 &  &  &  & \multicolumn{2}{c}{{Cumulative boundary crossing probability}} \\ 
\cmidrule(lr){5-6}
Bound & Z & Nominal p\textsuperscript{\textit{1}} & \textasciitilde{}HR at bound\textsuperscript{\textit{2}} & Alternate hypothesis & Null hypothesis \\ 
\midrule\addlinespace[2.5pt]
\multicolumn{6}{l}{Analysis: 1 Time: 36 N: 476 Events: 291.9 AHR: 0.68 Information fraction: 1} \\[2.5pt] 
\midrule\addlinespace[2.5pt]
Efficacy & 1.96 & 0.025 & 0.795 & 0.9 & 0.025 \\ 
\bottomrule
\end{tabular*}
\begin{minipage}{\linewidth}
\vspace{.05em}
\textsuperscript{\textit{1}}One-sided p-value for experimental vs control treatment. Value < 0.5 favors experimental, > 0.5 favors control.\\
\textsuperscript{\textit{2}}Approximate hazard ratio to cross bound.\\
\end{minipage}
\end{table}
```

## `gs_power_ahr()` summary `as_gt()`

```r
output <- gs_power_ahr(lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.1)) |>
    summary() |>
    as_gt()
gt_to_latex(output)
```

```
\begin{table}[t]
\caption*{
{\fontsize{20}{25}\selectfont  Bound summary for AHR design\fontsize{12}{15}\selectfont } \\ 
{\fontsize{14}{17}\selectfont  AHR approximations of \textasciitilde{}HR at bound\fontsize{12}{15}\selectfont }
} 
\fontsize{12.0pt}{14.0pt}\selectfont
\begin{tabular*}{\linewidth}{@{\extracolsep{\fill}}crrrrr}
\toprule
 &  &  &  & \multicolumn{2}{c}{{Cumulative boundary crossing probability}} \\ 
\cmidrule(lr){5-6}
Bound & Z & Nominal p\textsuperscript{\textit{1}} & \textasciitilde{}HR at bound\textsuperscript{\textit{2}} & Alternate hypothesis & Null hypothesis \\ 
\midrule\addlinespace[2.5pt]
\multicolumn{6}{l}{Analysis: 1 Time: 14.9 N: 108 Events: 30 AHR: 0.79 Information fraction: 0.6} \\[2.5pt] 
\midrule\addlinespace[2.5pt]
Efficacy & 2.67 & 0.0038 & 0.3774 & 0.0231 & 0.0038 \\ 
Futility & -1.17 & 0.8792 & 1.5336 & 0.0349 & 0.1208 \\ 
\midrule\addlinespace[2.5pt]
\multicolumn{6}{l}{Analysis: 2 Time: 19.2 N: 108 Events: 40 AHR: 0.74 Information fraction: 0.8} \\[2.5pt] 
\midrule\addlinespace[2.5pt]
Efficacy & 2.29 & 0.0110 & 0.4849 & 0.0897 & 0.0122 \\ 
Futility & -0.66 & 0.7462 & 1.2331 & 0.0668 & 0.2655 \\ 
\midrule\addlinespace[2.5pt]
\multicolumn{6}{l}{Analysis: 3 Time: 24.5 N: 108 Events: 50 AHR: 0.71 Information fraction: 1} \\[2.5pt] 
\midrule\addlinespace[2.5pt]
Efficacy & 2.03 & 0.0211 & 0.5631 & 0.2070 & 0.0250 \\ 
Futility & -0.23 & 0.5897 & 1.0662 & 0.1008 & 0.4303 \\ 
\bottomrule
\end{tabular*}
\begin{minipage}{\linewidth}
\vspace{.05em}
\textsuperscript{\textit{1}}One-sided p-value for experimental vs control treatment. Value < 0.5 favors experimental, > 0.5 favors control.\\
\textsuperscript{\textit{2}}Approximate hazard ratio to cross bound.\\
\end{minipage}
\end{table}
```

## `gs_design_wlr()` summary `as_gt()`

```r
output <- gs_design_wlr() |>
    summary() |>
    as_gt()
gt_to_latex(output)
```

```
\begin{table}[t]
\caption*{
{\fontsize{20}{25}\selectfont  Bound summary for WLR design\fontsize{12}{15}\selectfont } \\ 
{\fontsize{14}{17}\selectfont  WLR approximation of \textasciitilde{}wHR at bound\fontsize{12}{15}\selectfont }
} 
\fontsize{12.0pt}{14.0pt}\selectfont
\begin{tabular*}{\linewidth}{@{\extracolsep{\fill}}crrrrr}
\toprule
 &  &  &  & \multicolumn{2}{c}{{Cumulative boundary crossing probability}} \\ 
\cmidrule(lr){5-6}
Bound & Z & Nominal p\textsuperscript{\textit{1}} & \textasciitilde{}wHR at bound\textsuperscript{\textit{2}} & Alternate hypothesis & Null hypothesis \\ 
\midrule\addlinespace[2.5pt]
\multicolumn{6}{l}{Analysis: 1 Time: 36 N: 471.1 Events: 289 AHR: 0.68 Information fraction: 1\textsuperscript{\textit{3}}} \\[2.5pt] 
\midrule\addlinespace[2.5pt]
Efficacy & 1.96 & 0.025 & 0.7941 & 0.9 & 0.025 \\ 
\bottomrule
\end{tabular*}
\begin{minipage}{\linewidth}
\vspace{.05em}
\textsuperscript{\textit{1}}One-sided p-value for experimental vs control treatment. Value < 0.5 favors experimental, > 0.5 favors control.\\
\textsuperscript{\textit{2}}Approximate hazard ratio to cross bound.\\
\textsuperscript{\textit{3}}wAHR is the weighted AHR.\\
\end{minipage}
\end{table}
```

## `gs_power_wlr()` summary `as_gt()`

```r
output <- gs_power_wlr(lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.1)) |>
    summary() |>
    as_gt(
      footnote = list(
        content = c(
          "approximate weighted hazard ratio to cross bound.",
          "wAHR is the weighted AHR.",
          "the crossing probability.",
          "this table is generated by gs_power_wlr."
        ),
        location = c("~wHR at bound", NA, NA, NA),
        attr = c("colname", "analysis", "spanner", "title")
      )
    )
gt_to_latex(output)
```

```
\begin{table}[t]
\caption*{
{\fontsize{20}{25}\selectfont  Bound summary for WLR design\textsuperscript{\textit{1}}\fontsize{12}{15}\selectfont } \\ 
{\fontsize{14}{17}\selectfont  WLR approximation of \textasciitilde{}wHR at bound\fontsize{12}{15}\selectfont }
} 
\fontsize{12.0pt}{14.0pt}\selectfont
\begin{tabular*}{\linewidth}{@{\extracolsep{\fill}}crrrrr}
\toprule
 &  &  &  & \multicolumn{2}{c}{{Cumulative boundary crossing probability}}\textsuperscript{\textit{2}} \\ 
\cmidrule(lr){5-6}
Bound & Z & Nominal p & \textasciitilde{}wHR at bound\textsuperscript{\textit{3}} & Alternate hypothesis & Null hypothesis \\ 
\midrule\addlinespace[2.5pt]
\multicolumn{6}{l}{Analysis: 1 Time: 14.9 N: 108 Events: 30 AHR: 0.79 Information fraction: 0.6\textsuperscript{\textit{4}}} \\[2.5pt] 
\midrule\addlinespace[2.5pt]
Efficacy & 2.68 & 0.0037 & 0.3765 & 0.0217 & 0.0037 \\ 
Futility & -1.17 & 0.8798 & 1.5353 & 0.0341 & 0.1202 \\ 
\midrule\addlinespace[2.5pt]
\multicolumn{6}{l}{Analysis: 2 Time: 19.2 N: 108 Events: 40 AHR: 0.75 Information fraction: 0.8\textsuperscript{\textit{4}}} \\[2.5pt] 
\midrule\addlinespace[2.5pt]
Efficacy & 2.29 & 0.0110 & 0.4846 & 0.0886 & 0.0121 \\ 
Futility & -0.66 & 0.7452 & 1.2319 & 0.0664 & 0.2664 \\ 
\midrule\addlinespace[2.5pt]
\multicolumn{6}{l}{Analysis: 3 Time: 24.5 N: 108 Events: 50 AHR: 0.71 Information fraction: 1\textsuperscript{\textit{4}}} \\[2.5pt] 
\midrule\addlinespace[2.5pt]
Efficacy & 2.03 & 0.0212 & 0.5631 & 0.2071 & 0.0250 \\ 
Futility & -0.22 & 0.5881 & 1.0650 & 0.1002 & 0.4319 \\ 
\bottomrule
\end{tabular*}
\begin{minipage}{\linewidth}
\vspace{.05em}
\textsuperscript{\textit{1}}this table is generated by gs\_power\_wlr.\\
\textsuperscript{\textit{2}}the crossing probability.\\
\textsuperscript{\textit{3}}approximate weighted hazard ratio to cross bound.\\
\textsuperscript{\textit{4}}wAHR is the weighted AHR.\\
\end{minipage}
\end{table}
```

## `gs_power_combo()` summary `as_gt()`

```r
with_seed <- function(seed, code) {
    code <- substitute(code)
    original_seed <- .Random.seed
    on.exit(.Random.seed <<- original_seed)
    set.seed(seed)
    eval.parent(code)
  }

  # See <https://github.com/Merck/gsDesign2/issues/340>
  output <- with_seed(
    42,
    {
      gs_power_combo() |>
        summary() |>
        as_gt()
    }
  )
gt_to_latex(output)
```

```
\begin{table}[t]
\caption*{
{\fontsize{20}{25}\selectfont  Bound summary for MaxCombo design\fontsize{12}{15}\selectfont } \\ 
{\fontsize{14}{17}\selectfont  MaxCombo approximation\fontsize{12}{15}\selectfont }
} 
\fontsize{12.0pt}{14.0pt}\selectfont
\begin{tabular*}{\linewidth}{@{\extracolsep{\fill}}crrrr}
\toprule
 &  &  & \multicolumn{2}{c}{{Cumulative boundary crossing probability}} \\ 
\cmidrule(lr){4-5}
Bound & Z & Nominal p\textsuperscript{\textit{1}} & Alternate hypothesis & Null hypothesis \\ 
\midrule\addlinespace[2.5pt]
\multicolumn{5}{l}{Analysis: 1 Time: 12 N: 500 Events: 107.4 AHR: 0.84 Event fraction: 0.32\textsuperscript{\textit{2}}} \\[2.5pt] 
\midrule\addlinespace[2.5pt]
Efficacy & 3 & 0.0013 & 0.0175 & 0.0013 \\ 
Futility & -1 & 0.8413 & 0.0293 & 0.0000 \\ 
\midrule\addlinespace[2.5pt]
\multicolumn{5}{l}{Analysis: 2 Time: 24 N: 500 Events: 246.3 AHR: 0.72 Event fraction: 0.74\textsuperscript{\textit{2}}} \\[2.5pt] 
\midrule\addlinespace[2.5pt]
Efficacy & 2 & 0.0228 & 0.7261 & 0.0233 \\ 
Futility & 0 & 0.5000 & 0.0314 & 0.0000 \\ 
\midrule\addlinespace[2.5pt]
\multicolumn{5}{l}{Analysis: 3 Time: 36 N: 500 Events: 331.3 AHR: 0.68 Event fraction: 1\textsuperscript{\textit{2}}} \\[2.5pt] 
\midrule\addlinespace[2.5pt]
Efficacy & 1 & 0.1587 & 0.9674 & 0.1956 \\ 
Futility & 1 & 0.1587 & 0.0326 & 0.0000 \\ 
\bottomrule
\end{tabular*}
\begin{minipage}{\linewidth}
\vspace{.05em}
\textsuperscript{\textit{1}}One-sided p-value for experimental vs control treatment. Value < 0.5 favors experimental, > 0.5 favors control.\\
\textsuperscript{\textit{2}}EF is event fraction. AHR is under regular weighted log rank test.\\
\end{minipage}
\end{table}
```

## `gs_design_rd()` summary `as_gt()`

```r
output <- gs_design_rd() |>
    summary() |>
    as_gt()
gt_to_latex(output)
```

```
\begin{table}[t]
\caption*{
{\fontsize{20}{25}\selectfont  Bound summary of Binary Endpoint\fontsize{12}{15}\selectfont } \\ 
{\fontsize{14}{17}\selectfont  measured by risk difference\fontsize{12}{15}\selectfont }
} 
\fontsize{12.0pt}{14.0pt}\selectfont
\begin{tabular*}{\linewidth}{@{\extracolsep{\fill}}crrrrr}
\toprule
 &  &  &  & \multicolumn{2}{c}{{Cumulative boundary crossing probability}} \\ 
\cmidrule(lr){5-6}
Bound & Z & Nominal p\textsuperscript{\textit{1}} & \textasciitilde{}Risk difference at bound & Alternate hypothesis & Null hypothesis \\ 
\midrule\addlinespace[2.5pt]
\multicolumn{6}{l}{Analysis: 1 N: 2423.1 Risk difference: 0.05 Information fraction: 1} \\[2.5pt] 
\midrule\addlinespace[2.5pt]
Efficacy & 1.96 & 0.025 & 0.0302 & 0.9 & 0.025 \\ 
\bottomrule
\end{tabular*}
\begin{minipage}{\linewidth}
\vspace{.05em}
\textsuperscript{\textit{1}}One-sided p-value for experimental vs control treatment. Value < 0.5 favors experimental, > 0.5 favors control.\\
\end{minipage}
\end{table}
```

## `gs_power_rd()` summary `as_gt()`

```r
output <- gs_power_rd() |>
    summary() |>
    as_gt()
gt_to_latex(output)
```

```
\begin{table}[t]
\caption*{
{\fontsize{20}{25}\selectfont  Bound summary of Binary Endpoint\fontsize{12}{15}\selectfont } \\ 
{\fontsize{14}{17}\selectfont  measured by risk difference\fontsize{12}{15}\selectfont }
} 
\fontsize{12.0pt}{14.0pt}\selectfont
\begin{tabular*}{\linewidth}{@{\extracolsep{\fill}}crrrrr}
\toprule
 &  &  &  & \multicolumn{2}{c}{{Cumulative boundary crossing probability}} \\ 
\cmidrule(lr){5-6}
Bound & Z & Nominal p\textsuperscript{\textit{1}} & \textasciitilde{}Risk difference at bound & Alternate hypothesis & Null hypothesis \\ 
\midrule\addlinespace[2.5pt]
\multicolumn{6}{l}{Analysis: 1 N: 40 Risk difference: 0.05 Information fraction: 0.67} \\[2.5pt] 
\midrule\addlinespace[2.5pt]
Efficacy & 3.71 & 0.0001 & 0.4448 & 0.0005 & 0.0001 \\ 
Futility & -1.28 & 0.9000 & -0.1537 & 0.0444 & 0.1000 \\ 
\midrule\addlinespace[2.5pt]
\multicolumn{6}{l}{Analysis: 2 N: 50 Risk difference: 0.05 Information fraction: 0.83} \\[2.5pt] 
\midrule\addlinespace[2.5pt]
Efficacy & 2.51 & 0.0060 & 0.2693 & 0.0204 & 0.0060 \\ 
\midrule\addlinespace[2.5pt]
\multicolumn{6}{l}{Analysis: 3 N: 60 Risk difference: 0.05 Information fraction: 1} \\[2.5pt] 
\midrule\addlinespace[2.5pt]
Efficacy & 1.99 & 0.0231 & 0.1951 & 0.0705 & \textsuperscript{\textit{2}}0.0238 \\ 
\bottomrule
\end{tabular*}
\begin{minipage}{\linewidth}
\vspace{.05em}
\textsuperscript{\textit{1}}One-sided p-value for experimental vs control treatment. Value < 0.5 favors experimental, > 0.5 favors control.\\
\textsuperscript{\textit{2}}Cumulative alpha for final analysis (0.0238) is less than the full alpha (0.025) when the futility bound is non-binding. The smaller value subtracts the probability of crossing a futility bound before crossing an efficacy bound at a later analysis (0.025 - 0.0012 = 0.0238) under the null hypothesis.\\
\end{minipage}
\end{table}
```

## `gs_power_wlr()` summary `as_gt()` with custom title and subtitle

```r
output <- gs_power_wlr(lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.1)) |>
    summary() |>
    as_gt(title = "Bound Summary", subtitle = "from gs_power_wlr")
gt_to_latex(output)
```

```
\begin{table}[t]
\caption*{
{\fontsize{20}{25}\selectfont  Bound Summary\fontsize{12}{15}\selectfont } \\ 
{\fontsize{14}{17}\selectfont  from gs\_power\_wlr\fontsize{12}{15}\selectfont }
} 
\fontsize{12.0pt}{14.0pt}\selectfont
\begin{tabular*}{\linewidth}{@{\extracolsep{\fill}}crrrrr}
\toprule
 &  &  &  & \multicolumn{2}{c}{{Cumulative boundary crossing probability}} \\ 
\cmidrule(lr){5-6}
Bound & Z & Nominal p\textsuperscript{\textit{1}} & \textasciitilde{}wHR at bound\textsuperscript{\textit{2}} & Alternate hypothesis & Null hypothesis \\ 
\midrule\addlinespace[2.5pt]
\multicolumn{6}{l}{Analysis: 1 Time: 14.9 N: 108 Events: 30 AHR: 0.79 Information fraction: 0.6\textsuperscript{\textit{3}}} \\[2.5pt] 
\midrule\addlinespace[2.5pt]
Efficacy & 2.68 & 0.0037 & 0.3765 & 0.0217 & 0.0037 \\ 
Futility & -1.17 & 0.8798 & 1.5353 & 0.0341 & 0.1202 \\ 
\midrule\addlinespace[2.5pt]
\multicolumn{6}{l}{Analysis: 2 Time: 19.2 N: 108 Events: 40 AHR: 0.75 Information fraction: 0.8\textsuperscript{\textit{3}}} \\[2.5pt] 
\midrule\addlinespace[2.5pt]
Efficacy & 2.29 & 0.0110 & 0.4846 & 0.0886 & 0.0121 \\ 
Futility & -0.66 & 0.7452 & 1.2319 & 0.0664 & 0.2664 \\ 
\midrule\addlinespace[2.5pt]
\multicolumn{6}{l}{Analysis: 3 Time: 24.5 N: 108 Events: 50 AHR: 0.71 Information fraction: 1\textsuperscript{\textit{3}}} \\[2.5pt] 
\midrule\addlinespace[2.5pt]
Efficacy & 2.03 & 0.0212 & 0.5631 & 0.2071 & 0.0250 \\ 
Futility & -0.22 & 0.5881 & 1.0650 & 0.1002 & 0.4319 \\ 
\bottomrule
\end{tabular*}
\begin{minipage}{\linewidth}
\vspace{.05em}
\textsuperscript{\textit{1}}One-sided p-value for experimental vs control treatment. Value < 0.5 favors experimental, > 0.5 favors control.\\
\textsuperscript{\textit{2}}Approximate hazard ratio to cross bound.\\
\textsuperscript{\textit{3}}wAHR is the weighted AHR.\\
\end{minipage}
\end{table}
```

## `gs_power_wlr()` summary `as_gt()` with colname_spanner and colname_spannersub

```r
output <- gs_power_wlr(lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.1)) |>
    summary() |>
    as_gt(
      colname_spanner = "Cumulative probability to cross boundaries",
      colname_spannersub = c("under H1", "under H0")
    )
gt_to_latex(output)
```

```
\begin{table}[t]
\caption*{
{\fontsize{20}{25}\selectfont  Bound summary for WLR design\fontsize{12}{15}\selectfont } \\ 
{\fontsize{14}{17}\selectfont  WLR approximation of \textasciitilde{}wHR at bound\fontsize{12}{15}\selectfont }
} 
\fontsize{12.0pt}{14.0pt}\selectfont
\begin{tabular*}{\linewidth}{@{\extracolsep{\fill}}crrrrr}
\toprule
 &  &  &  & \multicolumn{2}{c}{{Cumulative probability to cross boundaries}} \\ 
\cmidrule(lr){5-6}
Bound & Z & Nominal p\textsuperscript{\textit{1}} & \textasciitilde{}wHR at bound\textsuperscript{\textit{2}} & under H1 & under H0 \\ 
\midrule\addlinespace[2.5pt]
\multicolumn{6}{l}{Analysis: 1 Time: 14.9 N: 108 Events: 30 AHR: 0.79 Information fraction: 0.6\textsuperscript{\textit{3}}} \\[2.5pt] 
\midrule\addlinespace[2.5pt]
Efficacy & 2.68 & 0.0037 & 0.3765 & 0.0217 & 0.0037 \\ 
Futility & -1.17 & 0.8798 & 1.5353 & 0.0341 & 0.1202 \\ 
\midrule\addlinespace[2.5pt]
\multicolumn{6}{l}{Analysis: 2 Time: 19.2 N: 108 Events: 40 AHR: 0.75 Information fraction: 0.8\textsuperscript{\textit{3}}} \\[2.5pt] 
\midrule\addlinespace[2.5pt]
Efficacy & 2.29 & 0.0110 & 0.4846 & 0.0886 & 0.0121 \\ 
Futility & -0.66 & 0.7452 & 1.2319 & 0.0664 & 0.2664 \\ 
\midrule\addlinespace[2.5pt]
\multicolumn{6}{l}{Analysis: 3 Time: 24.5 N: 108 Events: 50 AHR: 0.71 Information fraction: 1\textsuperscript{\textit{3}}} \\[2.5pt] 
\midrule\addlinespace[2.5pt]
Efficacy & 2.03 & 0.0212 & 0.5631 & 0.2071 & 0.0250 \\ 
Futility & -0.22 & 0.5881 & 1.0650 & 0.1002 & 0.4319 \\ 
\bottomrule
\end{tabular*}
\begin{minipage}{\linewidth}
\vspace{.05em}
\textsuperscript{\textit{1}}One-sided p-value for experimental vs control treatment. Value < 0.5 favors experimental, > 0.5 favors control.\\
\textsuperscript{\textit{2}}Approximate hazard ratio to cross bound.\\
\textsuperscript{\textit{3}}wAHR is the weighted AHR.\\
\end{minipage}
\end{table}
```

## `gs_power_wlr()` summary `as_gt()` with custom footnotes

```r
output <- gs_power_wlr(lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.1)) |>
    summary() |>
    as_gt(
      footnote = list(
        content = c(
          "approximate weighted hazard ratio to cross bound.",
          "wAHR is the weighted AHR.",
          "the crossing probability.",
          "this table is generated by gs_power_wlr."
        ),
        location = c("~wHR at bound", NA, NA, NA),
        attr = c("colname", "analysis", "spanner", "title")
      )
    )
gt_to_latex(output)
```

```
\begin{table}[t]
\caption*{
{\fontsize{20}{25}\selectfont  Bound summary for WLR design\textsuperscript{\textit{1}}\fontsize{12}{15}\selectfont } \\ 
{\fontsize{14}{17}\selectfont  WLR approximation of \textasciitilde{}wHR at bound\fontsize{12}{15}\selectfont }
} 
\fontsize{12.0pt}{14.0pt}\selectfont
\begin{tabular*}{\linewidth}{@{\extracolsep{\fill}}crrrrr}
\toprule
 &  &  &  & \multicolumn{2}{c}{{Cumulative boundary crossing probability}}\textsuperscript{\textit{2}} \\ 
\cmidrule(lr){5-6}
Bound & Z & Nominal p & \textasciitilde{}wHR at bound\textsuperscript{\textit{3}} & Alternate hypothesis & Null hypothesis \\ 
\midrule\addlinespace[2.5pt]
\multicolumn{6}{l}{Analysis: 1 Time: 14.9 N: 108 Events: 30 AHR: 0.79 Information fraction: 0.6\textsuperscript{\textit{4}}} \\[2.5pt] 
\midrule\addlinespace[2.5pt]
Efficacy & 2.68 & 0.0037 & 0.3765 & 0.0217 & 0.0037 \\ 
Futility & -1.17 & 0.8798 & 1.5353 & 0.0341 & 0.1202 \\ 
\midrule\addlinespace[2.5pt]
\multicolumn{6}{l}{Analysis: 2 Time: 19.2 N: 108 Events: 40 AHR: 0.75 Information fraction: 0.8\textsuperscript{\textit{4}}} \\[2.5pt] 
\midrule\addlinespace[2.5pt]
Efficacy & 2.29 & 0.0110 & 0.4846 & 0.0886 & 0.0121 \\ 
Futility & -0.66 & 0.7452 & 1.2319 & 0.0664 & 0.2664 \\ 
\midrule\addlinespace[2.5pt]
\multicolumn{6}{l}{Analysis: 3 Time: 24.5 N: 108 Events: 50 AHR: 0.71 Information fraction: 1\textsuperscript{\textit{4}}} \\[2.5pt] 
\midrule\addlinespace[2.5pt]
Efficacy & 2.03 & 0.0212 & 0.5631 & 0.2071 & 0.0250 \\ 
Futility & -0.22 & 0.5881 & 1.0650 & 0.1002 & 0.4319 \\ 
\bottomrule
\end{tabular*}
\begin{minipage}{\linewidth}
\vspace{.05em}
\textsuperscript{\textit{1}}this table is generated by gs\_power\_wlr.\\
\textsuperscript{\textit{2}}the crossing probability.\\
\textsuperscript{\textit{3}}approximate weighted hazard ratio to cross bound.\\
\textsuperscript{\textit{4}}wAHR is the weighted AHR.\\
\end{minipage}
\end{table}
```

## `gs_power_wlr()` summary `as_gt()` with display_bound

```r
output <- gs_power_wlr(lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.1)) |>
    summary() |>
    as_gt(display_bound = "Efficacy")
gt_to_latex(output)
```

```
\begin{table}[t]
\caption*{
{\fontsize{20}{25}\selectfont  Bound summary for WLR design\fontsize{12}{15}\selectfont } \\ 
{\fontsize{14}{17}\selectfont  WLR approximation of \textasciitilde{}wHR at bound\fontsize{12}{15}\selectfont }
} 
\fontsize{12.0pt}{14.0pt}\selectfont
\begin{tabular*}{\linewidth}{@{\extracolsep{\fill}}crrrrr}
\toprule
 &  &  &  & \multicolumn{2}{c}{{Cumulative boundary crossing probability}} \\ 
\cmidrule(lr){5-6}
Bound & Z & Nominal p\textsuperscript{\textit{1}} & \textasciitilde{}wHR at bound\textsuperscript{\textit{2}} & Alternate hypothesis & Null hypothesis \\ 
\midrule\addlinespace[2.5pt]
\multicolumn{6}{l}{Analysis: 1 Time: 14.9 N: 108 Events: 30 AHR: 0.79 Information fraction: 0.6\textsuperscript{\textit{3}}} \\[2.5pt] 
\midrule\addlinespace[2.5pt]
Efficacy & 2.68 & 0.0037 & 0.3765 & 0.0217 & 0.0037 \\ 
\midrule\addlinespace[2.5pt]
\multicolumn{6}{l}{Analysis: 2 Time: 19.2 N: 108 Events: 40 AHR: 0.75 Information fraction: 0.8\textsuperscript{\textit{3}}} \\[2.5pt] 
\midrule\addlinespace[2.5pt]
Efficacy & 2.29 & 0.0110 & 0.4846 & 0.0886 & 0.0121 \\ 
\midrule\addlinespace[2.5pt]
\multicolumn{6}{l}{Analysis: 3 Time: 24.5 N: 108 Events: 50 AHR: 0.71 Information fraction: 1\textsuperscript{\textit{3}}} \\[2.5pt] 
\midrule\addlinespace[2.5pt]
Efficacy & 2.03 & 0.0212 & 0.5631 & 0.2071 & 0.0250 \\ 
\bottomrule
\end{tabular*}
\begin{minipage}{\linewidth}
\vspace{.05em}
\textsuperscript{\textit{1}}One-sided p-value for experimental vs control treatment. Value < 0.5 favors experimental, > 0.5 favors control.\\
\textsuperscript{\textit{2}}Approximate hazard ratio to cross bound.\\
\textsuperscript{\textit{3}}wAHR is the weighted AHR.\\
\end{minipage}
\end{table}
```

## `gs_power_wlr()` summary `as_gt()` with display_columns

```r
output <- gs_power_wlr(lpar = list(sf = gsDesign::sfLDOF, total_spend = 0.1)) |>
    summary() |>
    as_gt(display_columns = c("Analysis", "Bound", "Nominal p", "Z", "Probability"))
gt_to_latex(output)
```

```
\begin{table}[t]
\caption*{
{\fontsize{20}{25}\selectfont  Bound summary for WLR design\fontsize{12}{15}\selectfont } \\ 
{\fontsize{14}{17}\selectfont  WLR approximation of \textasciitilde{}wHR at bound\fontsize{12}{15}\selectfont }
} 
\fontsize{12.0pt}{14.0pt}\selectfont
\begin{tabular*}{\linewidth}{@{\extracolsep{\fill}}crrrr}
\toprule
 &  &  & \multicolumn{2}{c}{{Cumulative boundary crossing probability}} \\ 
\cmidrule(lr){4-5}
Bound & Nominal p\textsuperscript{\textit{1}} & Z & Alternate hypothesis & Null hypothesis \\ 
\midrule\addlinespace[2.5pt]
\multicolumn{5}{l}{Analysis: 1 Time: 14.9 N: 108 Events: 30 AHR: 0.79 Information fraction: 0.6\textsuperscript{\textit{2}}} \\[2.5pt] 
\midrule\addlinespace[2.5pt]
Efficacy & 0.0037 & 2.68 & 0.0217 & 0.0037 \\ 
Futility & 0.8798 & -1.17 & 0.0341 & 0.1202 \\ 
\midrule\addlinespace[2.5pt]
\multicolumn{5}{l}{Analysis: 2 Time: 19.2 N: 108 Events: 40 AHR: 0.75 Information fraction: 0.8\textsuperscript{\textit{2}}} \\[2.5pt] 
\midrule\addlinespace[2.5pt]
Efficacy & 0.0110 & 2.29 & 0.0886 & 0.0121 \\ 
Futility & 0.7452 & -0.66 & 0.0664 & 0.2664 \\ 
\midrule\addlinespace[2.5pt]
\multicolumn{5}{l}{Analysis: 3 Time: 24.5 N: 108 Events: 50 AHR: 0.71 Information fraction: 1\textsuperscript{\textit{2}}} \\[2.5pt] 
\midrule\addlinespace[2.5pt]
Efficacy & 0.0212 & 2.03 & 0.2071 & 0.0250 \\ 
Futility & 0.5881 & -0.22 & 0.1002 & 0.4319 \\ 
\bottomrule
\end{tabular*}
\begin{minipage}{\linewidth}
\vspace{.05em}
\textsuperscript{\textit{1}}One-sided p-value for experimental vs control treatment. Value < 0.5 favors experimental, > 0.5 favors control.\\
\textsuperscript{\textit{2}}wAHR is the weighted AHR.\\
\end{minipage}
\end{table}
```

