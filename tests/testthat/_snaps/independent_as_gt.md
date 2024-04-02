# Snapshot test for fixed_design summary as_gt

    \setlength{\LTpost}{0mm}
    \begin{longtable}{lrrrrrr}
    \caption*{
    {\large Fixed Design under AHR Method\textsuperscript{\textit{1}}}
    } \\ 
    \toprule
    Design & N & Events & Time & Bound & alpha & Power \\ 
    \midrule\addlinespace[2.5pt]
    Average hazard ratio & 463.078 & 324.7077 & 36 & 1.959964 & 0.025 & 0.9 \\ 
    \bottomrule
    \end{longtable}
    \begin{minipage}{\linewidth}
    \textsuperscript{\textit{1}}Power computed with average hazard ratio method.\\
    \end{minipage}

# Snapshot test for fixed_design summary as_gt with custom title and footnote

    \setlength{\LTpost}{0mm}
    \begin{longtable}{lrrrrrr}
    \caption*{
    {\large Custom Title\textsuperscript{\textit{1}}}
    } \\ 
    \toprule
    Design & N & Events & Time & Bound & alpha & Power \\ 
    \midrule\addlinespace[2.5pt]
    Average hazard ratio & 463.078 & 324.7077 & 36 & 1.959964 & 0.025 & 0.9 \\ 
    \bottomrule
    \end{longtable}
    \begin{minipage}{\linewidth}
    \textsuperscript{\textit{1}}Custom footnote.\\
    \end{minipage}

# Snapshot test for fixed_design_fh summary as_gt

    \setlength{\LTpost}{0mm}
    \begin{longtable}{lrrrrrr}
    \caption*{
    {\large Fixed Design under Fleming-Harrington Method\textsuperscript{\textit{1}}}
    } \\ 
    \toprule
    Design & N & Events & Time & Bound & alpha & Power \\ 
    \midrule\addlinespace[2.5pt]
    Fleming-Harrington FH(0, 0) (logrank) & 458.3509 & 321.3931 & 36 & 1.959964 & 0.025 & 0.9 \\ 
    \bottomrule
    \end{longtable}
    \begin{minipage}{\linewidth}
    \textsuperscript{\textit{1}}Power for Fleming-Harrington test  FH(0, 0) (logrank) using method of Yung and Liu.\\
    \end{minipage}

# Snapshot test for gs_design_ahr summary as_gt

    \setlength{\LTpost}{0mm}
    \begin{longtable}{lrrrrr}
    \caption*{
    {\large Bound summary for AHR design} \\ 
    {\small AHR approximations of \textasciitilde{}HR at bound}
    } \\ 
    \toprule
     &  &  &  & \multicolumn{2}{c}{Cumulative boundary crossing probability} \\ 
    \cmidrule(lr){5-6}
    Bound & Z & Nominal p\textsuperscript{\textit{1}} & \textasciitilde{}HR at bound\textsuperscript{\textit{2}} & Alternate hypothesis & Null hypothesis \\ 
    \midrule\addlinespace[2.5pt]
    \multicolumn{6}{l}{Analysis: 1 Time: 36 N: 476 Event: 291.9 AHR: 0.68 Information fraction: 1} \\ 
    \midrule\addlinespace[2.5pt]
    Efficacy & 1.96 & 0.025 & 0.795 & 0.9 & 0.025 \\ 
    \bottomrule
    \end{longtable}
    \begin{minipage}{\linewidth}
    \textsuperscript{\textit{1}}One-sided p-value for experimental vs control treatment.
              Value < 0.5 favors experimental, > 0.5 favors control.\\
    \textsuperscript{\textit{2}}Approximate hazard ratio to cross bound.\\
    \end{minipage}

# Snapshot test for gs_power_ahr summary as_gt

    \setlength{\LTpost}{0mm}
    \begin{longtable}{lrrrrr}
    \caption*{
    {\large Bound summary for AHR design} \\ 
    {\small AHR approximations of \textasciitilde{}HR at bound}
    } \\ 
    \toprule
     &  &  &  & \multicolumn{2}{c}{Cumulative boundary crossing probability} \\ 
    \cmidrule(lr){5-6}
    Bound & Z & Nominal p\textsuperscript{\textit{1}} & \textasciitilde{}HR at bound\textsuperscript{\textit{2}} & Alternate hypothesis & Null hypothesis \\ 
    \midrule\addlinespace[2.5pt]
    \multicolumn{6}{l}{Analysis: 1 Time: 14.9 N: 108 Event: 30 AHR: 0.79 Information fraction: 0.6} \\ 
    \midrule\addlinespace[2.5pt]
    Futility & -1.28 & 0.9000 & 1.6031 & 0.0273 & 0.1000 \\ 
    Efficacy & 2.67 & 0.0038 & 0.3743 & 0.0231 & 0.0038 \\ 
    \midrule\addlinespace[2.5pt]
    \multicolumn{6}{l}{Analysis: 2 Time: 19.2 N: 108 Event: 40 AHR: 0.74 Information fraction: 0.8} \\ 
    \midrule\addlinespace[2.5pt]
    Efficacy & 2.29 & 0.0110 & 0.4812 & 0.0897 & 0.0122 \\ 
    \midrule\addlinespace[2.5pt]
    \multicolumn{6}{l}{Analysis: 3 Time: 24.5 N: 108 Event: 50 AHR: 0.71 Information fraction: 1} \\ 
    \midrule\addlinespace[2.5pt]
    Efficacy & 2.03 & 0.0211 & 0.5595 & 0.2070 & 0.0250 \\ 
    \bottomrule
    \end{longtable}
    \begin{minipage}{\linewidth}
    \textsuperscript{\textit{1}}One-sided p-value for experimental vs control treatment.
              Value < 0.5 favors experimental, > 0.5 favors control.\\
    \textsuperscript{\textit{2}}Approximate hazard ratio to cross bound.\\
    \end{minipage}

# Snapshot test for gs_design_wlr summary as_gt

    \setlength{\LTpost}{0mm}
    \begin{longtable}{lrrrrr}
    \caption*{
    {\large Bound summary for WLR design} \\ 
    {\small WLR approximation of \textasciitilde{}wHR at bound}
    } \\ 
    \toprule
     &  &  &  & \multicolumn{2}{c}{Cumulative boundary crossing probability} \\ 
    \cmidrule(lr){5-6}
    Bound & Z & Nominal p\textsuperscript{\textit{1}} & \textasciitilde{}wHR at bound\textsuperscript{\textit{2}} & Alternate hypothesis & Null hypothesis \\ 
    \midrule\addlinespace[2.5pt]
    \multicolumn{6}{l}{Analysis: 1 Time: 36 N: 471.1 Event: 289 AHR: 0.68 Information fraction: 1\textsuperscript{\textit{3}}} \\ 
    \midrule\addlinespace[2.5pt]
    Efficacy & 1.96 & 0.025 & 0.7940584 & 0.9 & 0.025 \\ 
    \bottomrule
    \end{longtable}
    \begin{minipage}{\linewidth}
    \textsuperscript{\textit{1}}One-sided p-value for experimental vs control treatment.
              Value < 0.5 favors experimental, > 0.5 favors control.\\
    \textsuperscript{\textit{2}}Approximate hazard ratio to cross bound.\\
    \textsuperscript{\textit{3}}wAHR is the weighted AHR.\\
    \end{minipage}

# Snapshot test for gs_power_wlr summary as_gt

    \setlength{\LTpost}{0mm}
    \begin{longtable}{lrrrrr}
    \caption*{
    {\large Bound summary for WLR design\textsuperscript{\textit{1}}} \\ 
    {\small WLR approximation of \textasciitilde{}wHR at bound}
    } \\ 
    \toprule
     &  &  &  & \multicolumn{2}{c}{Cumulative boundary crossing probability\textsuperscript{\textit{2}}} \\ 
    \cmidrule(lr){5-6}
    Bound & Z & Nominal p & \textasciitilde{}wHR at bound\textsuperscript{\textit{3}} & Alternate hypothesis & Null hypothesis \\ 
    \midrule\addlinespace[2.5pt]
    \multicolumn{6}{l}{Analysis: 1 Time: 14.9 N: 108 Event: 30 AHR: 0.79 Information fraction: 0.6\textsuperscript{\textit{4}}} \\ 
    \midrule\addlinespace[2.5pt]
    Futility & -1.28 & 0.9000 & 1.5967269 & 0.0268 & 0.1000 \\ 
    Efficacy & 2.67 & 0.0038 & 0.3774019 & 0.0221 & 0.0038 \\ 
    \midrule\addlinespace[2.5pt]
    \multicolumn{6}{l}{Analysis: 2 Time: 19.2 N: 108 Event: 40 AHR: 0.75 Information fraction: 0.8\textsuperscript{\textit{4}}} \\ 
    \midrule\addlinespace[2.5pt]
    Efficacy & 2.29 & 0.0110 & 0.4849272 & 0.0889 & 0.0122 \\ 
    \midrule\addlinespace[2.5pt]
    \multicolumn{6}{l}{Analysis: 3 Time: 24.5 N: 108 Event: 50 AHR: 0.71 Information fraction: 1\textsuperscript{\textit{4}}} \\ 
    \midrule\addlinespace[2.5pt]
    Efficacy & 2.03 & 0.0211 & 0.5630598 & 0.2071 & 0.0250 \\ 
    \bottomrule
    \end{longtable}
    \begin{minipage}{\linewidth}
    \textsuperscript{\textit{1}}this table is generated by gs\_power\_wlr.\\
    \textsuperscript{\textit{2}}the crossing probability.\\
    \textsuperscript{\textit{3}}approximate weighted hazard ratio to cross bound.\\
    \textsuperscript{\textit{4}}wAHR is the weighted AHR.\\
    \end{minipage}

# Snapshot test for gs_power_combo summary as_gt

    \setlength{\LTpost}{0mm}
    \begin{longtable}{lrrrr}
    \caption*{
    {\large Bound summary for MaxCombo design} \\ 
    {\small MaxCombo approximation}
    } \\ 
    \toprule
     &  &  & \multicolumn{2}{c}{Cumulative boundary crossing probability} \\ 
    \cmidrule(lr){4-5}
    Bound & Z & Nominal p\textsuperscript{\textit{1}} & Alternate hypothesis & Null hypothesis \\ 
    \midrule\addlinespace[2.5pt]
    \multicolumn{5}{l}{Analysis: 1 Time: 12 N: 500 Event: 107.4 AHR: 0.84 Event fraction: 0.32\textsuperscript{\textit{2}}} \\ 
    \midrule\addlinespace[2.5pt]
    Futility & -1 & 0.8413 & 0.0293 & 0.0000 \\ 
    Efficacy & 3 & 0.0013 & 0.0175 & 0.0013 \\ 
    \midrule\addlinespace[2.5pt]
    \multicolumn{5}{l}{Analysis: 2 Time: 24 N: 500 Event: 246.3 AHR: 0.72 Event fraction: 0.74\textsuperscript{\textit{2}}} \\ 
    \midrule\addlinespace[2.5pt]
    Futility & 0 & 0.5000 & 0.0314 & 0.0000 \\ 
    Efficacy & 2 & 0.0228 & 0.7261 & 0.0233 \\ 
    \midrule\addlinespace[2.5pt]
    \multicolumn{5}{l}{Analysis: 3 Time: 36 N: 500 Event: 331.3 AHR: 0.68 Event fraction: 1\textsuperscript{\textit{2}}} \\ 
    \midrule\addlinespace[2.5pt]
    Futility & 1 & 0.1587 & 0.0326 & 0.0000 \\ 
    Efficacy & 1 & 0.1587 & 0.9674 & 0.1956 \\ 
    \bottomrule
    \end{longtable}
    \begin{minipage}{\linewidth}
    \textsuperscript{\textit{1}}One-sided p-value for experimental vs control treatment.
                   Value < 0.5 favors experimental, > 0.5 favors control.\\
    \textsuperscript{\textit{2}}EF is event fraction. AHR  is under regular weighted log rank test.\\
    \end{minipage}

# Snapshot test for gs_design_rd summary as_gt

    \setlength{\LTpost}{0mm}
    \begin{longtable}{lrrrrr}
    \caption*{
    {\large Bound summary of Binary Endpoint} \\ 
    {\small measured by risk difference}
    } \\ 
    \toprule
     &  &  &  & \multicolumn{2}{c}{Cumulative boundary crossing probability} \\ 
    \cmidrule(lr){5-6}
    Bound & Z & Nominal p\textsuperscript{\textit{1}} & \textasciitilde{}Risk difference at bound & Alternate hypothesis & Null hypothesis \\ 
    \midrule\addlinespace[2.5pt]
    \multicolumn{6}{l}{Analysis: 1 N: 2433.6 Risk difference: 0.05 Information fraction: 1} \\ 
    \midrule\addlinespace[2.5pt]
    Efficacy & 1.96 & 0.025 & 0.0302 & 0.9 & 0.025 \\ 
    \bottomrule
    \end{longtable}
    \begin{minipage}{\linewidth}
    \textsuperscript{\textit{1}}One-sided p-value for experimental vs control treatment.
                             Value < 0.5 favors experimental, > 0.5 favors control.\\
    \end{minipage}

# Snapshot test for gs_power_rd summary as_gt

    \setlength{\LTpost}{0mm}
    \begin{longtable}{lrrrrr}
    \caption*{
    {\large Bound summary of Binary Endpoint} \\ 
    {\small measured by risk difference}
    } \\ 
    \toprule
     &  &  &  & \multicolumn{2}{c}{Cumulative boundary crossing probability} \\ 
    \cmidrule(lr){5-6}
    Bound & Z & Nominal p\textsuperscript{\textit{1}} & \textasciitilde{}Risk difference at bound & Alternate hypothesis & Null hypothesis \\ 
    \midrule\addlinespace[2.5pt]
    \multicolumn{6}{l}{Analysis: 1 N: 40 Risk difference: 0.05 Information fraction: 0.67} \\ 
    \midrule\addlinespace[2.5pt]
    Futility & -1.28 & 0.9000 & -0.1537 & 0.0444 & 0.1000 \\ 
    Efficacy & 3.71 & 0.0001 & 0.4448 & 0.0005 & 0.0001 \\ 
    \midrule\addlinespace[2.5pt]
    \multicolumn{6}{l}{Analysis: 2 N: 50 Risk difference: 0.05 Information fraction: 0.83} \\ 
    \midrule\addlinespace[2.5pt]
    Efficacy & 2.51 & 0.0060 & 0.2693 & 0.0204 & 0.0060 \\ 
    \midrule\addlinespace[2.5pt]
    \multicolumn{6}{l}{Analysis: 3 N: 60 Risk difference: 0.05 Information fraction: 1} \\ 
    \midrule\addlinespace[2.5pt]
    Efficacy & 1.99 & 0.0231 & 0.1951 & 0.0705 & \textsuperscript{\textit{2}} 0.0238 \\ 
    \bottomrule
    \end{longtable}
    \begin{minipage}{\linewidth}
    \textsuperscript{\textit{1}}One-sided p-value for experimental vs control treatment.
                             Value < 0.5 favors experimental, > 0.5 favors control.\\
    \textsuperscript{\textit{2}}Cumulative alpha for final analysis (0.0238) is less than the full alpha (0.025) when the futility bound is non-binding. The smaller value subtracts the probability of crossing a futility bound before crossing an efficacy bound at a later analysis (0.025 - 0.0012 = 0.0238) under the null hypothesis.\\
    \end{minipage}

# Snapshot test for gs_power_wlr summary as_gt with custom title and subtitle

    \setlength{\LTpost}{0mm}
    \begin{longtable}{lrrrrr}
    \caption*{
    {\large Bound Summary} \\ 
    {\small from gs\_power\_wlr}
    } \\ 
    \toprule
     &  &  &  & \multicolumn{2}{c}{Cumulative boundary crossing probability} \\ 
    \cmidrule(lr){5-6}
    Bound & Z & Nominal p\textsuperscript{\textit{1}} & \textasciitilde{}wHR at bound\textsuperscript{\textit{2}} & Alternate hypothesis & Null hypothesis \\ 
    \midrule\addlinespace[2.5pt]
    \multicolumn{6}{l}{Analysis: 1 Time: 14.9 N: 108 Event: 30 AHR: 0.79 Information fraction: 0.6\textsuperscript{\textit{3}}} \\ 
    \midrule\addlinespace[2.5pt]
    Futility & -1.28 & 0.9000 & 1.5967269 & 0.0268 & 0.1000 \\ 
    Efficacy & 2.67 & 0.0038 & 0.3774019 & 0.0221 & 0.0038 \\ 
    \midrule\addlinespace[2.5pt]
    \multicolumn{6}{l}{Analysis: 2 Time: 19.2 N: 108 Event: 40 AHR: 0.75 Information fraction: 0.8\textsuperscript{\textit{3}}} \\ 
    \midrule\addlinespace[2.5pt]
    Efficacy & 2.29 & 0.0110 & 0.4849272 & 0.0889 & 0.0122 \\ 
    \midrule\addlinespace[2.5pt]
    \multicolumn{6}{l}{Analysis: 3 Time: 24.5 N: 108 Event: 50 AHR: 0.71 Information fraction: 1\textsuperscript{\textit{3}}} \\ 
    \midrule\addlinespace[2.5pt]
    Efficacy & 2.03 & 0.0211 & 0.5630598 & 0.2071 & 0.0250 \\ 
    \bottomrule
    \end{longtable}
    \begin{minipage}{\linewidth}
    \textsuperscript{\textit{1}}One-sided p-value for experimental vs control treatment.
              Value < 0.5 favors experimental, > 0.5 favors control.\\
    \textsuperscript{\textit{2}}Approximate hazard ratio to cross bound.\\
    \textsuperscript{\textit{3}}wAHR is the weighted AHR.\\
    \end{minipage}

# Snapshot test for gs_power_wlr summary as_gt with colname_spanner and colname_spannersub

    \setlength{\LTpost}{0mm}
    \begin{longtable}{lrrrrr}
    \caption*{
    {\large Bound summary for WLR design} \\ 
    {\small WLR approximation of \textasciitilde{}wHR at bound}
    } \\ 
    \toprule
     &  &  &  & \multicolumn{2}{c}{Cumulative probability to cross boundaries} \\ 
    \cmidrule(lr){5-6}
    Bound & Z & Nominal p\textsuperscript{\textit{1}} & \textasciitilde{}wHR at bound\textsuperscript{\textit{2}} & under H1 & under H0 \\ 
    \midrule\addlinespace[2.5pt]
    \multicolumn{6}{l}{Analysis: 1 Time: 14.9 N: 108 Event: 30 AHR: 0.79 Information fraction: 0.6\textsuperscript{\textit{3}}} \\ 
    \midrule\addlinespace[2.5pt]
    Futility & -1.28 & 0.9000 & 1.5967269 & 0.0268 & 0.1000 \\ 
    Efficacy & 2.67 & 0.0038 & 0.3774019 & 0.0221 & 0.0038 \\ 
    \midrule\addlinespace[2.5pt]
    \multicolumn{6}{l}{Analysis: 2 Time: 19.2 N: 108 Event: 40 AHR: 0.75 Information fraction: 0.8\textsuperscript{\textit{3}}} \\ 
    \midrule\addlinespace[2.5pt]
    Efficacy & 2.29 & 0.0110 & 0.4849272 & 0.0889 & 0.0122 \\ 
    \midrule\addlinespace[2.5pt]
    \multicolumn{6}{l}{Analysis: 3 Time: 24.5 N: 108 Event: 50 AHR: 0.71 Information fraction: 1\textsuperscript{\textit{3}}} \\ 
    \midrule\addlinespace[2.5pt]
    Efficacy & 2.03 & 0.0211 & 0.5630598 & 0.2071 & \textsuperscript{\textit{4}} 0.0250 \\ 
    \bottomrule
    \end{longtable}
    \begin{minipage}{\linewidth}
    \textsuperscript{\textit{1}}One-sided p-value for experimental vs control treatment.
              Value < 0.5 favors experimental, > 0.5 favors control.\\
    \textsuperscript{\textit{2}}Approximate hazard ratio to cross bound.\\
    \textsuperscript{\textit{3}}wAHR is the weighted AHR.\\
    \textsuperscript{\textit{4}}Cumulative alpha for final analysis (-Inf) is less than the full alpha (0.025) when the futility bound is non-binding. The smaller value subtracts the probability of crossing a futility bound before crossing an efficacy bound at a later analysis (0.025 - Inf = -Inf) under the null hypothesis.\\
    \end{minipage}

# Snapshot test for gs_power_wlr summary as_gt with custom footnotes

    \setlength{\LTpost}{0mm}
    \begin{longtable}{lrrrrr}
    \caption*{
    {\large Bound summary for WLR design\textsuperscript{\textit{1}}} \\ 
    {\small WLR approximation of \textasciitilde{}wHR at bound}
    } \\ 
    \toprule
     &  &  &  & \multicolumn{2}{c}{Cumulative boundary crossing probability\textsuperscript{\textit{2}}} \\ 
    \cmidrule(lr){5-6}
    Bound & Z & Nominal p & \textasciitilde{}wHR at bound\textsuperscript{\textit{3}} & Alternate hypothesis & Null hypothesis \\ 
    \midrule\addlinespace[2.5pt]
    \multicolumn{6}{l}{Analysis: 1 Time: 14.9 N: 108 Event: 30 AHR: 0.79 Information fraction: 0.6\textsuperscript{\textit{4}}} \\ 
    \midrule\addlinespace[2.5pt]
    Futility & -1.28 & 0.9000 & 1.5967269 & 0.0268 & 0.1000 \\ 
    Efficacy & 2.67 & 0.0038 & 0.3774019 & 0.0221 & 0.0038 \\ 
    \midrule\addlinespace[2.5pt]
    \multicolumn{6}{l}{Analysis: 2 Time: 19.2 N: 108 Event: 40 AHR: 0.75 Information fraction: 0.8\textsuperscript{\textit{4}}} \\ 
    \midrule\addlinespace[2.5pt]
    Efficacy & 2.29 & 0.0110 & 0.4849272 & 0.0889 & 0.0122 \\ 
    \midrule\addlinespace[2.5pt]
    \multicolumn{6}{l}{Analysis: 3 Time: 24.5 N: 108 Event: 50 AHR: 0.71 Information fraction: 1\textsuperscript{\textit{4}}} \\ 
    \midrule\addlinespace[2.5pt]
    Efficacy & 2.03 & 0.0211 & 0.5630598 & 0.2071 & 0.0250 \\ 
    \bottomrule
    \end{longtable}
    \begin{minipage}{\linewidth}
    \textsuperscript{\textit{1}}this table is generated by gs\_power\_wlr.\\
    \textsuperscript{\textit{2}}the crossing probability.\\
    \textsuperscript{\textit{3}}approximate weighted hazard ratio to cross bound.\\
    \textsuperscript{\textit{4}}wAHR is the weighted AHR.\\
    \end{minipage}

# Snapshot test for gs_power_wlr summary as_gt with display_bound

    \setlength{\LTpost}{0mm}
    \begin{longtable}{lrrrrr}
    \caption*{
    {\large Bound summary for WLR design} \\ 
    {\small WLR approximation of \textasciitilde{}wHR at bound}
    } \\ 
    \toprule
     &  &  &  & \multicolumn{2}{c}{Cumulative boundary crossing probability} \\ 
    \cmidrule(lr){5-6}
    Bound & Z & Nominal p\textsuperscript{\textit{1}} & \textasciitilde{}wHR at bound\textsuperscript{\textit{2}} & Alternate hypothesis & Null hypothesis \\ 
    \midrule\addlinespace[2.5pt]
    \multicolumn{6}{l}{Analysis: 1 Time: 14.9 N: 108 Event: 30 AHR: 0.79 Information fraction: 0.6\textsuperscript{\textit{3}}} \\ 
    \midrule\addlinespace[2.5pt]
    Efficacy & 2.67 & 0.0038 & 0.3774019 & 0.0221 & 0.0038 \\ 
    \midrule\addlinespace[2.5pt]
    \multicolumn{6}{l}{Analysis: 2 Time: 19.2 N: 108 Event: 40 AHR: 0.75 Information fraction: 0.8\textsuperscript{\textit{3}}} \\ 
    \midrule\addlinespace[2.5pt]
    Efficacy & 2.29 & 0.0110 & 0.4849272 & 0.0889 & 0.0122 \\ 
    \midrule\addlinespace[2.5pt]
    \multicolumn{6}{l}{Analysis: 3 Time: 24.5 N: 108 Event: 50 AHR: 0.71 Information fraction: 1\textsuperscript{\textit{3}}} \\ 
    \midrule\addlinespace[2.5pt]
    Efficacy & 2.03 & 0.0211 & 0.5630598 & 0.2071 & 0.0250 \\ 
    \bottomrule
    \end{longtable}
    \begin{minipage}{\linewidth}
    \textsuperscript{\textit{1}}One-sided p-value for experimental vs control treatment.
              Value < 0.5 favors experimental, > 0.5 favors control.\\
    \textsuperscript{\textit{2}}Approximate hazard ratio to cross bound.\\
    \textsuperscript{\textit{3}}wAHR is the weighted AHR.\\
    \end{minipage}

# Snapshot test for gs_power_wlr summary as_gt with display_columns

    \setlength{\LTpost}{0mm}
    \begin{longtable}{lrrrr}
    \caption*{
    {\large Bound summary for WLR design} \\ 
    {\small WLR approximation of \textasciitilde{}wHR at bound}
    } \\ 
    \toprule
     &  &  & \multicolumn{2}{c}{Cumulative boundary crossing probability} \\ 
    \cmidrule(lr){4-5}
    Bound & Nominal p\textsuperscript{\textit{1}} & Z & Alternate hypothesis & Null hypothesis \\ 
    \midrule\addlinespace[2.5pt]
    \multicolumn{5}{l}{Analysis: 1 Time: 14.9 N: 108 Event: 30 AHR: 0.79 Information fraction: 0.6\textsuperscript{\textit{2}}} \\ 
    \midrule\addlinespace[2.5pt]
    Futility & 0.9000 & -1.28 & 0.0268 & 0.1000 \\ 
    Efficacy & 0.0038 & 2.67 & 0.0221 & 0.0038 \\ 
    \midrule\addlinespace[2.5pt]
    \multicolumn{5}{l}{Analysis: 2 Time: 19.2 N: 108 Event: 40 AHR: 0.75 Information fraction: 0.8\textsuperscript{\textit{2}}} \\ 
    \midrule\addlinespace[2.5pt]
    Efficacy & 0.0110 & 2.29 & 0.0889 & 0.0122 \\ 
    \midrule\addlinespace[2.5pt]
    \multicolumn{5}{l}{Analysis: 3 Time: 24.5 N: 108 Event: 50 AHR: 0.71 Information fraction: 1\textsuperscript{\textit{2}}} \\ 
    \midrule\addlinespace[2.5pt]
    Efficacy & 0.0211 & 2.03 & 0.2071 & 0.0250 \\ 
    \bottomrule
    \end{longtable}
    \begin{minipage}{\linewidth}
    \textsuperscript{\textit{1}}One-sided p-value for experimental vs control treatment.
              Value < 0.5 favors experimental, > 0.5 favors control.\\
    \textsuperscript{\textit{2}}wAHR is the weighted AHR.\\
    \end{minipage}

