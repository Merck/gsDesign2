# Snapshot test for fixed_design summary as_gt

    \begin{table}[t]
    \caption*{
    {\large Fixed Design under AHR Method\textsuperscript{\textit{1}}}
    } 
    \fontsize{12.0pt}{14.4pt}\selectfont
    \begin{tabular*}{\linewidth}{@{\extracolsep{\fill}}lrrrrrr}
    \toprule
    Design & N & Events & Time & Bound & alpha & Power \\ 
    \midrule\addlinespace[2.5pt]
    Average hazard ratio & 463.078 & 324.7077 & 36 & 1.959964 & 0.025 & 0.9 \\ 
    \bottomrule
    \end{tabular*}
    \begin{minipage}{\linewidth}
    \textsuperscript{\textit{1}}Power computed with average hazard ratio method.\\
    \end{minipage}
    \end{table}

# Snapshot test for fixed_design summary as_gt with custom title and footnote

    \begin{table}[t]
    \caption*{
    {\large Custom Title\textsuperscript{\textit{1}}}
    } 
    \fontsize{12.0pt}{14.4pt}\selectfont
    \begin{tabular*}{\linewidth}{@{\extracolsep{\fill}}lrrrrrr}
    \toprule
    Design & N & Events & Time & Bound & alpha & Power \\ 
    \midrule\addlinespace[2.5pt]
    Average hazard ratio & 463.078 & 324.7077 & 36 & 1.959964 & 0.025 & 0.9 \\ 
    \bottomrule
    \end{tabular*}
    \begin{minipage}{\linewidth}
    \textsuperscript{\textit{1}}Custom footnote.\\
    \end{minipage}
    \end{table}

# Snapshot test for fixed_design_fh summary as_gt

    \begin{table}[t]
    \caption*{
    {\large Fixed Design under Fleming-Harrington Method\textsuperscript{\textit{1}}}
    } 
    \fontsize{12.0pt}{14.4pt}\selectfont
    \begin{tabular*}{\linewidth}{@{\extracolsep{\fill}}lrrrrrr}
    \toprule
    Design & N & Events & Time & Bound & alpha & Power \\ 
    \midrule\addlinespace[2.5pt]
    Fleming-Harrington FH(0, 0) (logrank) & 458.3509 & 321.3931 & 36 & 1.959964 & 0.025 & 0.9 \\ 
    \bottomrule
    \end{tabular*}
    \begin{minipage}{\linewidth}
    \textsuperscript{\textit{1}}Power for Fleming-Harrington test  FH(0, 0) (logrank) using method of Yung and Liu.\\
    \end{minipage}
    \end{table}

# Snapshot test for gs_design_ahr summary as_gt

    \begin{table}[t]
    \caption*{
    {\large Bound summary for AHR design} \\ 
    {\small AHR approximations of \textasciitilde{}HR at bound}
    } 
    \fontsize{12.0pt}{14.4pt}\selectfont
    \begin{tabular*}{\linewidth}{@{\extracolsep{\fill}}lrrrrr}
    \toprule
     &  &  &  & \multicolumn{2}{c}{Cumulative boundary crossing probability} \\ 
    \cmidrule(lr){5-6}
    Bound & Z & Nominal p\textsuperscript{\textit{1}} & \textasciitilde{}HR at bound\textsuperscript{\textit{2}} & Alternate hypothesis & Null hypothesis \\ 
    \midrule\addlinespace[2.5pt]
    \multicolumn{6}{l}{Analysis: 1 Time: 36 N: 476 Events: 291.9 AHR: 0.68 Information fraction: 1} \\[2.5pt] 
    \midrule\addlinespace[2.5pt]
    Efficacy & 1.96 & 0.025 & 0.795 & 0.9 & 0.025 \\ 
    \bottomrule
    \end{tabular*}
    \begin{minipage}{\linewidth}
    \textsuperscript{\textit{1}}One-sided p-value for experimental vs control treatment. Value < 0.5 favors experimental, > 0.5 favors control.\\
    \textsuperscript{\textit{2}}Approximate hazard ratio to cross bound.\\
    \end{minipage}
    \end{table}

# Snapshot test for gs_power_ahr summary as_gt

    \begin{table}[t]
    \caption*{
    {\large Bound summary for AHR design} \\ 
    {\small AHR approximations of \textasciitilde{}HR at bound}
    } 
    \fontsize{12.0pt}{14.4pt}\selectfont
    \begin{tabular*}{\linewidth}{@{\extracolsep{\fill}}lrrrrr}
    \toprule
     &  &  &  & \multicolumn{2}{c}{Cumulative boundary crossing probability} \\ 
    \cmidrule(lr){5-6}
    Bound & Z & Nominal p\textsuperscript{\textit{1}} & \textasciitilde{}HR at bound\textsuperscript{\textit{2}} & Alternate hypothesis & Null hypothesis \\ 
    \midrule\addlinespace[2.5pt]
    \multicolumn{6}{l}{Analysis: 1 Time: 14.9 N: 108 Events: 30 AHR: 0.79 Information fraction: 0.6} \\[2.5pt] 
    \midrule\addlinespace[2.5pt]
    Futility & -1.17 & 0.8792 & 1.5336 & 0.0349 & 0.1208 \\ 
    Efficacy & 2.67 & 0.0038 & 0.3774 & 0.0231 & 0.0038 \\ 
    \midrule\addlinespace[2.5pt]
    \multicolumn{6}{l}{Analysis: 2 Time: 19.2 N: 108 Events: 40 AHR: 0.74 Information fraction: 0.8} \\[2.5pt] 
    \midrule\addlinespace[2.5pt]
    Futility & -0.66 & 0.7462 & 1.2331 & 0.0668 & 0.2655 \\ 
    Efficacy & 2.29 & 0.0110 & 0.4849 & 0.0897 & 0.0122 \\ 
    \midrule\addlinespace[2.5pt]
    \multicolumn{6}{l}{Analysis: 3 Time: 24.5 N: 108 Events: 50 AHR: 0.71 Information fraction: 1} \\[2.5pt] 
    \midrule\addlinespace[2.5pt]
    Futility & -0.23 & 0.5897 & 1.0662 & 0.1008 & 0.4303 \\ 
    Efficacy & 2.03 & 0.0211 & 0.5631 & 0.2070 & 0.0250 \\ 
    \bottomrule
    \end{tabular*}
    \begin{minipage}{\linewidth}
    \textsuperscript{\textit{1}}One-sided p-value for experimental vs control treatment. Value < 0.5 favors experimental, > 0.5 favors control.\\
    \textsuperscript{\textit{2}}Approximate hazard ratio to cross bound.\\
    \end{minipage}
    \end{table}

# Snapshot test for gs_design_wlr summary as_gt

    \begin{table}[t]
    \caption*{
    {\large Bound summary for WLR design} \\ 
    {\small WLR approximation of \textasciitilde{}wHR at bound}
    } 
    \fontsize{12.0pt}{14.4pt}\selectfont
    \begin{tabular*}{\linewidth}{@{\extracolsep{\fill}}lrrrrr}
    \toprule
     &  &  &  & \multicolumn{2}{c}{Cumulative boundary crossing probability} \\ 
    \cmidrule(lr){5-6}
    Bound & Z & Nominal p\textsuperscript{\textit{1}} & \textasciitilde{}wHR at bound\textsuperscript{\textit{2}} & Alternate hypothesis & Null hypothesis \\ 
    \midrule\addlinespace[2.5pt]
    \multicolumn{6}{l}{Analysis: 1 Time: 36 N: 471.1 Events: 289 AHR: 0.68 Information fraction: 1\textsuperscript{\textit{3}}} \\[2.5pt] 
    \midrule\addlinespace[2.5pt]
    Efficacy & 1.96 & 0.025 & 0.7941 & 0.9 & 0.025 \\ 
    \bottomrule
    \end{tabular*}
    \begin{minipage}{\linewidth}
    \textsuperscript{\textit{1}}One-sided p-value for experimental vs control treatment. Value < 0.5 favors experimental, > 0.5 favors control.\\
    \textsuperscript{\textit{2}}Approximate hazard ratio to cross bound.\\
    \textsuperscript{\textit{3}}wAHR is the weighted AHR.\\
    \end{minipage}
    \end{table}

# Snapshot test for gs_power_wlr summary as_gt

    \begin{table}[t]
    \caption*{
    {\large Bound summary for WLR design\textsuperscript{\textit{1}}} \\ 
    {\small WLR approximation of \textasciitilde{}wHR at bound}
    } 
    \fontsize{12.0pt}{14.4pt}\selectfont
    \begin{tabular*}{\linewidth}{@{\extracolsep{\fill}}lrrrrr}
    \toprule
     &  &  &  & \multicolumn{2}{c}{Cumulative boundary crossing probability\textsuperscript{\textit{2}}} \\ 
    \cmidrule(lr){5-6}
    Bound & Z & Nominal p & \textasciitilde{}wHR at bound\textsuperscript{\textit{3}} & Alternate hypothesis & Null hypothesis \\ 
    \midrule\addlinespace[2.5pt]
    \multicolumn{6}{l}{Analysis: 1 Time: 14.9 N: 108 Events: 30 AHR: 0.79 Information fraction: 0.6\textsuperscript{\textit{4}}} \\[2.5pt] 
    \midrule\addlinespace[2.5pt]
    Futility & -1.17 & 0.8798 & 1.5353 & 0.0341 & 0.1202 \\ 
    Efficacy & 2.68 & 0.0037 & 0.3765 & 0.0217 & 0.0037 \\ 
    \midrule\addlinespace[2.5pt]
    \multicolumn{6}{l}{Analysis: 2 Time: 19.2 N: 108 Events: 40 AHR: 0.75 Information fraction: 0.8\textsuperscript{\textit{4}}} \\[2.5pt] 
    \midrule\addlinespace[2.5pt]
    Futility & -0.66 & 0.7452 & 1.2319 & 0.0664 & 0.2664 \\ 
    Efficacy & 2.29 & 0.0110 & 0.4846 & 0.0886 & 0.0121 \\ 
    \midrule\addlinespace[2.5pt]
    \multicolumn{6}{l}{Analysis: 3 Time: 24.5 N: 108 Events: 50 AHR: 0.71 Information fraction: 1\textsuperscript{\textit{4}}} \\[2.5pt] 
    \midrule\addlinespace[2.5pt]
    Futility & -0.22 & 0.5881 & 1.0650 & 0.1002 & 0.4319 \\ 
    Efficacy & 2.03 & 0.0212 & 0.5631 & 0.2071 & 0.0250 \\ 
    \bottomrule
    \end{tabular*}
    \begin{minipage}{\linewidth}
    \textsuperscript{\textit{1}}this table is generated by gs\_power\_wlr.\\
    \textsuperscript{\textit{2}}the crossing probability.\\
    \textsuperscript{\textit{3}}approximate weighted hazard ratio to cross bound.\\
    \textsuperscript{\textit{4}}wAHR is the weighted AHR.\\
    \end{minipage}
    \end{table}

# Snapshot test for gs_power_combo summary as_gt

    \begin{table}[t]
    \caption*{
    {\large Bound summary for MaxCombo design} \\ 
    {\small MaxCombo approximation}
    } 
    \fontsize{12.0pt}{14.4pt}\selectfont
    \begin{tabular*}{\linewidth}{@{\extracolsep{\fill}}lrrrr}
    \toprule
     &  &  & \multicolumn{2}{c}{Cumulative boundary crossing probability} \\ 
    \cmidrule(lr){4-5}
    Bound & Z & Nominal p\textsuperscript{\textit{1}} & Alternate hypothesis & Null hypothesis \\ 
    \midrule\addlinespace[2.5pt]
    \multicolumn{5}{l}{Analysis: 1 Time: 12 N: 500 Events: 107.4 AHR: 0.84 Event fraction: 0.32\textsuperscript{\textit{2}}} \\[2.5pt] 
    \midrule\addlinespace[2.5pt]
    Futility & -1 & 0.8413 & 0.0293 & 0.0000 \\ 
    Efficacy & 3 & 0.0013 & 0.0175 & 0.0013 \\ 
    \midrule\addlinespace[2.5pt]
    \multicolumn{5}{l}{Analysis: 2 Time: 24 N: 500 Events: 246.3 AHR: 0.72 Event fraction: 0.74\textsuperscript{\textit{2}}} \\[2.5pt] 
    \midrule\addlinespace[2.5pt]
    Futility & 0 & 0.5000 & 0.0314 & 0.0000 \\ 
    Efficacy & 2 & 0.0228 & 0.7261 & 0.0233 \\ 
    \midrule\addlinespace[2.5pt]
    \multicolumn{5}{l}{Analysis: 3 Time: 36 N: 500 Events: 331.3 AHR: 0.68 Event fraction: 1\textsuperscript{\textit{2}}} \\[2.5pt] 
    \midrule\addlinespace[2.5pt]
    Futility & 1 & 0.1587 & 0.0326 & 0.0000 \\ 
    Efficacy & 1 & 0.1587 & 0.9674 & 0.1956 \\ 
    \bottomrule
    \end{tabular*}
    \begin{minipage}{\linewidth}
    \textsuperscript{\textit{1}}One-sided p-value for experimental vs control treatment. Value < 0.5 favors experimental, > 0.5 favors control.\\
    \textsuperscript{\textit{2}}EF is event fraction. AHR is under regular weighted log rank test.\\
    \end{minipage}
    \end{table}

# Snapshot test for gs_design_rd summary as_gt

    \begin{table}[t]
    \caption*{
    {\large Bound summary of Binary Endpoint} \\ 
    {\small measured by risk difference}
    } 
    \fontsize{12.0pt}{14.4pt}\selectfont
    \begin{tabular*}{\linewidth}{@{\extracolsep{\fill}}lrrrrr}
    \toprule
     &  &  &  & \multicolumn{2}{c}{Cumulative boundary crossing probability} \\ 
    \cmidrule(lr){5-6}
    Bound & Z & Nominal p\textsuperscript{\textit{1}} & \textasciitilde{}Risk difference at bound & Alternate hypothesis & Null hypothesis \\ 
    \midrule\addlinespace[2.5pt]
    \multicolumn{6}{l}{Analysis: 1 N: 2423.1 Risk difference: 0.05 Information fraction: 1} \\[2.5pt] 
    \midrule\addlinespace[2.5pt]
    Efficacy & 1.96 & 0.025 & 0.0302 & 0.9 & 0.025 \\ 
    \bottomrule
    \end{tabular*}
    \begin{minipage}{\linewidth}
    \textsuperscript{\textit{1}}One-sided p-value for experimental vs control treatment. Value < 0.5 favors experimental, > 0.5 favors control.\\
    \end{minipage}
    \end{table}

# Snapshot test for gs_power_rd summary as_gt

    \begin{table}[t]
    \caption*{
    {\large Bound summary of Binary Endpoint} \\ 
    {\small measured by risk difference}
    } 
    \fontsize{12.0pt}{14.4pt}\selectfont
    \begin{tabular*}{\linewidth}{@{\extracolsep{\fill}}lrrrrr}
    \toprule
     &  &  &  & \multicolumn{2}{c}{Cumulative boundary crossing probability} \\ 
    \cmidrule(lr){5-6}
    Bound & Z & Nominal p\textsuperscript{\textit{1}} & \textasciitilde{}Risk difference at bound & Alternate hypothesis & Null hypothesis \\ 
    \midrule\addlinespace[2.5pt]
    \multicolumn{6}{l}{Analysis: 1 N: 40 Risk difference: 0.05 Information fraction: 0.67} \\[2.5pt] 
    \midrule\addlinespace[2.5pt]
    Futility & -1.28 & 0.9000 & -0.1537 & 0.0444 & 0.1000 \\ 
    Efficacy & 3.71 & 0.0001 & 0.4448 & 0.0005 & 0.0001 \\ 
    \midrule\addlinespace[2.5pt]
    \multicolumn{6}{l}{Analysis: 2 N: 50 Risk difference: 0.05 Information fraction: 0.83} \\[2.5pt] 
    \midrule\addlinespace[2.5pt]
    Efficacy & 2.51 & 0.0060 & 0.2693 & 0.0204 & 0.0060 \\ 
    \midrule\addlinespace[2.5pt]
    \multicolumn{6}{l}{Analysis: 3 N: 60 Risk difference: 0.05 Information fraction: 1} \\[2.5pt] 
    \midrule\addlinespace[2.5pt]
    Efficacy & 1.99 & 0.0231 & 0.1951 & 0.0705 & 0.0238\textsuperscript{\textit{2}} \\ 
    \bottomrule
    \end{tabular*}
    \begin{minipage}{\linewidth}
    \textsuperscript{\textit{1}}One-sided p-value for experimental vs control treatment. Value < 0.5 favors experimental, > 0.5 favors control.\\
    \textsuperscript{\textit{2}}Cumulative alpha for final analysis (0.0238) is less than the full alpha (0.025) when the futility bound is non-binding. The smaller value subtracts the probability of crossing a futility bound before crossing an efficacy bound at a later analysis (0.025 - 0.0012 = 0.0238) under the null hypothesis.\\
    \end{minipage}
    \end{table}

# Snapshot test for gs_power_wlr summary as_gt with custom title and subtitle

    \begin{table}[t]
    \caption*{
    {\large Bound Summary} \\ 
    {\small from gs\_power\_wlr}
    } 
    \fontsize{12.0pt}{14.4pt}\selectfont
    \begin{tabular*}{\linewidth}{@{\extracolsep{\fill}}lrrrrr}
    \toprule
     &  &  &  & \multicolumn{2}{c}{Cumulative boundary crossing probability} \\ 
    \cmidrule(lr){5-6}
    Bound & Z & Nominal p\textsuperscript{\textit{1}} & \textasciitilde{}wHR at bound\textsuperscript{\textit{2}} & Alternate hypothesis & Null hypothesis \\ 
    \midrule\addlinespace[2.5pt]
    \multicolumn{6}{l}{Analysis: 1 Time: 14.9 N: 108 Events: 30 AHR: 0.79 Information fraction: 0.6\textsuperscript{\textit{3}}} \\[2.5pt] 
    \midrule\addlinespace[2.5pt]
    Futility & -1.17 & 0.8798 & 1.5353 & 0.0341 & 0.1202 \\ 
    Efficacy & 2.68 & 0.0037 & 0.3765 & 0.0217 & 0.0037 \\ 
    \midrule\addlinespace[2.5pt]
    \multicolumn{6}{l}{Analysis: 2 Time: 19.2 N: 108 Events: 40 AHR: 0.75 Information fraction: 0.8\textsuperscript{\textit{3}}} \\[2.5pt] 
    \midrule\addlinespace[2.5pt]
    Futility & -0.66 & 0.7452 & 1.2319 & 0.0664 & 0.2664 \\ 
    Efficacy & 2.29 & 0.0110 & 0.4846 & 0.0886 & 0.0121 \\ 
    \midrule\addlinespace[2.5pt]
    \multicolumn{6}{l}{Analysis: 3 Time: 24.5 N: 108 Events: 50 AHR: 0.71 Information fraction: 1\textsuperscript{\textit{3}}} \\[2.5pt] 
    \midrule\addlinespace[2.5pt]
    Futility & -0.22 & 0.5881 & 1.0650 & 0.1002 & 0.4319 \\ 
    Efficacy & 2.03 & 0.0212 & 0.5631 & 0.2071 & 0.0250 \\ 
    \bottomrule
    \end{tabular*}
    \begin{minipage}{\linewidth}
    \textsuperscript{\textit{1}}One-sided p-value for experimental vs control treatment. Value < 0.5 favors experimental, > 0.5 favors control.\\
    \textsuperscript{\textit{2}}Approximate hazard ratio to cross bound.\\
    \textsuperscript{\textit{3}}wAHR is the weighted AHR.\\
    \end{minipage}
    \end{table}

# Snapshot test for gs_power_wlr summary as_gt with colname_spanner and colname_spannersub

    \begin{table}[t]
    \caption*{
    {\large Bound summary for WLR design} \\ 
    {\small WLR approximation of \textasciitilde{}wHR at bound}
    } 
    \fontsize{12.0pt}{14.4pt}\selectfont
    \begin{tabular*}{\linewidth}{@{\extracolsep{\fill}}lrrrrr}
    \toprule
     &  &  &  & \multicolumn{2}{c}{Cumulative probability to cross boundaries} \\ 
    \cmidrule(lr){5-6}
    Bound & Z & Nominal p\textsuperscript{\textit{1}} & \textasciitilde{}wHR at bound\textsuperscript{\textit{2}} & under H1 & under H0 \\ 
    \midrule\addlinespace[2.5pt]
    \multicolumn{6}{l}{Analysis: 1 Time: 14.9 N: 108 Events: 30 AHR: 0.79 Information fraction: 0.6\textsuperscript{\textit{3}}} \\[2.5pt] 
    \midrule\addlinespace[2.5pt]
    Futility & -1.17 & 0.8798 & 1.5353 & 0.0341 & 0.1202 \\ 
    Efficacy & 2.68 & 0.0037 & 0.3765 & 0.0217 & 0.0037 \\ 
    \midrule\addlinespace[2.5pt]
    \multicolumn{6}{l}{Analysis: 2 Time: 19.2 N: 108 Events: 40 AHR: 0.75 Information fraction: 0.8\textsuperscript{\textit{3}}} \\[2.5pt] 
    \midrule\addlinespace[2.5pt]
    Futility & -0.66 & 0.7452 & 1.2319 & 0.0664 & 0.2664 \\ 
    Efficacy & 2.29 & 0.0110 & 0.4846 & 0.0886 & 0.0121 \\ 
    \midrule\addlinespace[2.5pt]
    \multicolumn{6}{l}{Analysis: 3 Time: 24.5 N: 108 Events: 50 AHR: 0.71 Information fraction: 1\textsuperscript{\textit{3}}} \\[2.5pt] 
    \midrule\addlinespace[2.5pt]
    Futility & -0.22 & 0.5881 & 1.0650 & 0.1002 & 0.4319 \\ 
    Efficacy & 2.03 & 0.0212 & 0.5631 & 0.2071 & 0.0250 \\ 
    \bottomrule
    \end{tabular*}
    \begin{minipage}{\linewidth}
    \textsuperscript{\textit{1}}One-sided p-value for experimental vs control treatment. Value < 0.5 favors experimental, > 0.5 favors control.\\
    \textsuperscript{\textit{2}}Approximate hazard ratio to cross bound.\\
    \textsuperscript{\textit{3}}wAHR is the weighted AHR.\\
    \end{minipage}
    \end{table}

# Snapshot test for gs_power_wlr summary as_gt with custom footnotes

    \begin{table}[t]
    \caption*{
    {\large Bound summary for WLR design\textsuperscript{\textit{1}}} \\ 
    {\small WLR approximation of \textasciitilde{}wHR at bound}
    } 
    \fontsize{12.0pt}{14.4pt}\selectfont
    \begin{tabular*}{\linewidth}{@{\extracolsep{\fill}}lrrrrr}
    \toprule
     &  &  &  & \multicolumn{2}{c}{Cumulative boundary crossing probability\textsuperscript{\textit{2}}} \\ 
    \cmidrule(lr){5-6}
    Bound & Z & Nominal p & \textasciitilde{}wHR at bound\textsuperscript{\textit{3}} & Alternate hypothesis & Null hypothesis \\ 
    \midrule\addlinespace[2.5pt]
    \multicolumn{6}{l}{Analysis: 1 Time: 14.9 N: 108 Events: 30 AHR: 0.79 Information fraction: 0.6\textsuperscript{\textit{4}}} \\[2.5pt] 
    \midrule\addlinespace[2.5pt]
    Futility & -1.17 & 0.8798 & 1.5353 & 0.0341 & 0.1202 \\ 
    Efficacy & 2.68 & 0.0037 & 0.3765 & 0.0217 & 0.0037 \\ 
    \midrule\addlinespace[2.5pt]
    \multicolumn{6}{l}{Analysis: 2 Time: 19.2 N: 108 Events: 40 AHR: 0.75 Information fraction: 0.8\textsuperscript{\textit{4}}} \\[2.5pt] 
    \midrule\addlinespace[2.5pt]
    Futility & -0.66 & 0.7452 & 1.2319 & 0.0664 & 0.2664 \\ 
    Efficacy & 2.29 & 0.0110 & 0.4846 & 0.0886 & 0.0121 \\ 
    \midrule\addlinespace[2.5pt]
    \multicolumn{6}{l}{Analysis: 3 Time: 24.5 N: 108 Events: 50 AHR: 0.71 Information fraction: 1\textsuperscript{\textit{4}}} \\[2.5pt] 
    \midrule\addlinespace[2.5pt]
    Futility & -0.22 & 0.5881 & 1.0650 & 0.1002 & 0.4319 \\ 
    Efficacy & 2.03 & 0.0212 & 0.5631 & 0.2071 & 0.0250 \\ 
    \bottomrule
    \end{tabular*}
    \begin{minipage}{\linewidth}
    \textsuperscript{\textit{1}}this table is generated by gs\_power\_wlr.\\
    \textsuperscript{\textit{2}}the crossing probability.\\
    \textsuperscript{\textit{3}}approximate weighted hazard ratio to cross bound.\\
    \textsuperscript{\textit{4}}wAHR is the weighted AHR.\\
    \end{minipage}
    \end{table}

# Snapshot test for gs_power_wlr summary as_gt with display_bound

    \begin{table}[t]
    \caption*{
    {\large Bound summary for WLR design} \\ 
    {\small WLR approximation of \textasciitilde{}wHR at bound}
    } 
    \fontsize{12.0pt}{14.4pt}\selectfont
    \begin{tabular*}{\linewidth}{@{\extracolsep{\fill}}lrrrrr}
    \toprule
     &  &  &  & \multicolumn{2}{c}{Cumulative boundary crossing probability} \\ 
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
    \textsuperscript{\textit{1}}One-sided p-value for experimental vs control treatment. Value < 0.5 favors experimental, > 0.5 favors control.\\
    \textsuperscript{\textit{2}}Approximate hazard ratio to cross bound.\\
    \textsuperscript{\textit{3}}wAHR is the weighted AHR.\\
    \end{minipage}
    \end{table}

# Snapshot test for gs_power_wlr summary as_gt with display_columns

    \begin{table}[t]
    \caption*{
    {\large Bound summary for WLR design} \\ 
    {\small WLR approximation of \textasciitilde{}wHR at bound}
    } 
    \fontsize{12.0pt}{14.4pt}\selectfont
    \begin{tabular*}{\linewidth}{@{\extracolsep{\fill}}lrrrr}
    \toprule
     &  &  & \multicolumn{2}{c}{Cumulative boundary crossing probability} \\ 
    \cmidrule(lr){4-5}
    Bound & Nominal p\textsuperscript{\textit{1}} & Z & Alternate hypothesis & Null hypothesis \\ 
    \midrule\addlinespace[2.5pt]
    \multicolumn{5}{l}{Analysis: 1 Time: 14.9 N: 108 Events: 30 AHR: 0.79 Information fraction: 0.6\textsuperscript{\textit{2}}} \\[2.5pt] 
    \midrule\addlinespace[2.5pt]
    Futility & 0.8798 & -1.17 & 0.0341 & 0.1202 \\ 
    Efficacy & 0.0037 & 2.68 & 0.0217 & 0.0037 \\ 
    \midrule\addlinespace[2.5pt]
    \multicolumn{5}{l}{Analysis: 2 Time: 19.2 N: 108 Events: 40 AHR: 0.75 Information fraction: 0.8\textsuperscript{\textit{2}}} \\[2.5pt] 
    \midrule\addlinespace[2.5pt]
    Futility & 0.7452 & -0.66 & 0.0664 & 0.2664 \\ 
    Efficacy & 0.0110 & 2.29 & 0.0886 & 0.0121 \\ 
    \midrule\addlinespace[2.5pt]
    \multicolumn{5}{l}{Analysis: 3 Time: 24.5 N: 108 Events: 50 AHR: 0.71 Information fraction: 1\textsuperscript{\textit{2}}} \\[2.5pt] 
    \midrule\addlinespace[2.5pt]
    Futility & 0.5881 & -0.22 & 0.1002 & 0.4319 \\ 
    Efficacy & 0.0212 & 2.03 & 0.2071 & 0.0250 \\ 
    \bottomrule
    \end{tabular*}
    \begin{minipage}{\linewidth}
    \textsuperscript{\textit{1}}One-sided p-value for experimental vs control treatment. Value < 0.5 favors experimental, > 0.5 favors control.\\
    \textsuperscript{\textit{2}}wAHR is the weighted AHR.\\
    \end{minipage}
    \end{table}

