Snapshot test for fixed_design_ahr summary as_rtf

```r
path <- tempfile(fileext = ".rtf")
  fixed_design_ahr_example() |>
    summary() |>
    as_rtf(file = path)
cat(readLines(path), sep = "\n")
```

```
{\rtf1\ansi
\deff0\deflang1033
{\fonttbl{\f0\froman\fcharset1\fprq2 Times New Roman;}
{\f1\froman\fcharset161\fprq2 Times New Roman Greek;}
{\f2\fswiss\fcharset161\fprq2 Arial Greek;}
{\f3\fswiss\fcharset0\fprq2 Arial;}
{\f4\fswiss\fcharset1\fprq2 Helvetica;}
{\f5\fswiss\fcharset1\fprq2 Calibri;}
{\f6\froman\fcharset1\fprq2 Georgia;}
{\f7\ffroman\fcharset1\fprq2 Cambria;}
{\f8\fmodern\fcharset0\fprq2 Courier New;}
{\f9\ftech\fcharset2\fprq2 Symbol;}
}


\paperw12240\paperh15840

\margl1800\margr1440\margt2520\margb1800\headery2520\footery1449

{\pard\hyphpar\sb180\sa180\fi0\li0\ri0\qc\fs24{\f0 Fixed Design under AHR Method {\super a}}\par}


\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrdb\brdrw15\clbrdrb\brdrw15\clvertalb\cellx2000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrdb\brdrw15\clbrdrb\brdrw15\clvertalb\cellx3000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrdb\brdrw15\clbrdrb\brdrw15\clvertalb\cellx4000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrdb\brdrw15\clbrdrb\brdrw15\clvertalb\cellx5000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrdb\brdrw15\clbrdrb\brdrw15\clvertalb\cellx6000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrdb\brdrw15\clbrdrb\brdrw15\clvertalb\cellx7000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrdb\brdrw15\clbrdrb\brdrw15\clvertalb\cellx8000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrdb\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrw15\clvertalb\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 Design}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 N}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 Events}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 Time}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 AHR}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 Bound}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 alpha}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 Power}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx2000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx3000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx4000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx5000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx6000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx7000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx8000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 Average hazard ratio}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 463.077965814336}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 324.707677968484}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 36}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.697102041399151}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 1.95996398454005}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.025}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.9}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrdb\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 {\super a} Power computed with average hazard ratio method.}\cell
\intbl\row\pard


}
```

Snapshot test for fixed_design_ahr summary as_rtf with custom title

```r
path <- tempfile(fileext = ".rtf")
  fixed_design_ahr_example() |>
    summary() |>
    as_rtf(
      title = "Fixed design under non-proportional hazards",
      file = path
    )
cat(readLines(path), sep = "\n")
```

```
{\rtf1\ansi
\deff0\deflang1033
{\fonttbl{\f0\froman\fcharset1\fprq2 Times New Roman;}
{\f1\froman\fcharset161\fprq2 Times New Roman Greek;}
{\f2\fswiss\fcharset161\fprq2 Arial Greek;}
{\f3\fswiss\fcharset0\fprq2 Arial;}
{\f4\fswiss\fcharset1\fprq2 Helvetica;}
{\f5\fswiss\fcharset1\fprq2 Calibri;}
{\f6\froman\fcharset1\fprq2 Georgia;}
{\f7\ffroman\fcharset1\fprq2 Cambria;}
{\f8\fmodern\fcharset0\fprq2 Courier New;}
{\f9\ftech\fcharset2\fprq2 Symbol;}
}


\paperw12240\paperh15840

\margl1800\margr1440\margt2520\margb1800\headery2520\footery1449

{\pard\hyphpar\sb180\sa180\fi0\li0\ri0\qc\fs24{\f0 Fixed design under non-proportional hazards {\super a}}\par}


\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrdb\brdrw15\clbrdrb\brdrw15\clvertalb\cellx2000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrdb\brdrw15\clbrdrb\brdrw15\clvertalb\cellx3000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrdb\brdrw15\clbrdrb\brdrw15\clvertalb\cellx4000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrdb\brdrw15\clbrdrb\brdrw15\clvertalb\cellx5000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrdb\brdrw15\clbrdrb\brdrw15\clvertalb\cellx6000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrdb\brdrw15\clbrdrb\brdrw15\clvertalb\cellx7000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrdb\brdrw15\clbrdrb\brdrw15\clvertalb\cellx8000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrdb\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrw15\clvertalb\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 Design}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 N}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 Events}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 Time}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 AHR}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 Bound}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 alpha}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 Power}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx2000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx3000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx4000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx5000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx6000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx7000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx8000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 Average hazard ratio}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 463.077965814336}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 324.707677968484}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 36}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.697102041399151}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 1.95996398454005}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.025}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.9}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrdb\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 {\super a} Power computed with average hazard ratio method.}\cell
\intbl\row\pard


}
```

Snapshot test for fixed_design_ahr summary as_rtf with custom footnote

```r
path <- tempfile(fileext = ".rtf")
  fixed_design_ahr_example() |>
    summary() |>
    as_rtf(
      footnote = "Power computed with average hazard ratio method given the sample size",
      file = path
    )
cat(readLines(path), sep = "\n")
```

```
{\rtf1\ansi
\deff0\deflang1033
{\fonttbl{\f0\froman\fcharset1\fprq2 Times New Roman;}
{\f1\froman\fcharset161\fprq2 Times New Roman Greek;}
{\f2\fswiss\fcharset161\fprq2 Arial Greek;}
{\f3\fswiss\fcharset0\fprq2 Arial;}
{\f4\fswiss\fcharset1\fprq2 Helvetica;}
{\f5\fswiss\fcharset1\fprq2 Calibri;}
{\f6\froman\fcharset1\fprq2 Georgia;}
{\f7\ffroman\fcharset1\fprq2 Cambria;}
{\f8\fmodern\fcharset0\fprq2 Courier New;}
{\f9\ftech\fcharset2\fprq2 Symbol;}
}


\paperw12240\paperh15840

\margl1800\margr1440\margt2520\margb1800\headery2520\footery1449

{\pard\hyphpar\sb180\sa180\fi0\li0\ri0\qc\fs24{\f0 Fixed Design under AHR Method {\super a}}\par}


\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrdb\brdrw15\clbrdrb\brdrw15\clvertalb\cellx2000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrdb\brdrw15\clbrdrb\brdrw15\clvertalb\cellx3000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrdb\brdrw15\clbrdrb\brdrw15\clvertalb\cellx4000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrdb\brdrw15\clbrdrb\brdrw15\clvertalb\cellx5000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrdb\brdrw15\clbrdrb\brdrw15\clvertalb\cellx6000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrdb\brdrw15\clbrdrb\brdrw15\clvertalb\cellx7000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrdb\brdrw15\clbrdrb\brdrw15\clvertalb\cellx8000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrdb\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrw15\clvertalb\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 Design}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 N}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 Events}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 Time}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 AHR}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 Bound}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 alpha}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 Power}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx2000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx3000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx4000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx5000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx6000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx7000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx8000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 Average hazard ratio}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 463.077965814336}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 324.707677968484}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 36}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.697102041399151}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 1.95996398454005}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.025}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.9}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrdb\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 {\super a} Power computed with average hazard ratio method given the sample size}\cell
\intbl\row\pard


}
```

Snapshot test for gs_design_ahr summary as_rtf

```r
path <- tempfile(fileext = ".rtf")
  gs_design_ahr() |>
    summary() |>
    as_rtf(file = path)
cat(readLines(path), sep = "\n")
```

```
{\rtf1\ansi
\deff0\deflang1033
{\fonttbl{\f0\froman\fcharset1\fprq2 Times New Roman;}
{\f1\froman\fcharset161\fprq2 Times New Roman Greek;}
{\f2\fswiss\fcharset161\fprq2 Arial Greek;}
{\f3\fswiss\fcharset0\fprq2 Arial;}
{\f4\fswiss\fcharset1\fprq2 Helvetica;}
{\f5\fswiss\fcharset1\fprq2 Calibri;}
{\f6\froman\fcharset1\fprq2 Georgia;}
{\f7\ffroman\fcharset1\fprq2 Cambria;}
{\f8\fmodern\fcharset0\fprq2 Courier New;}
{\f9\ftech\fcharset2\fprq2 Symbol;}
}


\paperw12240\paperh15840

\margl1800\margr1440\margt2520\margb1800\headery2520\footery1449

{\pard\hyphpar\sb180\sa180\fi0\li0\ri0\qc\fs24{\f0 Bound summary for AHR design}\line\fs24{\f0 AHR approximations of ~HR at bound}\par}


\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrdb\brdrw15\clbrdrb\brdrw15\clvertalb\cellx6000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrdb\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrw15\clvertalb\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 }\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 Cumulative boundary crossing probability}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrw15\clvertalb\cellx1500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrw15\clvertalb\cellx3000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrw15\clvertalb\cellx4500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrw15\clvertalb\cellx6000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrw15\clvertalb\cellx7500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrw15\clvertalb\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 Bound}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 Z}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 Nominal p {\super a}}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 ~HR at bound {\super b}}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 Alternate hypothesis}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 Null hypothesis}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 Analysis: 1 Time: 36 N: 476 Events: 291.9 AHR: 0.68 Information fraction: 1}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx1500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx3000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx4500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx6000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx7500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 Efficacy}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 1.96}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.025}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.795}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.9}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.025}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrdb\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 {\super a} One-sided p-value for experimental vs control treatment. Value < 0.5 favors experimental, > 0.5 favors control.\line{\super b} Approximate hazard ratio to cross bound.}\cell
\intbl\row\pard


}
```

Snapshot test for gs_design_wlr summary as_rtf

```r
path <- tempfile(fileext = ".rtf")
  gs_design_wlr() |>
    summary() |>
    as_rtf(file = path)
cat(readLines(path), sep = "\n")
```

```
{\rtf1\ansi
\deff0\deflang1033
{\fonttbl{\f0\froman\fcharset1\fprq2 Times New Roman;}
{\f1\froman\fcharset161\fprq2 Times New Roman Greek;}
{\f2\fswiss\fcharset161\fprq2 Arial Greek;}
{\f3\fswiss\fcharset0\fprq2 Arial;}
{\f4\fswiss\fcharset1\fprq2 Helvetica;}
{\f5\fswiss\fcharset1\fprq2 Calibri;}
{\f6\froman\fcharset1\fprq2 Georgia;}
{\f7\ffroman\fcharset1\fprq2 Cambria;}
{\f8\fmodern\fcharset0\fprq2 Courier New;}
{\f9\ftech\fcharset2\fprq2 Symbol;}
}


\paperw12240\paperh15840

\margl1800\margr1440\margt2520\margb1800\headery2520\footery1449

{\pard\hyphpar\sb180\sa180\fi0\li0\ri0\qc\fs24{\f0 Bound summary for WLR design}\line\fs24{\f0 WLR approximation of ~wHR at bound}\par}


\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrdb\brdrw15\clbrdrb\brdrw15\clvertalb\cellx6000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrdb\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrw15\clvertalb\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 }\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 Cumulative boundary crossing probability}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrw15\clvertalb\cellx1500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrw15\clvertalb\cellx3000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrw15\clvertalb\cellx4500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrw15\clvertalb\cellx6000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrw15\clvertalb\cellx7500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrw15\clvertalb\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 Bound}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 Z}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 Nominal p {\super a}}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 ~wHR at bound {\super b}}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 Alternate hypothesis}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 Null hypothesis}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 Analysis: 1 Time: 36 N: 471.1 Events: 289 AHR: 0.68 Information fraction: 1 {\super c}}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx1500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx3000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx4500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx6000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx7500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 Efficacy}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 1.96}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.025}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.7941}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.9}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.025}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrdb\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 {\super a} One-sided p-value for experimental vs control treatment. Value < 0.5 favors experimental, > 0.5 favors control.\line{\super b} Approximate hazard ratio to cross bound.\line{\super c} wAHR is the weighted AHR.}\cell
\intbl\row\pard


}
```

Snapshot test for gs_power_wlr summary as_rtf

```r
path <- tempfile(fileext = ".rtf")
  gs_power_wlr_example() |>
    summary() |>
    as_rtf(file = path)
cat(readLines(path), sep = "\n")
```

```
{\rtf1\ansi
\deff0\deflang1033
{\fonttbl{\f0\froman\fcharset1\fprq2 Times New Roman;}
{\f1\froman\fcharset161\fprq2 Times New Roman Greek;}
{\f2\fswiss\fcharset161\fprq2 Arial Greek;}
{\f3\fswiss\fcharset0\fprq2 Arial;}
{\f4\fswiss\fcharset1\fprq2 Helvetica;}
{\f5\fswiss\fcharset1\fprq2 Calibri;}
{\f6\froman\fcharset1\fprq2 Georgia;}
{\f7\ffroman\fcharset1\fprq2 Cambria;}
{\f8\fmodern\fcharset0\fprq2 Courier New;}
{\f9\ftech\fcharset2\fprq2 Symbol;}
}


\paperw12240\paperh15840

\margl1800\margr1440\margt2520\margb1800\headery2520\footery1449

{\pard\hyphpar\sb180\sa180\fi0\li0\ri0\qc\fs24{\f0 Bound summary for WLR design}\line\fs24{\f0 WLR approximation of ~wHR at bound}\par}


\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrdb\brdrw15\clbrdrb\brdrw15\clvertalb\cellx6000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrdb\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrw15\clvertalb\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 }\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 Cumulative boundary crossing probability}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrw15\clvertalb\cellx1500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrw15\clvertalb\cellx3000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrw15\clvertalb\cellx4500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrw15\clvertalb\cellx6000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrw15\clvertalb\cellx7500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrw15\clvertalb\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 Bound}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 Z}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 Nominal p {\super a}}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 ~wHR at bound {\super b}}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 Alternate hypothesis}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 Null hypothesis}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 Analysis: 1 Time: 14.9 N: 108 Events: 30 AHR: 0.79 Information fraction: 0.6 {\super c}}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx1500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx3000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx4500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx6000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx7500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 Futility}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 -1.17}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.8798}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 1.535}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.0341}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.1202}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx1500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx3000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx4500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx6000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx7500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 Efficacy}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 2.68}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.0037}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.3765}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.0217}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.0037}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 Analysis: 2 Time: 19.2 N: 108 Events: 40 AHR: 0.75 Information fraction: 0.8 {\super c}}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx1500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx3000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx4500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx6000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx7500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 Futility}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 -0.66}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.7452}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 1.232}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.0664}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.2664}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx1500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx3000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx4500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx6000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx7500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 Efficacy}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 2.29}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.011}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.4846}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.0886}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.0121}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 Analysis: 3 Time: 24.5 N: 108 Events: 50 AHR: 0.71 Information fraction: 1 {\super c}}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx1500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx3000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx4500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx6000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx7500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 Futility}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 -0.22}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.5881}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 1.065}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.1002}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.4319}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx1500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx3000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx4500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx6000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx7500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 Efficacy}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 2.03}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.0212}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.5631}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.2071}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.025}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrdb\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 {\super a} One-sided p-value for experimental vs control treatment. Value < 0.5 favors experimental, > 0.5 favors control.\line{\super b} Approximate hazard ratio to cross bound.\line{\super c} wAHR is the weighted AHR.}\cell
\intbl\row\pard


}
```

Snapshot test for gs_design_rd summary as_rtf

```r
path <- tempfile(fileext = ".rtf")
  gs_design_rd() |>
    summary() |>
    as_rtf(file = path)
cat(readLines(path), sep = "\n")
```

```
{\rtf1\ansi
\deff0\deflang1033
{\fonttbl{\f0\froman\fcharset1\fprq2 Times New Roman;}
{\f1\froman\fcharset161\fprq2 Times New Roman Greek;}
{\f2\fswiss\fcharset161\fprq2 Arial Greek;}
{\f3\fswiss\fcharset0\fprq2 Arial;}
{\f4\fswiss\fcharset1\fprq2 Helvetica;}
{\f5\fswiss\fcharset1\fprq2 Calibri;}
{\f6\froman\fcharset1\fprq2 Georgia;}
{\f7\ffroman\fcharset1\fprq2 Cambria;}
{\f8\fmodern\fcharset0\fprq2 Courier New;}
{\f9\ftech\fcharset2\fprq2 Symbol;}
}


\paperw12240\paperh15840

\margl1800\margr1440\margt2520\margb1800\headery2520\footery1449

{\pard\hyphpar\sb180\sa180\fi0\li0\ri0\qc\fs24{\f0 Bound summary of Binary Endpoint}\line\fs24{\f0 measured by risk difference}\par}


\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrdb\brdrw15\clbrdrb\brdrw15\clvertalb\cellx6000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrdb\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrw15\clvertalb\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 }\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 Cumulative boundary crossing probability}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrw15\clvertalb\cellx1500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrw15\clvertalb\cellx3000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrw15\clvertalb\cellx4500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrw15\clvertalb\cellx6000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrw15\clvertalb\cellx7500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrw15\clvertalb\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 Bound}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 Z}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 Nominal p {\super a}}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 ~Risk difference at bound}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 Alternate hypothesis}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 Null hypothesis}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 Analysis: 1 N: 2423.1 Risk difference: 0.05 Information fraction: 1}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx1500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx3000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx4500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx6000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx7500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 Efficacy}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 1.96}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.025}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.0302}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.9}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.025}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrdb\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 {\super a} One-sided p-value for experimental vs control treatment. Value < 0.5 favors experimental, > 0.5 favors control.}\cell
\intbl\row\pard


}
```

Snapshot test for gs_power_wlr summary as_rtf with custom title and subtitle

```r
path <- tempfile(fileext = ".rtf")
  gs_power_wlr_example() |>
    summary() |>
    as_rtf(
      title = "Bound Summary",
      subtitle = "from gs_power_wlr",
      file = path
    )
cat(readLines(path), sep = "\n")
```

```
{\rtf1\ansi
\deff0\deflang1033
{\fonttbl{\f0\froman\fcharset1\fprq2 Times New Roman;}
{\f1\froman\fcharset161\fprq2 Times New Roman Greek;}
{\f2\fswiss\fcharset161\fprq2 Arial Greek;}
{\f3\fswiss\fcharset0\fprq2 Arial;}
{\f4\fswiss\fcharset1\fprq2 Helvetica;}
{\f5\fswiss\fcharset1\fprq2 Calibri;}
{\f6\froman\fcharset1\fprq2 Georgia;}
{\f7\ffroman\fcharset1\fprq2 Cambria;}
{\f8\fmodern\fcharset0\fprq2 Courier New;}
{\f9\ftech\fcharset2\fprq2 Symbol;}
}


\paperw12240\paperh15840

\margl1800\margr1440\margt2520\margb1800\headery2520\footery1449

{\pard\hyphpar\sb180\sa180\fi0\li0\ri0\qc\fs24{\f0 Bound Summary}\line\fs24{\f0 from gs_power_wlr}\par}


\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrdb\brdrw15\clbrdrb\brdrw15\clvertalb\cellx6000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrdb\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrw15\clvertalb\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 }\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 Cumulative boundary crossing probability}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrw15\clvertalb\cellx1500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrw15\clvertalb\cellx3000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrw15\clvertalb\cellx4500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrw15\clvertalb\cellx6000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrw15\clvertalb\cellx7500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrw15\clvertalb\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 Bound}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 Z}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 Nominal p {\super a}}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 ~wHR at bound {\super b}}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 Alternate hypothesis}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 Null hypothesis}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 Analysis: 1 Time: 14.9 N: 108 Events: 30 AHR: 0.79 Information fraction: 0.6 {\super c}}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx1500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx3000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx4500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx6000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx7500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 Futility}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 -1.17}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.8798}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 1.535}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.0341}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.1202}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx1500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx3000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx4500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx6000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx7500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 Efficacy}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 2.68}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.0037}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.3765}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.0217}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.0037}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 Analysis: 2 Time: 19.2 N: 108 Events: 40 AHR: 0.75 Information fraction: 0.8 {\super c}}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx1500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx3000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx4500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx6000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx7500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 Futility}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 -0.66}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.7452}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 1.232}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.0664}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.2664}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx1500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx3000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx4500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx6000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx7500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 Efficacy}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 2.29}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.011}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.4846}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.0886}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.0121}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 Analysis: 3 Time: 24.5 N: 108 Events: 50 AHR: 0.71 Information fraction: 1 {\super c}}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx1500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx3000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx4500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx6000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx7500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 Futility}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 -0.22}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.5881}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 1.065}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.1002}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.4319}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx1500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx3000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx4500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx6000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx7500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 Efficacy}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 2.03}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.0212}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.5631}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.2071}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.025}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrdb\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 {\super a} One-sided p-value for experimental vs control treatment. Value < 0.5 favors experimental, > 0.5 favors control.\line{\super b} Approximate hazard ratio to cross bound.\line{\super c} wAHR is the weighted AHR.}\cell
\intbl\row\pard


}
```

Snapshot test for gs_power_wlr summary as_rtf with custom spanner and sub-spanner

```r
path <- tempfile(fileext = ".rtf")
  gs_power_wlr_example() |>
    summary() |>
    as_rtf(
      colname_spanner = "Cumulative probability to cross boundaries",
      colname_spannersub = c("under H1", "under H0"),
      file = path
    )
cat(readLines(path), sep = "\n")
```

```
{\rtf1\ansi
\deff0\deflang1033
{\fonttbl{\f0\froman\fcharset1\fprq2 Times New Roman;}
{\f1\froman\fcharset161\fprq2 Times New Roman Greek;}
{\f2\fswiss\fcharset161\fprq2 Arial Greek;}
{\f3\fswiss\fcharset0\fprq2 Arial;}
{\f4\fswiss\fcharset1\fprq2 Helvetica;}
{\f5\fswiss\fcharset1\fprq2 Calibri;}
{\f6\froman\fcharset1\fprq2 Georgia;}
{\f7\ffroman\fcharset1\fprq2 Cambria;}
{\f8\fmodern\fcharset0\fprq2 Courier New;}
{\f9\ftech\fcharset2\fprq2 Symbol;}
}


\paperw12240\paperh15840

\margl1800\margr1440\margt2520\margb1800\headery2520\footery1449

{\pard\hyphpar\sb180\sa180\fi0\li0\ri0\qc\fs24{\f0 Bound summary for WLR design}\line\fs24{\f0 WLR approximation of ~wHR at bound}\par}


\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrdb\brdrw15\clbrdrb\brdrw15\clvertalb\cellx6000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrdb\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrw15\clvertalb\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 }\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 Cumulative probability to cross boundaries}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrw15\clvertalb\cellx1500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrw15\clvertalb\cellx3000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrw15\clvertalb\cellx4500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrw15\clvertalb\cellx6000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrw15\clvertalb\cellx7500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrw15\clvertalb\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 Bound}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 Z}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 Nominal p {\super a}}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 ~wHR at bound {\super b}}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 under H1}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 under H0}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 Analysis: 1 Time: 14.9 N: 108 Events: 30 AHR: 0.79 Information fraction: 0.6 {\super c}}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx1500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx3000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx4500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx6000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx7500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 Futility}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 -1.17}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.8798}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 1.535}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.0341}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.1202}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx1500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx3000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx4500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx6000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx7500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 Efficacy}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 2.68}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.0037}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.3765}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.0217}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.0037}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 Analysis: 2 Time: 19.2 N: 108 Events: 40 AHR: 0.75 Information fraction: 0.8 {\super c}}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx1500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx3000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx4500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx6000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx7500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 Futility}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 -0.66}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.7452}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 1.232}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.0664}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.2664}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx1500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx3000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx4500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx6000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx7500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 Efficacy}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 2.29}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.011}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.4846}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.0886}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.0121}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 Analysis: 3 Time: 24.5 N: 108 Events: 50 AHR: 0.71 Information fraction: 1 {\super c}}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx1500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx3000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx4500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx6000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx7500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 Futility}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 -0.22}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.5881}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 1.065}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.1002}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.4319}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx1500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx3000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx4500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx6000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx7500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 Efficacy}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 2.03}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.0212}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.5631}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.2071}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.025}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrdb\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 {\super a} One-sided p-value for experimental vs control treatment. Value < 0.5 favors experimental, > 0.5 favors control.\line{\super b} Approximate hazard ratio to cross bound.\line{\super c} wAHR is the weighted AHR.}\cell
\intbl\row\pard


}
```

Snapshot test for gs_power_wlr summary as_rtf with custom footnote

```r
path <- tempfile(fileext = ".rtf")
  gs_power_wlr_example() |>
    summary() |>
    as_rtf(
      footnote = list(
        content = c(
          "approximate weighted hazard ratio to cross bound.",
          "wAHR is the weighted AHR.",
          "the crossing probability.",
          "this table is generated by gs_power_wlr."
        ),
        location = c("~wHR at bound", NA, NA, NA),
        attr = c("colname", "analysis", "spanner", "title")
      ),
      file = path
    )
cat(readLines(path), sep = "\n")
```

```
{\rtf1\ansi
\deff0\deflang1033
{\fonttbl{\f0\froman\fcharset1\fprq2 Times New Roman;}
{\f1\froman\fcharset161\fprq2 Times New Roman Greek;}
{\f2\fswiss\fcharset161\fprq2 Arial Greek;}
{\f3\fswiss\fcharset0\fprq2 Arial;}
{\f4\fswiss\fcharset1\fprq2 Helvetica;}
{\f5\fswiss\fcharset1\fprq2 Calibri;}
{\f6\froman\fcharset1\fprq2 Georgia;}
{\f7\ffroman\fcharset1\fprq2 Cambria;}
{\f8\fmodern\fcharset0\fprq2 Courier New;}
{\f9\ftech\fcharset2\fprq2 Symbol;}
}


\paperw12240\paperh15840

\margl1800\margr1440\margt2520\margb1800\headery2520\footery1449

{\pard\hyphpar\sb180\sa180\fi0\li0\ri0\qc\fs24{\f0 Bound summary for WLR design \super d}\line\fs24{\f0 WLR approximation of ~wHR at bound}\par}


\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrdb\brdrw15\clbrdrb\brdrw15\clvertalb\cellx6000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrdb\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrw15\clvertalb\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 }\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 Cumulative boundary crossing probability {\super c}}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrw15\clvertalb\cellx1500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrw15\clvertalb\cellx3000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrw15\clvertalb\cellx4500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrw15\clvertalb\cellx6000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrw15\clvertalb\cellx7500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrw15\clvertalb\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 Bound}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 Z}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 Nominal p}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 ~wHR at bound {\super a}}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 Alternate hypothesis}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 Null hypothesis}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 Analysis: 1 Time: 14.9 N: 108 Events: 30 AHR: 0.79 Information fraction: 0.6 {\super b}}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx1500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx3000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx4500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx6000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx7500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 Futility}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 -1.17}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.8798}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 1.535}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.0341}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.1202}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx1500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx3000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx4500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx6000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx7500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 Efficacy}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 2.68}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.0037}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.3765}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.0217}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.0037}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 Analysis: 2 Time: 19.2 N: 108 Events: 40 AHR: 0.75 Information fraction: 0.8 {\super b}}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx1500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx3000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx4500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx6000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx7500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 Futility}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 -0.66}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.7452}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 1.232}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.0664}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.2664}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx1500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx3000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx4500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx6000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx7500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 Efficacy}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 2.29}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.011}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.4846}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.0886}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.0121}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 Analysis: 3 Time: 24.5 N: 108 Events: 50 AHR: 0.71 Information fraction: 1 {\super b}}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx1500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx3000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx4500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx6000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx7500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 Futility}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 -0.22}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.5881}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 1.065}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.1002}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.4319}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx1500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx3000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx4500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx6000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx7500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 Efficacy}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 2.03}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.0212}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.5631}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.2071}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.025}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrdb\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 {\super a} approximate weighted hazard ratio to cross bound.\line{\super b} wAHR is the weighted AHR.\line{\super c} the crossing probability.\line{\super d} this table is generated by gs_power_wlr.}\cell
\intbl\row\pard


}
```

Snapshot test for gs_power_wlr summary as_rtf with custom columns to display

```r
path <- tempfile(fileext = ".rtf")
  gs_power_wlr_example() |>
    summary() |>
    as_rtf(
      display_columns = c("Analysis", "Bound", "Nominal p", "Z", "Probability"),
      file = path
    )
cat(readLines(path), sep = "\n")
```

```
{\rtf1\ansi
\deff0\deflang1033
{\fonttbl{\f0\froman\fcharset1\fprq2 Times New Roman;}
{\f1\froman\fcharset161\fprq2 Times New Roman Greek;}
{\f2\fswiss\fcharset161\fprq2 Arial Greek;}
{\f3\fswiss\fcharset0\fprq2 Arial;}
{\f4\fswiss\fcharset1\fprq2 Helvetica;}
{\f5\fswiss\fcharset1\fprq2 Calibri;}
{\f6\froman\fcharset1\fprq2 Georgia;}
{\f7\ffroman\fcharset1\fprq2 Cambria;}
{\f8\fmodern\fcharset0\fprq2 Courier New;}
{\f9\ftech\fcharset2\fprq2 Symbol;}
}


\paperw12240\paperh15840

\margl1800\margr1440\margt2520\margb1800\headery2520\footery1449

{\pard\hyphpar\sb180\sa180\fi0\li0\ri0\qc\fs24{\f0 Bound summary for WLR design}\line\fs24{\f0 WLR approximation of ~wHR at bound}\par}


\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrdb\brdrw15\clbrdrb\brdrw15\clvertalb\cellx5400
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrdb\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrw15\clvertalb\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 }\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 Cumulative boundary crossing probability}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrw15\clvertalb\cellx1800
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrw15\clvertalb\cellx3600
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrw15\clvertalb\cellx5400
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrw15\clvertalb\cellx7200
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrw15\clvertalb\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 Bound}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 Nominal p {\super a}}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 Z}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 Alternate hypothesis}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 Null hypothesis}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 Analysis: 1 Time: 14.9 N: 108 Events: 30 AHR: 0.79 Information fraction: 0.6 {\super b}}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx1800
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx3600
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx5400
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx7200
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 Futility}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.8798}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 -1.17}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.0341}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.1202}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx1800
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx3600
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx5400
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx7200
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 Efficacy}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.0037}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 2.68}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.0217}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.0037}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 Analysis: 2 Time: 19.2 N: 108 Events: 40 AHR: 0.75 Information fraction: 0.8 {\super b}}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx1800
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx3600
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx5400
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx7200
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 Futility}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.7452}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 -0.66}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.0664}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.2664}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx1800
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx3600
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx5400
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx7200
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 Efficacy}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.011}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 2.29}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.0886}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.0121}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 Analysis: 3 Time: 24.5 N: 108 Events: 50 AHR: 0.71 Information fraction: 1 {\super b}}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx1800
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx3600
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx5400
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx7200
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 Futility}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.5881}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 -0.22}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.1002}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.4319}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx1800
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx3600
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx5400
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx7200
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 Efficacy}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.0212}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 2.03}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.2071}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.025}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrdb\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 {\super a} One-sided p-value for experimental vs control treatment. Value < 0.5 favors experimental, > 0.5 favors control.\line{\super b} wAHR is the weighted AHR.}\cell
\intbl\row\pard


}
```

Snapshot test for gs_power_wlr summary as_rtf with efficacy bounds displayed

```r
path <- tempfile(fileext = ".rtf")
  gs_power_wlr_example() |>
    summary() |>
    as_rtf(display_bound = "Efficacy", file = path)
cat(readLines(path), sep = "\n")
```

```
{\rtf1\ansi
\deff0\deflang1033
{\fonttbl{\f0\froman\fcharset1\fprq2 Times New Roman;}
{\f1\froman\fcharset161\fprq2 Times New Roman Greek;}
{\f2\fswiss\fcharset161\fprq2 Arial Greek;}
{\f3\fswiss\fcharset0\fprq2 Arial;}
{\f4\fswiss\fcharset1\fprq2 Helvetica;}
{\f5\fswiss\fcharset1\fprq2 Calibri;}
{\f6\froman\fcharset1\fprq2 Georgia;}
{\f7\ffroman\fcharset1\fprq2 Cambria;}
{\f8\fmodern\fcharset0\fprq2 Courier New;}
{\f9\ftech\fcharset2\fprq2 Symbol;}
}


\paperw12240\paperh15840

\margl1800\margr1440\margt2520\margb1800\headery2520\footery1449

{\pard\hyphpar\sb180\sa180\fi0\li0\ri0\qc\fs24{\f0 Bound summary for WLR design}\line\fs24{\f0 WLR approximation of ~wHR at bound}\par}


\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrdb\brdrw15\clbrdrb\brdrw15\clvertalb\cellx6000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrdb\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrw15\clvertalb\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 }\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 Cumulative boundary crossing probability}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrw15\clvertalb\cellx1500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrw15\clvertalb\cellx3000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrw15\clvertalb\cellx4500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrw15\clvertalb\cellx6000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrw15\clvertalb\cellx7500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrw15\clvertalb\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 Bound}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 Z}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 Nominal p {\super a}}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 ~wHR at bound {\super b}}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 Alternate hypothesis}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 Null hypothesis}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 Analysis: 1 Time: 14.9 N: 108 Events: 30 AHR: 0.79 Information fraction: 0.6 {\super c}}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx1500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx3000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx4500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx6000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx7500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 Efficacy}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 2.68}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.0037}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.3765}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.0217}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.0037}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 Analysis: 2 Time: 19.2 N: 108 Events: 40 AHR: 0.75 Information fraction: 0.8 {\super c}}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx1500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx3000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx4500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx6000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx7500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 Efficacy}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 2.29}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.011}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.4846}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.0886}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.0121}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 Analysis: 3 Time: 24.5 N: 108 Events: 50 AHR: 0.71 Information fraction: 1 {\super c}}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx1500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx3000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx4500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx6000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx7500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 Efficacy}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 2.03}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.0212}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.5631}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.2071}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.025}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrdb\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 {\super a} One-sided p-value for experimental vs control treatment. Value < 0.5 favors experimental, > 0.5 favors control.\line{\super b} Approximate hazard ratio to cross bound.\line{\super c} wAHR is the weighted AHR.}\cell
\intbl\row\pard


}
```

Snapshot test for gs_power_wlr summary as_rtf with futility bounds displayed

```r
path <- tempfile(fileext = ".rtf")
  gs_power_wlr_example(binding = TRUE) |>
    summary() |>
    as_rtf(display_bound = "Futility", file = path)
cat(readLines(path), sep = "\n")
```

```
{\rtf1\ansi
\deff0\deflang1033
{\fonttbl{\f0\froman\fcharset1\fprq2 Times New Roman;}
{\f1\froman\fcharset161\fprq2 Times New Roman Greek;}
{\f2\fswiss\fcharset161\fprq2 Arial Greek;}
{\f3\fswiss\fcharset0\fprq2 Arial;}
{\f4\fswiss\fcharset1\fprq2 Helvetica;}
{\f5\fswiss\fcharset1\fprq2 Calibri;}
{\f6\froman\fcharset1\fprq2 Georgia;}
{\f7\ffroman\fcharset1\fprq2 Cambria;}
{\f8\fmodern\fcharset0\fprq2 Courier New;}
{\f9\ftech\fcharset2\fprq2 Symbol;}
}


\paperw12240\paperh15840

\margl1800\margr1440\margt2520\margb1800\headery2520\footery1449

{\pard\hyphpar\sb180\sa180\fi0\li0\ri0\qc\fs24{\f0 Bound summary for WLR design}\line\fs24{\f0 WLR approximation of ~wHR at bound}\par}


\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrdb\brdrw15\clbrdrb\brdrw15\clvertalb\cellx6000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrdb\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrw15\clvertalb\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 }\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 Cumulative boundary crossing probability}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrw15\clvertalb\cellx1500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrw15\clvertalb\cellx3000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrw15\clvertalb\cellx4500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrw15\clvertalb\cellx6000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrw15\clvertalb\cellx7500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrw15\clvertalb\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 Bound}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 Z}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 Nominal p {\super a}}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 ~wHR at bound {\super b}}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 Alternate hypothesis}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 Null hypothesis}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 Analysis: 1 Time: 14.9 N: 108 Events: 30 AHR: 0.79 Information fraction: 0.6 {\super c}}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx1500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx3000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx4500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx6000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx7500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 Futility}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 -1.17}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.8798}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 1.535}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.0341}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.1202}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 Analysis: 2 Time: 19.2 N: 108 Events: 40 AHR: 0.75 Information fraction: 0.8 {\super c}}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx1500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx3000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx4500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx6000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx7500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 Futility}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 -0.66}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.7452}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 1.232}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.0664}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.2664}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 Analysis: 3 Time: 24.5 N: 108 Events: 50 AHR: 0.71 Information fraction: 1 {\super c}}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx1500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx3000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx4500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx6000
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx7500
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrs\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrs\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 Futility}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 -0.22}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.5881}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 1.065}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.1002}\cell
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\qc\fs18{\f0 0.4319}\cell
\intbl\row\pard
\trowd\trgaph108\trleft0\trqc
\clbrdrl\brdrs\brdrw15\clbrdrt\brdrw15\clbrdrr\brdrs\brdrw15\clbrdrb\brdrdb\brdrw15\clvertalt\cellx9000
\pard\hyphpar0\sb15\sa15\fi0\li0\ri0\ql\fs18{\f0 {\super a} One-sided p-value for experimental vs control treatment. Value < 0.5 favors experimental, > 0.5 favors control.\line{\super b} Approximate hazard ratio to cross bound.\line{\super c} wAHR is the weighted AHR.}\cell
\intbl\row\pard


}
```

