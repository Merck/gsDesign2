# Statistical information under null and alternative hypothesis

In a group sequential design of the \\k\\-th analysis, the Z-score is \\
Z_k = \delta_k / \sqrt{\text{Var}(\delta_k \| H_0)} \\ The statistical
information \\\mathcal I_k\\ is defined as the inverse of the variance
of \\\delta_k\\, i.e., \\ \mathcal I_k = 1 / \text{Var}(\delta_k). \\

## Continuous outcomes

Imagine a trial with a continuous outcome. Let \\X\_{0, i} \sim N(\mu_0,
\sigma^2)\\ for subjects \\i = 1, \ldots, n_0\\ in the control arm and
\\X\_{1,i} \sim N(\mu_1, \sigma^2)\\ for patient \\i\\ (\\i = 1, \ldots,
n_1\\) in the experimental arm.

For a superiority design, the tested hypothesis is \\ H_0: \\ \mu_0 =
\mu_1 \\\\\\ \text{vs.} \\\\\\ H_1:\\ \mu_1 \> \mu_0. \\ Suppose at the
\\k\\-th analysis, there are \\n\_{0k}\\ subjects in the control arm,
and there are \\n\_{1k}\\ subjects in the experimental arm. We have
\\\delta_k\\ as the difference of group means, i.e., \\ \delta_k =
\frac{\sum\_{i=1}^{n\_{1k}} X\_{i,1}}{n\_{1k}} -
\frac{\sum\_{i=1}^{n\_{0k}} X\_{i,0}}{n\_{0k}}. \\ It can be estimated
as \\\widehat\delta_k = \frac{\sum\_{i=1}^{n\_{1k}} x\_{i,1}}{n\_{1k}} -
\frac{\sum\_{i=1}^{n\_{0k}} x\_{i,0}}{n\_{0k}}\\, where \\x\_{i,j}\\ are
the observation of \\X\_{i,j}\\ of subject \\i\\ in arm \\j\\.

The statistical information \\\mathcal I_k\\ is \\ \mathcal I_k^{-1} =
\text{Var}(\delta_k \| H_0) = \sigma^2 (1 / n\_{1k} + 1 / n\_{0k}), \\
under both \\H_0\\ and \\H_1\\, which can be estimated as \\\mathcal I_k
= \widehat\sigma^2 (1 / n\_{1k} + 1 / n\_{0k})\\.

## Binary outcomes

Imagine a trial with a binary outcome. Let \\X\_{0, i} \sim B(p_0)\\ for
patient \\i = 1, \ldots, n_0\\ and \\X\_{1,i} \sim B(p_1)\\ for patient
\\i\\ (\\i = 1, \ldots, n_1\\), where \\p_0\\ and \\p_1\\ are failure
rate probability. Suppose at the \\k\\-th analysis, there are
\\n\_{0k}\\ subjects in the control arm, and there are \\n\_{1k}\\
subjects in the experimental arm. For a superiority design, the null and
alternative hypothesis is \\ H_0: \\ p_0 = p_1 = p \\\\\\ \text{vs.}
\\\\\\ H_1:\\ p_0 \> p_1. \\ The nature-scale treatment effect is \\
\delta_k = \frac{\sum\_{i=1}^{n\_{1k}} X\_{i,1}}{n\_{1k}} -
\frac{\sum\_{i=1}^{n\_{0k}} X\_{i,0}}{n\_{0k}}, \\ It can be estimated
as \\\widehat\delta_k = \frac{\sum\_{i=1}^{n\_{1k}} x\_{i,1}}{n\_{1k}} -
\frac{\sum\_{i=1}^{n\_{0k}} x\_{i,0}}{n\_{0k}}\\, where \\x\_{i,j}\\ are
the observation of \\X\_{i,j}\\ of subject \\i\\ in arm \\j\\.

The statistical information is \\ \mathcal I_k^{-1} =
\text{Var}(\delta_k) = \left\\ \begin{array}{ll} p(1-p)/n\_{1k} +
p(1-p)/n\_{0k} & \text{under } H_0\\ p_1(1-p_1)/n\_{1k} +
p_0(1-p_0)/n\_{0k} & \text{under } H_1\\ \end{array} \right.. \\ Its
estimation is \\ \widehat{\mathcal I}\_k^{-1} = \left\\
\begin{array}{ll} \bar p(1 - \bar p) / n\_{1k} + \bar p(1 - \bar p) /
n\_{0k} & \text{under } H_0\\ \widehat p_1(1-p_1) / n\_{1k} + \widehat
p_0(1 - \widehat p_0)/n\_{0k} & \text{under } H_1\\ \end{array} \right.,
\\ where \\\bar p = \frac{\sum\_{i=1}^{n\_{1k}}x\_{i1} +
\sum\_{i=1}^{n\_{0k}}x\_{i0}}{n\_{1k} + n\_{0k}}\\, \\\widehat p_j =
\frac{\sum\_{i=1}^{n\_{jk}}x\_{ij}}{n\_{jk}}\\ for \\j = 0, 1\\.

## Survival outcome

In many clinical trials, the outcome is the time to some event. For
simplicity, assume the event is death so that each person can only have
one event; the same ideas apply for events that can recur, but in those
cases we restrict attention to the first event for each patients. We use
the logrank statistics to compare the treatment and control arms. If we
assume there are \\N_k\\ total number of deaths at analysis \\k\\. The
numerator of the logrank statistics at analysis \\k\\ is (Proschan, Lan,
and Wittes 2006) \\ \sum\_{i=1}^{N_k} D_i, \\ where \\D_i = O_i - E_i\\
with \\O_i\\ is the indicator that the \\i\\th death occurred in a
treatment patient, and \\E_i = m\_{1i} / (m\_{0i} + m\_{1i})\\ as the
null expectation of \\O_i\\ given the respective numbers, \\m\_{0i}\\
and \\m\_{1i}\\, of control and treatment patients at risk just prior to
the \\i\\th death.

Conditioned on \\m\_{0i}\\ and \\m\_{1i}\\, the \\O_i\\ has a Bernoulli
distribution with parameter \\E_i\\. The null conditional mean and
variance of \\D_i\\ are 0 and \\V_i = E_i(1 − E_i)\\, respectively.

Unconditionally, the \\D_i\\ are uncorrelated, mean 0 random variables
with variance \\E(V_i)\\ under the null hypothesis.

Thus, conditioned on \\N_k\\, we have \\ \begin{array}{ccl} \mathcal
I_k^{-1} & = & \text{Var}(\delta_k) = \sum\_{i=1}^{N_k} \text{Var}(D_i)
= \sum\_{i=1}^{N_k} E(V_i) = E \left( \sum\_{i=1}^{N_k} V_i \right) = E
\left( \sum\_{i=1}^{N_k} E_i(1 − E_i) \right) \\ & = & \left\\
\begin{array}{ll} E\left(\sum\_{i=1}^{N_k} \frac{r}{1+r} \frac{1}{1+r}
\right) & \text{under } H_0\\ E\left(\sum\_{i=1}^{N_k}
\frac{m\_{1i}}{(m\_{0i} + m\_{1i})} \frac{m\_{0i}}{(m\_{0i} +
m\_{1i})}\right) & \text{under } H_1 \end{array} \right., \end{array} \\
where \\r\\ is the randomization ratio. Its estimation is \\
\begin{array}{ccl} \widehat{\mathcal I}\_k^{-1} & = & \left\\
\begin{array}{ll} \sum\_{i=1}^{N_k} \frac{r}{1+r} \frac{1}{1+r} &
\text{under } H_0\\ \sum\_{i=1}^{N_k} \frac{m\_{1i}}{(m\_{0i} +
m\_{1i})} \frac{m\_{0i}}{(m\_{0i} + m\_{1i})} & \text{under } H_1
\end{array} \right.. \end{array} \\

## References

Proschan, Michael A., K. K. Gordon Lan, and Janet Turk Wittes. 2006.
*Statistical Monitoring of Clinical Trials: A Unified Approach*. New
York, NY: Springer.
