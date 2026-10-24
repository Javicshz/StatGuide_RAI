# LaTeX versions of topic_formulas (content_data.R), typeset with KaTeX in the
# Formulas tab. Item i matches item i of the same topic; "" means words only.
# Keep both lists in sync when editing formulas.

topic_formulas_tex <- list(
  s3_1 = c(
    r"(\text{relative frequency} = \frac{f}{n})",
    r"(\text{percent} = \frac{f}{n}\times 100)",
    r"(F_k = \sum_{i=1}^{k} f_i)"
  ),
  s3_2 = c(r"(\text{count} = \frac{\text{total}\times\text{percent}}{100})"),
  s3_3 = c(r"(\text{class width} = \text{next lower class limit} - \text{current lower class limit})"),
  s3_4 = c(r"(\frac{\text{larger value}}{\text{smaller value}})"),
  s4_1 = c(r"(\bar{x} = \frac{\sum_{i=1}^{n} x_i}{n})", "", "", ""),
  s4_2 = c(
    r"(\text{range} = x_{\max} - x_{\min})",
    r"(s^2 = \frac{\sum_{i=1}^{n}(x_i - \bar{x})^2}{n-1})",
    r"(s = \sqrt{s^2})",
    r"(\mathrm{CV} = \frac{s}{\bar{x}}\times 100\%,\qquad \bar{x}>0)"
  ),
  s4_3 = c(
    r"(\mathrm{IQR} = Q_3 - Q_1)",
    r"(Q_1 - 1.5\,\mathrm{IQR},\qquad Q_3 + 1.5\,\mathrm{IQR})",
    r"(L = \frac{p}{100}\,n)"
  ),
  s4_5 = c(r"(\bar{x}_{\text{grouped}} = \frac{\sum_i m_i f_i}{\sum_i f_i},\qquad m_i=\text{class midpoint},\quad f_i=\text{class frequency})"),
  s4_6 = c(r"(\hat{p} = \frac{x}{n})", r"(\text{percent} = \text{proportion}\times 100)"),
  s5_1 = c(r"(-1 \le r \le 1)", ""),
  s5_2 = c(
    r"(\hat{y} = b_0 + b_1 x)",
    r"(b_1 = r\frac{s_y}{s_x})",
    r"(b_0 = \bar{y} - b_1\bar{x})"
  ),
  s5_3 = c(
    r"(e_i = y_i - \hat{y}_i)",
    r"(R^2 = r^2\qquad (\text{simple least-squares regression with an intercept}))",
    r"(\text{percent explained} = R^2\times 100\%)"
  ),
  s5_4 = c("", r"(\hat{y} = b_0 + b_1 x)"),
  s6_1 = c(
    r"(P(A) = \frac{\text{number of favorable outcomes}}{\text{total number of outcomes}}\qquad (\text{equally likely outcomes}))",
    r"(P(A^c) = 1 - P(A))"
  ),
  s6_2 = c(
    r"(P(A\cup B) = P(A) + P(B) - P(A\cap B))",
    r"(P(A\cap B) = 0)"
  ),
  s6_3 = c(
    r"(P(A\mid B) = \frac{P(A\cap B)}{P(B)},\qquad P(B)>0)",
    r"(P(A\cap B) = P(A)\,P(B\mid A),\qquad P(A)>0)",
    r"(P(A\cap B) = P(A)\,P(B))"
  ),
  s6_4 = c(
    r"({}_nP_r = \frac{n!}{(n-r)!})",
    r"(\binom{n}{r} = \frac{n!}{r!(n-r)!})"
  ),
  s6_5 = c(r"(P(A\mid B) = \frac{P(B\mid A)P(A)}{P(B)},\qquad P(B)>0,\quad P(A)>0)"),
  s7_1 = c(r"(0\le P(X=x)\le 1\quad\text{for every }x,\qquad \sum_x P(X=x)=1)"),
  s7_2 = c(
    r"(E(X) = \sum_x x\,P(X=x))",
    r"(\operatorname{Var}(X) = \sum_x (x-\mu)^2 P(X=x))",
    r"(\sigma = \sqrt{\operatorname{Var}(X)})"
  ),
  s7_3 = c(
    r"(P(X=x) = \frac{1}{\text{number of outcomes}})",
    r"(\mu = \frac{\text{lowest value} + \text{highest value}}{2}\qquad (\text{equally spaced outcomes}))"
  ),
  s7_4 = c(
    r"(P(X=x) = \binom{n}{x}p^x(1-p)^{n-x})",
    r"(\mu = np)",
    r"(\sigma = \sqrt{np(1-p)})"
  ),
  s7_5 = c(
    r"(P(X=x) = \frac{e^{-\lambda}\lambda^x}{x!})",
    r"(\mu = \sigma^2 = \lambda)"
  ),
  s7_6 = c(
    "",
    r"(P(X=x) = \frac{\binom{\text{successes}}{x}\binom{\text{failures}}{n-x}}{\binom{\text{total}}{n}})"
  ),
  s8_1 = c(
    r"(\mu = \frac{a+b}{2})",
    r"(\sigma = \frac{b-a}{\sqrt{12}})",
    r"(P(c<X<d) = \frac{d-c}{b-a},\qquad a\le c<d\le b,\quad a<b)"
  ),
  s8_2 = c(
    "",
    r"(\begin{aligned}P(\mu-\sigma<X<\mu+\sigma)&\approx 68\%\\P(\mu-2\sigma<X<\mu+2\sigma)&\approx 95\%\\P(\mu-3\sigma<X<\mu+3\sigma)&\approx 99.7\%\end{aligned})"
  ),
  s8_3 = c(r"(z = \frac{x-\mu}{\sigma})", r"(\mu_Z=0,\qquad \sigma_Z=1)"),
  s8_4 = c(
    r"(P(a<X<b) = P(X<b)-P(X<a))",
    r"(x = \mu + z\sigma)"
  ),
  s8_6 = c(r"(\mu=np)", r"(\sigma=\sqrt{np(1-p)})", r"(\text{corrected boundary} = \text{count boundary}\pm 0.5)"),
  s9_1 = c("", ""),
  s9_2 = c(
    r"(\mu_{\bar{X}} = \mu)",
    r"(\operatorname{SE}(\bar{X}) = \frac{\sigma}{\sqrt{n}})",
    r"(z = \frac{\bar{x}-\mu}{\sigma/\sqrt{n}})"
  ),
  s9_3 = c(
    r"(\mu_{\hat{p}} = p)",
    r"(\operatorname{SE}(\hat{p}) = \sqrt{\frac{p(1-p)}{n}})",
    r"(z = \frac{\hat{p}-p}{\sqrt{p(1-p)/n}})"
  ),
  s10_1 = c(r"(\bar{x})", r"(\hat{p}=\frac{x}{n})", r"(s^2)"),
  s10_2 = c(r"(\bar{x}\pm z_{\alpha/2}\frac{\sigma}{\sqrt{n}})"),
  s10_3 = c(r"(\bar{x}\pm t_{\alpha/2,\,n-1}\frac{s}{\sqrt{n}})", r"(\mathrm{df}=n-1)"),
  s10_4 = c(r"(\hat{p}\pm z_{\alpha/2}\sqrt{\frac{\hat{p}(1-\hat{p})}{n}})"),
  s10_5 = c("", ""),
  s11_1 = c("", "", r"(\text{reject }H_0\quad\text{when}\quad p\text{-value}\le\alpha)"),
  s11_2 = c(r"(z=\frac{\bar{x}-\mu_0}{\sigma/\sqrt{n}})"),
  s11_3 = c(r"(t=\frac{\bar{x}-\mu_0}{s/\sqrt{n}})", r"(\mathrm{df}=n-1)"),
  s11_5 = c(r"(z=\frac{\hat{p}-p_0}{\sqrt{p_0(1-p_0)/n}})"),
  s11_6 = c(""),
  s12_1 = c(
    r"(\operatorname{SE}(\bar{X}_1-\bar{X}_2)=\sqrt{\frac{\sigma_1^2}{n_1}+\frac{\sigma_2^2}{n_2}})",
    r"(z=\frac{(\bar{x}_1-\bar{x}_2)-0}{\sqrt{\sigma_1^2/n_1+\sigma_2^2/n_2}}\qquad (H_0:\mu_1-\mu_2=0))"
  ),
  s12_2 = c(
    r"(\widehat{\operatorname{SE}}(\bar{X}_1-\bar{X}_2)=\sqrt{\frac{s_1^2}{n_1}+\frac{s_2^2}{n_2}})",
    ""
  ),
  s12_3 = c(
    r"(\bar{d}\pm t_{\alpha/2,\,n-1}\frac{s_d}{\sqrt{n}})",
    r"(t=\frac{\bar{d}-0}{s_d/\sqrt{n}}\qquad (H_0:\mu_d=0))"
  ),
  s12_4 = c(
    r"(\hat{p}_1-\hat{p}_2)",
    r"(\widehat{\operatorname{SE}}(\hat{p}_1-\hat{p}_2)=\sqrt{\frac{\hat{p}_1\hat{q}_1}{n_1}+\frac{\hat{p}_2\hat{q}_2}{n_2}},\qquad \hat{q}_i=1-\hat{p}_i)",
    r"(\hat{p}_{\text{pooled}}=\frac{x_1+x_2}{n_1+n_2})"
  ),
  s12_5 = c(
    "",
    r"(F=\frac{s_1^2}{s_2^2}\qquad (H_0:\sigma_1^2=\sigma_2^2))",
    r"(\frac{\sigma_1^2}{\sigma_2^2})"
  ),
  s16_2 = c(
    r"(E_i=n p_i)",
    r"(\chi^2=\sum_{i=1}^{k}\frac{(O_i-E_i)^2}{E_i})",
    r"(\mathrm{df}=k-1,\qquad k=\text{number of categories})"
  ),
  s16_3 = c(
    r"(E=\frac{\text{row total}\times\text{column total}}{\text{grand total}})",
    r"(\chi^2=\sum_{i=1}^{r}\sum_{j=1}^{c}\frac{(O_{ij}-E_{ij})^2}{E_{ij}})",
    r"(\mathrm{df}=(r-1)(c-1),\qquad r=\text{rows},\ c=\text{columns})"
  )
)
