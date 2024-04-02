Advanced Macroeconomics Homework 6
================
2024-04-03

## Question 1

### 1(a) Plots

``` r
data <- read.csv("/Users/aryaman/Downloads/cons_homework_csv.csv")
data$date <- as.yearqtr(data$date)

ggplot(data, aes(x = date)) +
  geom_line(aes(y = log(NDS), color = "NDS")) +
  geom_line(aes(y = log(GDP), color = "GDP")) +
  labs(title = "Logs of NDS and GDP over Time",
       x = "Date",
       y = "Log Value",
       color = "Variable") +
  scale_color_manual(values = c("blue", "red")) +
  theme_minimal()
```

![](AdvMacroHW6_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
ggplot(data, aes(x = date)) +
  geom_line(aes(y = SP500)) +
  labs(title = "Real SP500 Index over Time",
       x = "Date",
       y = "Real SP500 Index") +
  theme_minimal()
```

![](AdvMacroHW6_files/figure-gfm/unnamed-chunk-1-2.png)<!-- -->

``` r
# Creating restricted data for centered plots 
data_2008 <- data[data$year >= "2007" & data$year <= "2009", ]
data_2008$GDP <- data_2008$GDP / data$GDP[data$date == "2007q1"]
data_2008$NDS <- data_2008$NDS / data$NDS[data$date == "2007q1"]
data_2008$SP500 <- data_2008$SP500 / data$SP500[data$date == "2007q1"]

data_2020 <- data[data$year >= "2019" & data$year <= "2020", ]
data_2020$GDP <- data_2020$GDP / data$GDP[data$date == "2019q1"]
data_2020$NDS <- data_2020$NDS / data$NDS[data$date == "2019q1"]
data_2020$SP500 <- data_2020$SP500 / data$SP500[data$date == "2019q1"]

ggplot(data_2008, aes(x = date)) +
  geom_line(aes(y = log(NDS), color = "NDS")) +
  geom_line(aes(y = log(GDP), color = "GDP")) +
  geom_line(aes(y = log(SP500), color = "SP500")) +
  labs(title = "Logs of NDS and GDP over Time (Centered on 2008Q2)",
       x = "Date",
       y = "Log Value",
       color = "Variable") +
  scale_color_manual(values = c("blue", "red", "black")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_y_continuous(
    sec.axis = sec_axis(~ ., name = "SP500", breaks = scales::pretty_breaks())
  )
```

![](AdvMacroHW6_files/figure-gfm/unnamed-chunk-1-3.png)<!-- -->

``` r
ggplot(data_2020, aes(x = date)) +
  geom_line(aes(y = log(NDS), color = "NDS")) +
  geom_line(aes(y = log(GDP), color = "GDP")) +
  geom_line(aes(y = log(SP500), color = "SP500")) +
  labs(title = "Logs of NDS and GDP over Time (Centered on 2020Q1)",
       x = "Date",
       y = "Log Value",
       color = "Variable") +
  scale_color_manual(values = c("blue", "red", "black")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_y_continuous(
    sec.axis = sec_axis(~ ., name = "SP500", breaks = scales::pretty_breaks())
 )
```

![](AdvMacroHW6_files/figure-gfm/unnamed-chunk-1-4.png)<!-- -->

During the 2008 recession, the stock market fell drastically,
experiencing a log drop of more than 0.6 from the beginning of 2007 to
the beginning of 2009. Consumption and income dipped as well, but far
less than the stock market. Relative to 2007q1, consumption dropped
slightly more than income during the recession. Log income dropped by
around 0.03 while consumption dropped by around 0.05 across this period.

During the pandemic, we see that the stock market experienced the shock
before income and consumption, dropping by about 0.07 in logs relative
to the beginning of 2019. GDP and consumption dropped less steeply than
the stock market. Income dropped lower and steeper than consumption
during this period. Consumption dropped the least out of the three
variables in logs during the pandemic.

### 1(b) Growth Rates

``` r
data$delta_log_GDP <- c(NA, diff(log(data$GDP)))
data$delta_log_NDS <- c(NA, diff(log(data$NDS)))
data$delta_log_SP500 <- c(NA, diff(log(data$SP500)))

# I will now restrict the data to observations until 2019Q4 
data_res <- data[data$date <= "2019q4", ]



growth_rates <- data.frame(date = data_res$date,
                       delta_log_GDP = data_res$delta_log_GDP,
                       delta_log_NDS = data_res$delta_log_NDS,
                       delta_log_SP500 = data_res$delta_log_SP500)

growth_rates <- growth_rates[-1,]

ggplot(growth_rates, aes(x = date)) +
  geom_line(aes(y = delta_log_GDP, color = "GDP")) +
  geom_line(aes(y = delta_log_NDS, color = "NDS")) +
  geom_line(aes(y = delta_log_SP500, color = "SP500")) +
  labs(title = "Growth Rates of GDP, NDS, and SP500",
       x = "Date",
       y = "Growth Rate",
       color = "Variable") +
  scale_color_manual(values = c("blue", "red", "black")) +
  theme_minimal()
```

![](AdvMacroHW6_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
ggplot(growth_rates, aes(x = date)) +
  geom_line(aes(y = delta_log_GDP, color = "GDP")) +
  geom_line(aes(y = delta_log_NDS, color = "NDS")) +
  labs(title = "Growth Rates of GDP and NDS",
       x = "Date",
       y = "Growth Rate",
       color = "Variable") +
  scale_color_manual(values = c("blue", "red")) +
  theme_minimal()
```

![](AdvMacroHW6_files/figure-gfm/unnamed-chunk-2-2.png)<!-- -->

I have provided one plot with the growth rates of all three variables.
The growth rate of the stock market is clearly far more volatile than
that of consumption and income, so I plotted just income and consumption
growth rates in a separate plot to observe them on a more appropriate
scale.

### 1(c) Summary Statistics for Growth Rates

``` r
stargazer(growth_rates, type='latex', 
          title = "Summary Statistics for Growth Rates")
```

% Table created by stargazer v.5.2.3 by Marek Hlavac, Social Policy
Institute. E-mail: marek.hlavac at gmail.com % Date and time: Mon, Apr
01, 2024 - 23:30:19
Income and consumption growth both have a similar volatility as measured
by standard deviation. Income growth is slightly more volatile than
consumption growth with a standard deviation of 0.08 compared to 0.07
for consumption growth. Stock market growth is far more volatile than
the growth of both income and consumption with a standard deviation
approximately ten times greater than that of income and consumption
growth.

### 1(d) Growth Rates Correlation Matrix

``` r
correlation_matrix <- cor(growth_rates[, c("delta_log_GDP", 
                                           "delta_log_NDS", "delta_log_SP500")])

kable(correlation_matrix, 
      caption = "Correlation matrix between growth rates",
      format = "latex",
      booktabs = TRUE,
      digits = 2)
```

We observe a moderate correlation ($\rho = 0.58$) between consumption
and income growth, and low correlation between the stock market and both
consumption and income growth. However, stock market growth is slightly
more correlated with consumption growth ($\rho = 0.24$) than with income
growth ($\rho = 0.15$).

### 1(e)

``` r
acf_gdp <- acf(diff(data_res$GDP), lag.max = 4, plot = FALSE)
acf_nds <- acf(diff(data_res$NDS), lag.max = 4, plot = FALSE)
acf_sp500 <- acf(diff(data_res$SP500), lag.max = 4, plot = FALSE)

autocorrelations <- data.frame(
  Lag = 0:4,
  GDP = acf_gdp$acf,
  NDS = acf_nds$acf,
  SP500 = acf_sp500$acf
)

kable(autocorrelations, 
      caption = "Autocorrelations of Variables of Interest, 
      Lagged from 0-4 Periods",
      format = "latex",
      position = "h",
      booktabs = TRUE,
      digits = 3)
```

### 1(f)

``` r
ggplot(data_res, aes(x = delta_log_GDP, y = delta_log_NDS)) +
  geom_point() +
  labs(title = "Scatter Plot of Real Consumption Growth vs Real Income (GDP) Growth",
       x = "Real Income (GDP) Growth",
       y = "Real Consumption Growth")
```

    ## Warning: Removed 1 rows containing missing values (`geom_point()`).

![](AdvMacroHW6_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
ggplot(data_res, aes(x = delta_log_SP500, y = delta_log_NDS)) +
  geom_point() +
  labs(title = "Scatter Plot of Real Consumption Growth vs Real Stock Market Growth",
       x = "Real Stock Market Growth",
       y = "Real Consumption Growth")
```

    ## Warning: Removed 1 rows containing missing values (`geom_point()`).

![](AdvMacroHW6_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->

## Question 2

We seek to test whether consumption truly follows a random walk by
regressing the growth of consumption at time $t$ on values of
consumption, income, and stock market growth known at time $t-1$. We
denote the set of these variables as $X_{t-1}$. We will first regress
consumption growth against one period lagged values of consumption,
income, and stock market growth, i.e.  \[
\begin{aligned}
\Delta \ln{(NDS_t)} &= \alpha + \beta\left[\Delta \ln{(NDS_{t-1})}\right] + \epsilon_t && \text{(1)} \\
\Delta \ln{(NDS_t)} &= \alpha + \beta\left[\Delta \ln{(GDP_{t-1})}\right] + \epsilon_t && \text{(2)} \\
\Delta \ln{(NDS_t)} &= \alpha + \beta\left[\Delta \ln{(SP_{t-1})}\right] + \epsilon_t && \text{(3)} \\
\end{aligned}
\] $$
\begin{aligned}
\Delta \ln{(NDS_t)} &= \alpha + \beta_1\left[\Delta \ln{(NDS_{t-1})}\right] +
\beta_2\left[\Delta \ln{(GDP_{t-1})}\right] + 
\beta_3\left[\Delta \ln{(SP_{t-1})}\right] + \epsilon_t && \text{(4)}
\end{aligned}
$$

``` r
model_NDSlag1 <- lm(delta_log_NDS ~ lag(delta_log_NDS, 1), data = data_res)
model_GDPlag1 <- lm(delta_log_NDS ~ lag(delta_log_GDP, 1), data = data_res)
model_SPlag1 <- lm(delta_log_NDS ~ lag(delta_log_SP500, 1), data = data_res)
model_combinedlag1 <- lm(delta_log_NDS ~ lag(delta_log_NDS, 1)
                     + lag(delta_log_GDP, 1)
                     + lag(delta_log_SP500, 1), data = data_res)

stargazer(model_NDSlag1, model_GDPlag1, model_SPlag1, 
          title="Consumption Growth vs. One-Period Lagged 
          Variables (Separate)",
          header=FALSE)
```

``` r
stargazer(model_combinedlag1, 
          title="Consumption Growth vs. One-Period Lagged 
          Variables (Combined)",
          column.labels="(4)",
          header=FALSE)
```

We observe that when regressing consumption growth against one period
lagged values of consumption, income, and stock market growth
separately, the coefficients are all statistically significant at the 1%
level, suggesting that $b \neq 0$. When we combine all three, one-period
lagged consumption is no longer statistically significant, but both
income and stock market growth remain statistically significant at the
1% level. Let the vector
$L_{t-i} = \{\Delta \ln{(NDS_{t-i})},\:\Delta \ln{(GDP_{t-i})},\:\Delta \ln{(SP_{t-i})}\}$
and let $\beta_{L_{t-i}}$ denote the vector of corresponding OLS
coefficients. We will now add two-period lagged variables into these
regressions by estimating: $$
\begin{aligned}
\Delta \ln{(NDS_t)} &= \alpha + \beta_{L_{t-1}}\left[L_{t-1}\right] + \beta_4\left[\Delta \ln{(NDS_{t-2})}\right] + \epsilon_t && \text{(5)} \\
\Delta \ln{(NDS_t)} &= \alpha + \beta_{L_{t-1}}\left[L_{t-1}\right] + \beta_4\left[\Delta \ln{(GDP_{t-2})}\right] + \epsilon_t && \text{(6)} \\
\Delta \ln{(NDS_t)} &= \alpha + \beta_{L_{t-1}}\left[L_{t-1}\right] + \beta_4\left[\Delta \ln{(SP_{t-2})}\right] + \epsilon_t && \text{(7)}
\end{aligned}
$$

``` r
model_combinedNDS2 <- lm(delta_log_NDS ~ lag(delta_log_NDS, 1) 
                     + lag(delta_log_GDP, 1)
                     + lag(delta_log_SP500, 1)
                     + lag(delta_log_NDS, 2), 
                     data = data_res)
model_combinedGDP2 <- lm(delta_log_NDS ~ lag(delta_log_NDS, 1) 
                     + lag(delta_log_GDP, 1)
                     + lag(delta_log_SP500, 1)
                     + lag(delta_log_GDP, 2), 
                     data = data_res)
model_combinedSP2 <- lm(delta_log_NDS ~ lag(delta_log_NDS, 1) 
                     + lag(delta_log_GDP, 1)
                     + lag(delta_log_SP500, 1)
                     + lag(delta_log_SP500, 2), 
                     data = data_res)
stargazer(model_combinedNDS2, model_combinedGDP2, model_combinedSP2, 
          title = "Consumption Growth vs. One-Period Lagged Variables with Add. Lags (Separate)",
          column.labels = c("(5)", "(6)", "(7)"),
          model.numbers = FALSE,
          header=FALSE)
```

From these regressions, we see that consumption growth lagged by one
period is not a good predictor for future consumption growth, while
income growth, lagged by both one and two periods, and stock market
growth, lagged by one period, are statistically significant predictors.
We can thus run another regression including further lags of income
growth alongside stock market growth lagged by one period. We will thus
estimate $$
\begin{aligned}
\Delta \ln{(NDS_t)} &= \alpha + \sum^4_{i=1}\beta_i\left[\Delta \ln{(GDP_{t-i})}\right] + \beta_5\left[\Delta \ln{(SP_{t-1})}\right] + \epsilon_t && \text{(8)} \\
\end{aligned}
$$ After running this regression, we find that income growth lagged more
than two periods loses predictive power. We will omit these variables as
we should only include statistically significant covariates when
attempting to achieve a high $R^2$ value to avoid overfitting biases. We
thus run a final regression which includes only income growth lagged by
one and two periods and stock market growth lagged by one period. $$
\begin{aligned}
\Delta \ln{(NDS_t)} &= \alpha + \sum^2_{i=1}\beta_i\left[\Delta \ln{(GDP_{t-i})}\right] + \beta_3\left[\Delta \ln{(SP_{t-1})}\right] + \epsilon_t && \text{(9)} \\
\end{aligned}
$$ Table 8 summarizes these results.

``` r
model_combinedlags <- lm(delta_log_NDS ~ lag(delta_log_GDP, 1)
                     + lag(delta_log_GDP, 2)
                     + lag(delta_log_GDP, 3)
                     + lag(delta_log_GDP, 4)
                     + lag(delta_log_SP500, 1), data = data_res)
model_combinedlags12 <- lm(delta_log_NDS ~ lag(delta_log_GDP, 1)
                     + lag(delta_log_GDP, 2)
                     + lag(delta_log_SP500, 1), data = data_res)
stargazer(model_combinedlags, model_combinedlags12,
          title="Consumption Growth vs. 
          Chosen Lagged Variables (Combined)",
          column.labels=c("(8)", "(9)"),
          model.numbers=FALSE,
          header=FALSE)
```

Since all variables in regression (9) are significant at the 1% level
and the adjusted $R^2$ value drops very slightly after omitting the
insignificant covariates from regression (8), we conclude that
approximately 18.2% of the variation in consumption growth can be
determined by income growth lagged by one and two periods, and stock
market growth lagged by one period. Lagged income has a more acute
effect on consumption growth than lagged stock market growth does,
suggested by the relative magnitudes of the calculated coefficients.

Thus, we can conclude that consumption does not fully follow a true
random walk. Rather, approximately a fifth of the variation in
consumption growth can be explained by previous income and stock market
growth.

## Question 3

### 3(a) Forecasting Future Income Growth

We seek to find a vector of variables known at time $t-1$, $X_{t-1}$,
which predict income growth at time $t$. I will first run three
regressions of income growth on lagged values of the three variables of
interest. We will estimate: $$
\begin{aligned}
\Delta \ln{(GDP_t)} &= \alpha + \sum^4_{i=1}\beta_i\left[\Delta \ln{(GDP_{t-i})}\right] + \epsilon_t && \text{(10)} \\
\Delta \ln{(GDP_t)} &= \alpha + \sum^4_{i=1}\beta_i\left[\Delta \ln{(NDS_{t-i})}\right] + \epsilon_t && \text{(11)} \\
\Delta \ln{(GDP_t)} &= \alpha + \sum^4_{i=1}\beta_i\left[\Delta \ln{(SP_{t-i})}\right] + \epsilon_t && \text{(12)} 
\end{aligned}
$$

``` r
incgrowth_GDPlag <- lm(delta_log_GDP ~ lag(delta_log_GDP, 1)
                       + lag(delta_log_GDP, 2)
                       + lag(delta_log_GDP, 3)
                       + lag(delta_log_GDP, 4), data = data_res)

incgrowth_NDSlag <- lm(delta_log_GDP ~ lag(delta_log_NDS, 1)
                       + lag(delta_log_NDS, 2)
                       + lag(delta_log_NDS, 3)
                       + lag(delta_log_NDS, 4), data = data_res)

incgrowth_SP500lag <- lm(delta_log_GDP ~ lag(delta_log_SP500, 1)
                       + lag(delta_log_SP500, 2)
                       + lag(delta_log_SP500, 3)
                       + lag(delta_log_SP500, 4), data = data_res)

stargazer(incgrowth_GDPlag, incgrowth_NDSlag, incgrowth_SP500lag, 
          title="Income Growth vs. Lagged Variables (Separate)",
          column.labels=c("(10)", "(11)", "(12)"),
          model.numbers=FALSE,
          header=FALSE)
```

The six covariates statistically significant at the 1% significance
level in predicting future income growth are one and two period lags of
all three of the variables of interest. I will first regress future
income growth on each of the three variables lagged by one period. I
will then run three further regressions of income growth on $L_{t-1}$
and two-period lagged values of each of the three variables
separately.  
$$
\begin{aligned}
\Delta \ln{(GDP_t)} &= \alpha + \beta_{L_{t-1}}\left[L_{t-1}\right] + \epsilon_t && \text{(13)} \\
\Delta \ln{(GDP_t)} &= \alpha + \beta_{L_{t-1}}\left[L_{t-1}\right] + \beta_4\left[\Delta \ln{(GDP_{t-2})}\right] + \epsilon_t && \text{(14)} \\
\Delta \ln{(GDP_t)} &= \alpha + \beta_{L_{t-1}}\left[L_{t-1}\right] + \beta_4\left[\Delta \ln{(NDS_{t-2})}\right] + \epsilon_t && \text{(15)} \\
\Delta \ln{(GDP_t)} &= \alpha + \beta_{L_{t-1}}\left[L_{t-1}\right] + \beta_4\left[\Delta \ln{(SP_{t-2})}\right] + \epsilon_t && \text{(16)}
\end{aligned}
$$

``` r
inc_growth_base <- lm(delta_log_GDP ~ lag(delta_log_GDP, 1)
                     + lag(delta_log_NDS, 1)
                     + lag(delta_log_SP500, 1),
                     data = data_res)

stargazer(inc_growth_base, title="Income Growth vs. 
          One-Period Lagged Variables",
          column.labels="(13)",
          header=FALSE)
```

``` r
inc_growth_GDP2 <- lm(delta_log_GDP ~ lag(delta_log_GDP, 1)
                     + lag(delta_log_NDS, 1)
                     + lag(delta_log_SP500, 1)
                     + lag(delta_log_GDP, 2), 
                     data = data_res)
inc_growth_NDS2 <- lm(delta_log_GDP ~ lag(delta_log_GDP, 1)
                     + lag(delta_log_NDS, 1)
                     + lag(delta_log_SP500, 1)
                     + lag(delta_log_NDS, 2), 
                     data = data_res)
inc_growth_SP5002 <- lm(delta_log_GDP ~ lag(delta_log_GDP, 1)
                     + lag(delta_log_NDS, 1)
                     + lag(delta_log_SP500, 1)
                     + lag(delta_log_SP500, 2), 
                     data = data_res)

stargazer(inc_growth_GDP2, 
          inc_growth_NDS2, inc_growth_SP5002,
          title="Income Growth vs. One and Two-Period Lagged Variables",
          column.labels=c("(14)", "(15)", "(16)"),
          model.numbers=FALSE,
          header=FALSE)
```

We choose to include both $\Delta \ln{(NDS_{t-2})}$ and
$\Delta \ln{(SP_{t-2})}$ as they are both statistically significant at
the $1%$ level and increase the adjusted $R^2$ values of the regressions
from the base regression on only one-period lags (13) (adjusted
$R^2 = 0.152$) the most (adj. $R^2 = 0.195$ and adj. $R^2 = 0.184$ in
regressions (15) and (16), respectively). We thus run a regression of
future income growth on the five covariates as follows: $$
\begin{aligned}
\Delta \ln{(GDP_t)} &= \alpha + \beta_{L_{t-1}}\left[L_{t-1}\right] + \beta_4\left[\Delta \ln{(NDS_{t-2})}\right] + \beta_5\left[\Delta \ln{(SP_{t-2})}\right] + \epsilon_t && \text{(17)} \end{aligned}
$$ We observe that while the adjusted $R^2$ value of this regression is
an improvement over the base, the coefficient on
$\Delta \ln{(GDP_{t-1})}$ is no longer statistically significant. We
thus run another regression omitting this variable. $$
\begin{aligned}
\Delta \ln{(GDP_t)} &= \alpha + \beta_1\left[\Delta \ln{(NDS_{t-1})}\right] + \beta_2\left[\Delta \ln{(NDS_{t-1})}\right] + \beta_3\left[\Delta \ln{(SP_{t-1})}\right] + \beta_4\left[\Delta \ln{(SP_{t-2})}\right] + \epsilon_t && \text{(18)}
\end{aligned}
$$ The results of these two regressions are summarized in Table 12.

``` r
inc_growth_NDS2_SP5002 <- lm(delta_log_GDP ~ lag(delta_log_GDP, 1)
                          + lag(delta_log_NDS, 1)
                          + lag(delta_log_SP500, 1)
                          + lag(delta_log_NDS, 2)
                          + lag(delta_log_SP500, 2), 
                          data = data_res)

inc_growth_model <- lm(delta_log_GDP ~ lag(delta_log_NDS, 1)
                          + lag(delta_log_SP500, 1)
                          + lag(delta_log_NDS, 2)
                          + lag(delta_log_SP500, 2), 
                          data = data_res)

stargazer(inc_growth_NDS2_SP5002, inc_growth_model,
          title="Income Growth vs. Chosen Lagged Variables",
          model.numbers=FALSE,
          column.labels=c("(17)","(18)"),
          header=FALSE)
```

By omitting this variable, the regression now has an adjusted $R^2$
value of $0.209$, a very small drop from the adjusted $R^2$ value from
the previous regression. Further, all of the coefficients are now
statistically significant at the 1% level, including the constant. We
thus conclude that future income growth is likely best estimated by
regression (18). Our determined vector of four covariates is then
$$X_{t-1} = \{\ln{(NDS_{t-1})}, \Delta \ln{(SP_{t-1})}, \Delta \ln{(NDS_{t-2})}, \Delta \ln{(SP_{t-2})}\}$$

### 3(b)

We will estimate two models: $$
\begin{aligned}
\Delta \ln{(NDS_t)} &= a_c + b'_cX_{t-1} + u_t \\
\Delta \ln{(GDP_t)} &= a_y + b'_yX_{t-1} + v_t
\end{aligned}
$$ The following plot shows the values of income growth against values
of consumption growth fitted by the above regressions.

``` r
NDSt_model <- lm(delta_log_NDS ~ lag(delta_log_NDS, 1)
                          + lag(delta_log_SP500, 1)
                          + lag(delta_log_NDS, 2)
                          + lag(delta_log_SP500, 2), 
                          data = data_res)

GDPt_model <- lm(delta_log_GDP ~ lag(delta_log_NDS, 1)
                          + lag(delta_log_SP500, 1)
                          + lag(delta_log_NDS, 2)
                          + lag(delta_log_SP500, 2), 
                          data = data_res)
fitted_NDSt <- predict(NDSt_model)
fitted_GDPt <- predict(GDPt_model)

plot(fitted_GDPt, fitted_NDSt, xlab = "Fitted Values of Income Growth", 
     ylab = "Fitted Values of Consumption Growth",
     main = "Fitted Income Growth vs. Fitted Consumption Growth", 
     col = "blue")
```

![](AdvMacroHW6_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
plot(delta_log_GDP ~ delta_log_NDS,
       data = data_res,
       cex = 0.6,
       xlab = "Income Growth", 
       ylab = "Consumption Growth",
       main = " Income Growth vs. Consumption Growth with Fitted Values")
points(fitted_GDPt, fitted_NDSt, col="blue")
```

![](AdvMacroHW6_files/figure-gfm/unnamed-chunk-13-2.png)<!-- -->

I have plotted the fitted values alone as well as the fitted values
alongside the original values for comparison. We can see that the fitted
values have a far stronger linear relationship with a much tighter
distribution. If the Campbell-Mankiw model holds, then we should see
that the coefficients of the above two regression equations should
satisfy $$\frac{b_c}{b_y} = \lambda \approx 0.5$$ which implies that
$b_c$ should be twice as large in magnitude as the coefficient $b_y$
according to Campbell and Mankiw’s approximation of
$\lambda \approx 0.5$.

### 3(c)

We now run an IV regression of consumption growth on income growth with
$X_{t-1}$ as instrumental variables.

``` r
iv_model <- ivreg(delta_log_NDS ~ delta_log_GDP | lag(delta_log_NDS, 1)
                                + lag(delta_log_SP500, 1)
                                + lag(delta_log_NDS, 2)
                                + lag(delta_log_SP500, 2), 
                                data = data_res)

stargazer(iv_model)
```

% Table created by stargazer v.5.2.3 by Marek Hlavac, Social Policy
Institute. E-mail: marek.hlavac at gmail.com % Date and time: Mon, Apr
01, 2024 - 23:30:20
The observed coefficient corresponds to an estimate of $\lambda = 0.573$
significant at the 1% level. This provides strong evidence that
consumption does not follow a random walk. The estimate is slightly
larger than that of Campbell and Mankiw. The value implies that over
half of consumption growth is determined by income growth, and the
remaining proportion, less than half, follows a random-walk.

## Question 4

The pandemic had a profound effect on the economy and likely made
consumers more conservative with their spending on nondurables, tying
their consumption with income more than any surprise news in that
period. This would, theoretically, increase $\lambda$ as consumption
would be more related to income than follow a random walk. We construct
an indicator variable $PAN_t$ to model and control for the effect of the
pandemic.

``` r
data$pandemic <- ifelse(data$year >= 2020, 1, 0)
```

To control for the pandemic, we should first consider how the lagged
values of the pandemic indicator variable $PAN_t$ influence the three
variables of interest, if at all. Table 14 summarizes the results of the
following regressions:

$$
\begin{aligned}
\Delta \ln{(GDP_t)} &= \alpha + \sum^4_{i=1}\beta_i\left[\Delta \ln{(PAN_{t-i})}\right] + \epsilon_t && \text{(20)} \\
\Delta \ln{(NDS_t)} &= \alpha + \sum^4_{i=1}\beta_i\left[\Delta \ln{(PAN_{t-i})}\right] + \epsilon_t && \text{(21)} \\
\Delta \ln{(SP_t)} &= \alpha + \sum^4_{i=1}\beta_i\left[\Delta \ln{(PAN_{t-i})}\right] + \epsilon_t && \text{(22)} 
\end{aligned}
$$

``` r
incgrowth_pandemic_GDP <- lm(delta_log_GDP ~ lag(pandemic, 1)
                             + lag(pandemic, 2)
                             + lag(pandemic, 3)
                             + lag(pandemic, 4), 
                             data = data)

incgrowth_pandemic_NDS <- lm(delta_log_NDS ~ lag(pandemic, 1)
                             + lag(pandemic, 2)
                             + lag(pandemic, 3)
                             + lag(pandemic, 4), 
                             data = data)

incgrowth_pandemic_SP500 <- lm(delta_log_SP500 ~ lag(pandemic, 1)
                                + lag(pandemic, 2)
                                + lag(pandemic, 3)
                                + lag(pandemic, 4), 
                                data = data)
stargazer(incgrowth_pandemic_GDP, incgrowth_pandemic_NDS, 
          incgrowth_pandemic_SP500,
          title = "Regression Results with Lagged Pandemic Variables",
          model.numbers=FALSE,
          column.labels=c("(20)", "(21)","(22)"),
          header=FALSE)
```

We observe that the pandemic indicator variables, lagged up to three
periods, is statistically significant in predicting growth in both
consumption and income. We can then run one regression using the same
set of determining variables $X_{t-1}$ including the pandemic data, and
another regression using $X_{t-1}$ as well as the set of pandemic lag
variables $P_{t-1} = \{PAN_{t-1}, PAN_{t-2}, PAN_{t-3}\}$.  
$$
\begin{aligned}
\Delta\ln{(NDS_t)} &= \alpha + \beta X_{t-1} + \epsilon_t && \text{(23)} \\
\Delta\ln{(NDS_t)} &= \alpha + \beta_x X_{t-1} + \beta_pP_{t-1} + \epsilon_t && \text{(24)}
\end{aligned}
$$

``` r
iv_model_without_pandemic <- ivreg(delta_log_NDS ~ delta_log_GDP | lag(delta_log_NDS, 1)
                                  + lag(delta_log_SP500, 1)
                                  + lag(delta_log_NDS, 2)
                                  + lag(delta_log_SP500, 2),
                                  data = data)
iv_model_with_pandemic <- ivreg(delta_log_NDS ~ delta_log_GDP | lag(delta_log_NDS, 1)
                                + lag(delta_log_NDS, 2)
                                + lag(delta_log_SP500, 1)
                                + lag(delta_log_SP500, 2)
                                + lag(pandemic, 1)
                                + lag(pandemic, 2)
                                + lag(pandemic, 3),
                                data = data)
stargazer(iv_model_without_pandemic, iv_model_with_pandemic, 
          type = "latex", 
          title = "IV Regression Results with Pandemic Data Included",
          column.labels = c("uncontrolled (23)", "controlled (24)"),
          model.numbers = FALSE,
          header = FALSE)
```

If we omit the indicator for the pandemic and run the IV regression with
the same set of determining variables $X_{t-1}$ on the full data, the
observed lambda value increases significantly from $\lambda = 0.573$ to
$\lambda = 0.877$. When including the set of pandemic lags $P_{t-1}$
directly into the IV regression as a determining factor of income
growth, the observed lambda value decreases to $0.683$, still larger
than the observed lambda in the restricted data set. This suggests that
when including the pandemic, agents tend to determine their consumption
based more heavily on income as opposed to surprises. When controlling
for the pandemic, however, this effect is less pronounced, but still
existent.

We must check if, when including and controlling for the pandemic, the
set of variables $X_{t-1}$ chosen to predict future income growth are
still applicable. Therefore, we may need to refine $X_{t-1}$ taking into
account the lagged effect of the pandemic. We thus run regressions on
each of the three variables of interest against their own lags as well
as the lagged pandemic values for one, two, and three periods.

``` r
incgrowth_GDPlag_with_pandemic <- lm(delta_log_GDP ~ lag(delta_log_GDP, 1)
                                     + lag(delta_log_GDP, 2)
                                     + lag(delta_log_GDP, 3)
                                     + lag(delta_log_GDP, 4)
                                     + lag(pandemic, 1)
                                     + lag(pandemic, 2)
                                     + lag(pandemic, 3),
                                     data = data)
incgrowth_NDSlag_with_pandemic <- lm(delta_log_GDP ~ lag(delta_log_NDS, 1)
                                     + lag(delta_log_NDS, 2)
                                     + lag(delta_log_NDS, 3)
                                     + lag(delta_log_NDS, 4)
                                     + lag(pandemic, 1)
                                     + lag(pandemic, 2)
                                     + lag(pandemic, 3),
                                     data = data)
incgrowth_SP500lag_with_pandemic <- lm(delta_log_GDP ~ lag(delta_log_SP500, 1)
                                       + lag(delta_log_SP500, 2)
                                       + lag(delta_log_SP500, 3)
                                       + lag(delta_log_SP500, 4)
                                       + lag(pandemic, 1)
                                       + lag(pandemic, 2)
                                       + lag(pandemic, 3),
                                       data = data)
```

Following a similar intuition, we pick the coefficients which are
significant at the 1% level in their respective regressions as it may
indicate predictive power for future income growth. The regressions
indicate that we should move forward with one and two lagged values for
all three variables of interest.

``` r
inc_growth_model_with_pandemic <- lm(delta_log_GDP ~ lag(delta_log_GDP, 1)
                                    + lag(delta_log_GDP, 2) 
                                    + lag(delta_log_NDS, 1)
                                    + lag(delta_log_NDS, 2)
                                    + lag(delta_log_SP500, 1)
                                    + lag(delta_log_SP500, 2)
                                    + lag(pandemic, 1)
                                    + lag(pandemic, 2)
                                    + lag(pandemic, 3),
                                    data = data)
```

However, after controlling for other factors, lagged values of income
growth no longer have statistically significant predictive power. We
thus omit them and run another regression.

``` r
inc_growth_model_with_pandemic <- lm(delta_log_GDP ~ lag(delta_log_NDS, 1)
                                    + lag(delta_log_NDS, 2)
                                    + lag(delta_log_SP500, 1)
                                    + lag(delta_log_SP500, 2)
                                    + lag(pandemic, 1)
                                    + lag(pandemic, 2)
                                    + lag(pandemic, 3),
                                    data = data)
stargazer(inc_growth_model_with_pandemic, header=FALSE,
          title = "Income Growth vs. Lagged Consumption Growth, 
          Stock Market Growth, and Pandemic Variables")
```

Now, all covariates are significant at the 1% level, except stock market
data lagged by two periods which is significant at the 5% level. The
adjusted $R^2$ value of this regression is $0.567$, a remarkable
improvement from the model we ran for forecasting income growth without
the pandemic data included. Our set of covariates when considering the
pandemic $X^p_{t-1}$ are the same as the previous investigation we ran
on the restricted data, with the addition of the three lagged values of
the pandemic variable. $$X^p_{t-1} = X_{t-1} + P_{t-1} = $$
$$\{\Delta\ln{(NDS_{t-1})},\;\Delta\ln{(NDS_{t-2})},\;\Delta\ln{(SP_{t-1})},\;\Delta\ln{(SP_{t-2})},\;PAN_{t-1},\; PAN_{t-2},\; PAN_{t-3} \}$$

Thus, the lambda value calculated in the controlled pandemic IV
regression, Eq. (24), is likely the best estimate we have after
including the pandemic data. It is more robust to include the pandemic
data and control for it than to simply omit it. The lambda increased by
19.2% after including and controlling for the pandemic, which is a
relatively significant increase. I believe that it is necessary to
consider the result after including and controlling for the pandemic as
it gives the most accurate representation of agent consumption behavior
right now considering that the pandemic has recently happened.

The ripple effects of the pandemic are still reflected in the economy
today, and it is thus important to recognize that more consumers are now
hand-to-mouth as opposed to following the permanent income hypothesis,
and this effect may persist for a considerable period. This should,
however, be taken with a grain of salt as consumer consumption behavior
may revert to back to its pre-pandemic norms over time.
