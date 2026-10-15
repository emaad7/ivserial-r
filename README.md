# ivserial (R) — exact symbolic tests of serial independence

`ivserial` tests whether a time series is a sequence of independent and identically
distributed (i.i.d.) values, with the procedure of

> Ashtari Nezhad, E. (2026). *Information Variance and Exact Symbolic Tests of Serial Independence.*

The test is **rank based and exact**: under independence its P-value is valid for every
sample size and every continuous distribution, with no model, bandwidth or tuning to choose.
Besides the P-value it reports **how strong** the dependence is (a coefficient between 0 and 1)
and **which patterns** of consecutive values cause it, and it plans **sample sizes**.

The package can be used through a **window** (no programming, `ivserial_app()`) or through
**R code**; both give the same results. A Python version with the same functions is on PyPI:
`pip install ivserial` ([github.com/emaad7/ivserial](https://github.com/emaad7/ivserial)).

## Installation

```r
install.packages("remotes")                       # once
remotes::install_github("emaad7/ivserial-r")
install.packages(c("shiny", "readxl"))            # for the window and for Excel files
```

## Quick start

```r
library(ivserial)
ivserial_app()                                   # the window

x   <- 100 * diff(log(exchange_rates$China))     # log returns of the yuan
res <- serial_test(x, seed = 1)                  # B = 9999 permutations
res                                              # report
res$p_C; res$cramer_v; head(res$cells)           # P-value, strength (0-1), patterns

screen_series(exchange_rates[-1], transform = "logdiff", seed = 1)   # 20 series, BH-adjusted
required_sample_size(0.13, m = 3, d = 4, power = 0.8, tau = 1.13)    # planning
null_limit_summary(3, 4)                                             # null distribution
```

```
Exact symbolic test of serial independence  [China]
===================================================
Observations n = 939
Block statistic: m = 3 consecutive values, d = 5 quantile classes (125 patterns)
Portmanteau: lags 1..2, d = 5 classes
Random permutations B = 9999

Test                                 Statistic     P-value
C = min(p_Q, p_P2)  [proposed]                      0.0001
Q  (blocks, permutation)               380.369      0.0001
Q  (blocks, asymptotic)                           < 0.0001
P2 (lags, permutation)                 170.349      0.0001
P2 (lags, asymptotic)                             < 0.0001

Serial independence is REJECTED at the 0.05 level (combined test C).
Strength of dependence: V-hat = 0.109
...
pattern         observed  expected  residual
3-3-3                 41       7.5     12.24
1-5-5                 20       7.5      4.57
```

## User guide

`ivserial_guide()` or `vignette("ivserial")` after installation (also the *User guide* tab of the window), or
[`vignettes/ivserial.Rmd`](vignettes/ivserial.Rmd).

## Without installing from GitHub

Download `ivserial_1.0.1.tar.gz` from the *Releases* page and run
`install.packages("ivserial_1.0.1.tar.gz", repos = NULL, type = "source")`.

## Citation

`citation("ivserial")`. Bug reports: <https://github.com/emaad7/ivserial-r/issues>.

## License

MIT (see `LICENSE.md`).
