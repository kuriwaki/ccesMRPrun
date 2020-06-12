Running MRP with CCES
================

<!-- badges: start -->

[![R build
status](https://github.com/kuriwaki/ccesMRPrun/workflows/R-CMD-check/badge.svg)](https://github.com/kuriwaki/ccesMRPrun/actions)
<!-- badges: end -->

``` r
remotes::install_github("kuriwaki/ccesMRPrun")
```

``` r
library(ccesMRPrun)
library(tidyverse)
```

To prepare, see \<www.shirokuriwaki.com/ccesMRPprep\>.

# Fitting

This is a simple wrapper around `brms::brm` but with some custom priors
and a binomial model as a default.

``` r
cces_georgia <- read_rds("data-analysis/cc-count_GA.rds")

# don't run in readme, takes too long
fit <- fit_brms("yes | trials(n_response) ~ (1 | age) + (1 | edu) + (1 | cd)",
                cces_georgia)
```

# Poststratification

``` r
# sample data (too large for package; not in github yet)
fit <- read_rds("data-analysis/fit_GA.rds")
summary(fit)
```

    ##  Family: binomial 
    ##   Links: mu = logit 
    ## Formula: yes | trials(n_response) ~ (1 | age:gender) + (1 | educ) + dempres_vshare + (1 | cd) 
    ##    Data: count_df (Number of observations: 1572) 
    ## Samples: 4 chains, each with iter = 500; warmup = 250; thin = 1;
    ##          total post-warmup samples = 1000
    ## 
    ## Group-Level Effects: 
    ## ~age:gender (Number of levels: 141) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.81      0.08     0.65     0.99 1.01      420      824
    ## 
    ## ~cd (Number of levels: 14) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.13      0.08     0.01     0.31 1.01      310      487
    ## 
    ## ~educ (Number of levels: 4) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.66      0.29     0.28     1.39 1.00      563      735
    ## 
    ## Population-Level Effects: 
    ##                Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept          0.47      0.36    -0.26     1.12 1.00      433      523
    ## dempres_vshare    -0.20      0.33    -0.82     0.50 1.00      862      506
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

There are two main methods in this package: modeling and

``` r
cc_count <- read_rds("data-analysis/cc-count_GA.rds")
poststrat <- read_rds("data-analysis/by-cd-cell_poststrat_2018.rds")

drw <- poststrat_draws(fit, poststrat, cc_count)
```

    ## Warning: The `x` argument of `as_tibble.matrix()` must have column names if `.name_repair` is omitted as of tibble 2.0.0.
    ## Using compatibility `.name_repair`.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_warnings()` to see where this warning was generated.

``` r
drw
```

    ## # A tibble: 14,000 x 4
    ##    qID     cd     iter p_mrp
    ##    <chr>   <chr> <dbl> <dbl>
    ##  1 turnout GA-01     1 0.426
    ##  2 turnout GA-01     2 0.880
    ##  3 turnout GA-01     3 0.469
    ##  4 turnout GA-01     4 0.803
    ##  5 turnout GA-01     5 0.542
    ##  6 turnout GA-01     6 0.556
    ##  7 turnout GA-01     7 0.435
    ##  8 turnout GA-01     8 0.449
    ##  9 turnout GA-01     9 0.693
    ## 10 turnout GA-01    10 0.768
    ## # … with 13,990 more rows

# summaries

We often care about the posterior mean and 95 percent credible intervals
of the draws.

``` r
summ_sims(drw)
```

    ## # A tibble: 14 x 10
    ## # Groups:   qID [1]
    ##    qID   cd    p_mrp_est p_mrp_025 p_mrp_050 p_mrp_100 p_mrp_500 p_mrp_900
    ##    <chr> <chr>     <dbl>     <dbl>     <dbl>     <dbl>     <dbl>     <dbl>
    ##  1 turn… GA-01     0.539     0.213     0.258     0.307     0.550     0.766
    ##  2 turn… GA-02     0.525     0.191     0.239     0.293     0.528     0.749
    ##  3 turn… GA-03     0.564     0.228     0.269     0.330     0.569     0.787
    ##  4 turn… GA-04     0.531     0.201     0.244     0.300     0.537     0.760
    ##  5 turn… GA-05     0.560     0.224     0.282     0.325     0.568     0.775
    ##  6 turn… GA-06     0.601     0.254     0.306     0.375     0.615     0.812
    ##  7 turn… GA-07     0.549     0.218     0.266     0.315     0.554     0.779
    ##  8 turn… GA-08     0.528     0.201     0.244     0.296     0.534     0.753
    ##  9 turn… GA-09     0.546     0.212     0.261     0.313     0.552     0.769
    ## 10 turn… GA-10     0.545     0.215     0.264     0.312     0.553     0.767
    ## 11 turn… GA-11     0.571     0.226     0.282     0.340     0.579     0.787
    ## 12 turn… GA-12     0.500     0.175     0.213     0.268     0.502     0.734
    ## 13 turn… GA-13     0.505     0.192     0.222     0.277     0.504     0.737
    ## 14 turn… GA-14     0.515     0.190     0.232     0.283     0.513     0.739
    ## # … with 2 more variables: p_mrp_950 <dbl>, p_mrp_975 <dbl>
