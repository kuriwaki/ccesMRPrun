Running MRP with CCES
================

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
    ##  1 turnout GA-01     1 0.335
    ##  2 turnout GA-01     2 0.597
    ##  3 turnout GA-01     3 0.711
    ##  4 turnout GA-01     4 0.683
    ##  5 turnout GA-01     5 0.643
    ##  6 turnout GA-01     6 0.679
    ##  7 turnout GA-01     7 0.607
    ##  8 turnout GA-01     8 0.662
    ##  9 turnout GA-01     9 0.534
    ## 10 turnout GA-01    10 0.716
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
    ##  1 turn… GA-01     0.543     0.205     0.254     0.304     0.554     0.762
    ##  2 turn… GA-02     0.528     0.196     0.235     0.291     0.534     0.751
    ##  3 turn… GA-03     0.567     0.227     0.272     0.332     0.574     0.784
    ##  4 turn… GA-04     0.534     0.198     0.245     0.301     0.536     0.751
    ##  5 turn… GA-05     0.564     0.225     0.272     0.329     0.573     0.777
    ##  6 turn… GA-06     0.605     0.269     0.303     0.371     0.617     0.808
    ##  7 turn… GA-07     0.552     0.206     0.254     0.320     0.566     0.765
    ##  8 turn… GA-08     0.532     0.193     0.239     0.288     0.540     0.751
    ##  9 turn… GA-09     0.549     0.206     0.255     0.314     0.559     0.767
    ## 10 turn… GA-10     0.548     0.211     0.252     0.311     0.558     0.760
    ## 11 turn… GA-11     0.574     0.225     0.279     0.340     0.585     0.788
    ## 12 turn… GA-12     0.503     0.175     0.216     0.270     0.508     0.730
    ## 13 turn… GA-13     0.509     0.179     0.217     0.276     0.511     0.735
    ## 14 turn… GA-14     0.519     0.178     0.223     0.282     0.523     0.732
    ## # … with 2 more variables: p_mrp_950 <dbl>, p_mrp_975 <dbl>
