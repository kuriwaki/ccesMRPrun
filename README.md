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

To prepare, see &lt;www.shirokuriwaki.com/ccesMRPprep&gt;.

# Fitting

This is a simple wrapper around `brms::brm` but with some custom priors
and a binomial model as a default.

``` r
ff <- "yes | trials(n_response) ~ (1|age) + (1 + female |educ) + clinton_vote + (1|cd)"

# turn into counts
cc_count <- ccesMRPprep::build_counts(cces_GA, model_ff = ff)

fit <- fit_brms_binomial(ff, cc_count, verbose = FALSE)
```

# Poststratification

There are two main methods in this package: modeling and

``` r
poststrat_GA <- acs_GA
drw <- poststrat_draws(fit, 
                       poststrat_tgt = poststrat_GA,
                       orig_data = cc_count)
```

    ## Warning: The `x` argument of `as_tibble.matrix()` must have unique column names if `.name_repair` is omitted as of tibble 2.0.0.
    ## Using compatibility `.name_repair`.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_warnings()` to see where this warning was generated.

``` r
drw
```

    ## # A tibble: 56,000 x 3
    ##    cd     iter p_mrp
    ##    <chr> <dbl> <dbl>
    ##  1 GA-01     1 0.444
    ##  2 GA-01     2 0.405
    ##  3 GA-01     3 0.415
    ##  4 GA-01     4 0.426
    ##  5 GA-01     5 0.440
    ##  6 GA-01     6 0.486
    ##  7 GA-01     7 0.429
    ##  8 GA-01     8 0.423
    ##  9 GA-01     9 0.513
    ## 10 GA-01    10 0.548
    ## # … with 55,990 more rows

# summaries

We often care about the posterior mean and 95 percent credible intervals
of the draws.

``` r
summ_sims(drw)
```

    ## # A tibble: 14 x 9
    ##    cd    p_mrp_est p_mrp_025 p_mrp_050 p_mrp_100 p_mrp_500 p_mrp_900 p_mrp_950
    ##    <chr>     <dbl>     <dbl>     <dbl>     <dbl>     <dbl>     <dbl>     <dbl>
    ##  1 GA-01     0.443     0.374     0.383     0.395     0.437     0.500     0.520
    ##  2 GA-02     0.502     0.427     0.441     0.455     0.501     0.549     0.565
    ##  3 GA-03     0.354     0.286     0.298     0.312     0.355     0.396     0.409
    ##  4 GA-04     0.662     0.598     0.609     0.620     0.662     0.707     0.720
    ##  5 GA-05     0.708     0.640     0.651     0.666     0.709     0.750     0.763
    ##  6 GA-06     0.509     0.445     0.455     0.464     0.504     0.562     0.580
    ##  7 GA-07     0.450     0.384     0.396     0.411     0.450     0.491     0.505
    ##  8 GA-08     0.339     0.254     0.271     0.288     0.342     0.384     0.395
    ##  9 GA-09     0.262     0.202     0.211     0.222     0.262     0.301     0.313
    ## 10 GA-10     0.384     0.321     0.331     0.343     0.383     0.426     0.440
    ## 11 GA-11     0.399     0.341     0.350     0.361     0.397     0.440     0.453
    ## 12 GA-12     0.384     0.303     0.318     0.334     0.388     0.429     0.442
    ## 13 GA-13     0.586     0.494     0.513     0.530     0.590     0.635     0.647
    ## 14 GA-14     0.259     0.184     0.198     0.212     0.261     0.302     0.312
    ## # … with 1 more variable: p_mrp_975 <dbl>

``` r
mrp_val <- summ_sims(drw) %>% 
  left_join(distinct(acs_GA, cd, clinton_vote))
```

    ## Joining, by = "cd"

``` r
library(ggrepel)

scatter_45(mrp_val, clinton_vote, p_mrp_est,
           xlab = "Clinton Vote",
           ylab = "MRP Estimate") +
  geom_text_repel(aes(label = cd))
```

<img src="README_files/figure-gfm/unnamed-chunk-7-1.png" style="display: block; margin: auto;" />
