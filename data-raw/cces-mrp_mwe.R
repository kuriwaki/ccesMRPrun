library(dataverse)
library(haven)
library(tidyverse)
library(ccesMRPprep)


# Load data ---
ccc <- get_cces_dataverse("cumulative")
elec_data <- read_dta("~/Dropbox/CCES_representation/data/source/election/by-cd_presidential-vote.dta")

# subset to
cc_st <- ccc %>%
  mutate(st = as_factor(st)) %>%
  filter(st == "GA", year == 2016) %>%
  ccc_std_demographics() %>%
  mutate(female = as.numeric(gender == 2)) %>%
  select(year, case_id, weight, weight_post, cd, age, female, educ, race, pid3,
         voted_pres_party, intent_pres_party, vv_turnout_gvm) %>%
  mutate_if(is.labelled, as_factor)

# elec data
cd_elec <- elec_data %>%
  filter(year == 2016, district_lines == 2016, str_detect(cd, "GA")) %>%
  filter(cand == "Clinton") %>%
  transmute(cd, clinton_vote = vote, clinton_vote_2pty = vote_2pty)


# data
cc_df <- cc_st %>%
  left_join(cd_elec, by = "cd")

# binary response
cc_df$response <- as.numeric(cc_df$voted_pres_party == "Democratic")

# rename
cces_GA <- cc_df

# model
form <- "response ~ (1|age) + (1 + female |educ) + clinton_vote + (1|cd)"
fit_GA <- fit_brms(form, cces_GA, verbose = FALSE)


# draws
data("acs_GA", package = "ccesMRPrun")
drw_GA <- poststrat_draws(fit_GA, poststrat_tgt = acs_GA)


# summaries
summ_GA <- summ_sims(drw_GA)

# Save
usethis::use_data(cces_GA, overwrite = TRUE)
usethis::use_data(fit_GA, overwrite = TRUE)
usethis::use_data(summ_GA, overwrite = TRUE)
