library(ccesMRPprep)

library(ccesMRPrun)
cces_GA # built-in data


cd_elec <- cces_GA %>%
  distinct(cd, clinton_vote)

acs_all <- get_acs_cces(acscodes_age_sex_educ, year = 2016, dataset = "acs5")

acs_GA <- acs_all %>%
  filter(str_detect(cd, "GA")) %>%
  transmute(year, cd, female, educ, age, count) %>%
  filter(count > 0) %>%
  left_join(cd_elec)


elec_GA <- distinct(acs_GA, cd, clinton_vote, clinton_vote_2pty)

usethis::use_data(acs_GA, overwrite = TRUE)
usethis::use_data(elec_GA, overwrite = TRUE)
