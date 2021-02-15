# installs dependencies, runs R CMD check, runs covr::codecov()
do_package_checks(codecov = FALSE)

if (ci_on_ghactions() && ci_has_env("BUILD_PKGDOWN")) {
  # creates pkgdown site and pushes to gh-pages branch
  # only for the runner with the "BUILD_PKGDOWN" env var set
  get_stage("install") %>%
    add_step(step_install_cran("patchwork"))

  do_readme_rmd()
  do_pkgdown()
}
