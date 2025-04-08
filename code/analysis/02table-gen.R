###
# Table Generating Script
# Murphy John
# last modified: 2025-04-08
###

# Setup ========================================================================

# load packages
library(dplyr)

# Table 1 ======================================================================
## SLR results

# load model object
fit1 <- readRDS(here::here("results/output/slr.rds"))
summary(fit1)

t1 <- fit1 %>%
  gtsummary::tbl_regression(
    estimate_fun = purrr::partial(gtsummary::style_ratio, digits = 3)
  ) %>%
  gtsummary::as_gt()

# save as png 
gt::gtsave(t1, here::here("results/tables/slr_results.png"))

# TODO: add hot/cold spot decription and spatial lag tables