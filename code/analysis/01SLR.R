###
# Regression Analysis
# Murphy John
# last modified: 2025-04-06
# This script run the SLR model
###

# Setup ========================================================================

# load data
dat <- readRDS(here::here("data/processed/clean-data.rds"))

# SLR ==========================================================================

# full model
fit1 <- lm(rate_sfr_1000 ~ theme1 + theme2 + theme3 + theme4 + avg_temp +
            max_temp + avg_precip + hsa_value + rucc_value + vcas_value +
            dog_pop_ratio, data=dat)
summary(fit1)

# final model
fit2 <- lm(rate_sfr_1000 ~ theme3 + avg_temp + avg_precip
             + rucc_value + vcas_value + dog_pop_ratio, data=dat)
summary(fit2)

# Save as rds ==================================================================
saveRDS(fit2, here::here("results/output/slr.rds"))

# END OF SCRIPT ================================================================