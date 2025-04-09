###
# Regression Analysis
# Murphy John
# last modified: 2025-04-06
## This script runs the SLR model plus some others that have assumptions better
## fit for our data.
###

## TODO: likelihood test for effects 
## TODO: test for best performing model: AIC, rsq, rmse

# Setup ========================================================================
# load packages
library(dplyr)
library(ggplot2)

# load data
dat <- readRDS(here::here("data/processed/clean-data.rds"))

# formulas

count_formula1 <- sfr_count1 ~ theme1 + theme2 + theme3 + theme4 + 
  population_sd + avg_temp + max_temp + avg_precip + hsa_value + rucc_value + 
  vcas_value + dog_pop_sd

count_formula2 <- sfr_count2 ~ theme1 + theme2 + theme3 + theme4 + 
  population_sd + avg_temp + max_temp + avg_precip + hsa_value + rucc_value + 
  vcas_value + dog_pop_sd

count_formula3 <- sfr_count_total ~ theme1 + theme2 + theme3 + theme4 + 
  population_sd + avg_temp + max_temp + avg_precip + hsa_value + rucc_value + 
  vcas_value + dog_pop_sd

# EDA ==========================================================================

# histograms of sfr counts

## 2016-2019
ggplot(dat, aes(x=sfr_count1)) +
  geom_histogram()

### filter out zeros
dat1 <- dat %>% filter(sfr_count1 != 0)

ggplot(dat1, aes(x=sfr_count1)) +
  geom_histogram()

mean(dat1$sfr_count1)
var(dat1$sfr_count1)

## 2019-2022
ggplot(dat, aes(x=sfr_count2)) +
  geom_histogram()

### filter out zeros
dat2 <- dat %>% filter(sfr_count2 != 0)

ggplot(dat2, aes(x=sfr_count2)) +
  geom_histogram()

mean(dat2$sfr_count2)
var(dat2$sfr_count2)

## aggregated
dat3 <- dat %>% filter(sfr_count_total != 0)

ggplot(dat3, aes(x=sfr_count_total)) +
  geom_histogram()

mean(dat3$sfr_count_total)
var(dat3$sfr_count_total)

## we are dealing with count data. variance is much larger than means. lots of
## zeros. poisson not suitable

# SLR ==========================================================================
## 2016-2019 only 

# full model
fit_slr1 <- lm(rate_sfr1_1000 ~ theme1 + theme2 + theme3 + theme4 + avg_temp +
            max_temp + avg_precip + hsa_value + rucc_value + vcas_value +
            dog_pop_ratio, data=dat)
summary(fit_slr1)

# final model
fit_slr2 <- lm(rate_sfr1_1000 ~ theme3 + avg_temp + avg_precip
             + rucc_value + vcas_value + dog_pop_ratio, data=dat)
summary(fit_slr2)

## 2019-2022 only 

# full model
fit_slr3 <- lm(rate_sfr2_1000 ~ theme1 + theme2 + theme3 + theme4 + avg_temp +
             max_temp + avg_precip + hsa_value + rucc_value + vcas_value +
             dog_pop_ratio, data=dat)
summary(fit_slr3)

# final model
fit_slr4 <- lm(rate_sfr2_1000 ~ theme3 + avg_temp + 
             avg_precip + vcas_value + dog_pop_ratio, 
           data=dat)
summary(fit_slr4)

## aggregated

# full model
fit_slr5 <- lm(rate_sfr_total_1000 ~ theme1 + theme2 + theme3 + theme4 + avg_temp +
             max_temp + avg_precip + hsa_value + rucc_value + vcas_value +
             dog_pop_ratio, data=dat)
summary(fit_slr5)

# final model
fit_slr6 <- lm(rate_sfr2_1000 ~ theme3 + avg_temp
            + avg_precip + vcas_value + dog_pop_ratio, 
           data=dat)
summary(fit_slr6)

## Save as rds ==================================================================
saveRDS(fit_slr2, here::here("results/output/slr2016-2019.rds"))
saveRDS(fit_slr4, here::here("results/output/slr2019-2022.rds"))
saveRDS(fit_slr6, here::here("results/output/slr-total.rds"))

# Quasi-Poisson regression ======================================================

## 2016-2019 ====
# full
fit_qp1 <- glm(
  formula = count_formula1,
  family = quasipoisson(link = "log"),
  data = dat
)
summary(fit_qp1)

# final
fit_qp2 <- glm(
  sfr_count1 ~ theme1 + population_sd + avg_temp + avg_precip + hsa_value + 
    rucc_value + vcas_value + dog_pop_sd,
  family = quasipoisson(link = "log"),
  data = dat
)
summary(fit_qp2)

# get preds
fit_qp2_pred <- predict(fit_qp2, newdata = dat)
fit_qp2_result <- dat %>%
  select(sfr_count1) %>%
  bind_cols(fit_qp2_pred)

# calculate rmse
fit_qp2_result %>%
  yardstick::rmse(
    truth=sfr_count1, 
    estimate=...2
  )
# calculate rsq
fit_qp2_result %>%
  yardstick::rsq(
    truth=sfr_count1, 
    estimate=...2
  )

## 2019-2022 ====
# full
fit_qp3 <- glm(
  formula = count_formula2,
  family = quasipoisson(link = "log"),
  data = dat
)
summary(fit_qp3)

# final
fit_qp4 <- glm(
  sfr_count2 ~ population_sd + avg_temp + avg_precip + hsa_value + 
    rucc_value + dog_pop_sd,
  family = quasipoisson(link = "log"),
  data = dat
)
summary(fit_qp4)

# get preds
fit_qp4_pred <- predict(fit_qp4, newdata = dat)
fit_qp4_result <- dat %>%
  select(sfr_count2) %>%
  bind_cols(fit_qp4_pred)

# calculate rmse
fit_qp4_result %>%
  yardstick::rmse(
    truth=sfr_count2, 
    estimate=...2
  )
# calculate rsq
fit_qp4_result %>%
  yardstick::rsq(
    truth=sfr_count2, 
    estimate=...2
  )

## aggregated ====
# full
fit_qp5 <- glm(
  formula = count_formula3,
  family = quasipoisson(link = "log"),
  data = dat
)
summary(fit_qp5)

# final
fit_qp6 <- glm(
  sfr_count_total ~ theme1 + population_sd + avg_temp + avg_precip + hsa_value 
  + rucc_value + vcas_value + dog_pop_sd,
  family = quasipoisson(link = "log"),
  data = dat
)
summary(fit_qp6)

# get preds
fit_qp6_pred <- predict(fit_qp6, newdata = dat)
fit_qp6_result <- dat %>%
  select(sfr_count_total) %>%
  bind_cols(fit_qp6_pred)

# calculate rmse
fit_qp6_result %>%
  yardstick::rmse(
    truth=sfr_count_total, 
    estimate=...2
  )
# calculate rsq
fit_qp6_result %>%
  yardstick::rsq(
    truth=sfr_count_total, 
    estimate=...2
  )

# Negative Binomial ============================================================
## 2016-2019 ====
# full
fit_nb1 <- MASS::glm.nb(
  formula = count_formula1,
  data = dat
)
summary(fit_nb1)

# final
fit_nb2 <- MASS::glm.nb(
  sfr_count1 ~ theme3 + theme4 + population_sd + avg_temp + avg_precip + 
    rucc_value + dog_pop_sd,
  data = dat
)
summary(fit_nb2)

# get preds
fit_nb2_pred <- predict(fit_nb2, newdata = dat)
fit_nb2_result <- dat %>%
  select(sfr_count1) %>%
  bind_cols(fit_nb2_pred)

# calculate rmse
fit_nb2_result %>%
  yardstick::rmse(
    truth=sfr_count1, 
    estimate=...2
  )
# calculate rsq
fit_nb2_result %>%
  yardstick::rsq(
    truth=sfr_count1, 
    estimate=...2
  )

## 2019-2022 ====
# full
fit_nb3 <- MASS::glm.nb(
  formula = count_formula2,
  data = dat
)
summary(fit_nb3)

# final
fit_nb4 <- MASS::glm.nb(
  sfr_count2 ~ theme2 + theme3 + theme4 + population_sd + avg_temp
     + avg_precip + hsa_value + rucc_value +
    dog_pop_sd,
  data = dat
)
summary(fit_nb4)

# get preds
fit_nb4_pred <- predict(fit_nb4, newdata = dat)
fit_nb4_result <- dat %>%
  select(sfr_count2) %>%
  bind_cols(fit_nb4_pred)

# calculate rmse
fit_nb4_result %>%
  yardstick::rmse(
    truth=sfr_count2, 
    estimate=...2
  )
# calculate rsq
fit_nb4_result %>%
  yardstick::rsq(
    truth=sfr_count2, 
    estimate=...2
  )

## aggregated ====
# full
fit_nb5 <- MASS::glm.nb(
  formula = count_formula3,
  data = dat
)
summary(fit_nb5)

# final
fit_nb6 <- MASS::glm.nb(
  sfr_count_total ~ theme2 + theme3 + theme4 + population_sd + avg_temp
    + avg_precip + hsa_value + rucc_value +
    dog_pop_sd,
  data = dat
)
summary(fit_nb6)

# get preds
fit_nb6_pred <- predict(fit_nb6, newdata = dat)
fit_nb6_result <- dat %>%
  select(sfr_count_total) %>%
  bind_cols(fit_nb6_pred)

# calculate rmse
fit_nb6_result %>%
  yardstick::rmse(
    truth=sfr_count_total, 
    estimate=...2
  )
# calculate rsq
fit_nb6_result %>%
  yardstick::rsq(
    truth=sfr_count_total, 
    estimate=...2
  )

# Hurdle =======================================================================

## 2016-2019 ====
# TODO: check final models

# full model
fit_hd1 <- pscl::hurdle(
  formula = count_formula1,
  dist = "negbin",
  data = dat
)
summary(fit_hd1)

# final
fit_hd2 <- pscl::hurdle(
  sfr_count1 ~ theme3 + avg_temp + avg_precip + rucc_value + dog_pop_sd,
  dist = "negbin",
  data = dat
)
summary(fit_hd2)

## 2019-2022 ====

# full model
fit_hd3 <- pscl::hurdle(
  formula = count_formula2,
  dist = "negbin",
  data = dat
)
summary(fit_hd3)

# final
fit_hd4 <- pscl::hurdle(
  sfr_count2 ~ theme1 + theme3 + avg_temp + avg_precip + rucc_value + vcas_value
  + dog_pop_sd,
  dist = "negbin",
  data = dat
)
summary(fit_hd4)

## aggregate ====

# full model
fit_hd5 <- pscl::hurdle(
  formula = count_formula3,
  dist = "negbin",
  data = dat
)
summary(fit_hd5)

# final
fit_hd6 <- pscl::hurdle(
  sfr_count_total ~ theme3 + population_sd + avg_temp + avg_precip + rucc_value 
  + dog_pop_sd,
  dist = "negbin",
  data = dat
)
summary(fit_hd6)

# ZINB =========================================================================

## 2016-2019 ====

# full model
fit_zinb1 <- pscl::zeroinfl(
 formula = count_formula1,
  dist = "negbin",
  data = dat
  )
summary(fit_zinb1)

# final model
fit_zinb2 <- pscl::zeroinfl(sfr_count1 ~ theme1 + theme3 + 
                              population_sd + avg_temp + avg_precip + rucc_value
                            + vcas_value + dog_pop_sd,
                       data = dat,
                       dist = "negbin",
                       link = "logit")
summary(fit_zinb2)

fit_zinb2_pred <- predict(fit_zinb2, newdata = dat)
fit_zinb2_result <- dat %>%
  select(sfr_count1) %>%
  bind_cols(fit_zinb2_pred)

# calculate rmse
fit_zinb2_result %>%
  yardstick::rmse(
    truth=sfr_count1, 
    estimate=...2
  )
# calculate rsq
fit_zinb2_result %>%
  yardstick::rsq(
    truth=sfr_count1, 
    estimate=...2
  )

## 2019-2022 ====

# full model
fit_zinb3 <- pscl::zeroinfl(
  formula = count_formula2,
  data = dat,
  dist = "negbin",
  link = "logit")
summary(fit_zinb3)

# final model
fit_zinb4 <- pscl::zeroinfl(sfr_count2 ~ theme1 + theme4 + avg_temp + avg_precip 
                            + hsa_value + rucc_value + dog_pop_sd,
                       data = dat,
                       dist = "negbin",
                       link = "logit")
summary(fit_zinb4)

fit_zinb4_pred <- predict(fit_zinb4, newdata = dat)
fit_zinb4_result <- dat %>%
  select(sfr_count2) %>%
  bind_cols(fit_zinb4_pred)

# calculate rmse
fit_zinb4_result %>%
  yardstick::rmse(
    truth=sfr_count2, 
    estimate=...2
  )
# calculate rsq
fit_zinb4_result %>%
  yardstick::rsq(
    truth=sfr_count2, 
    estimate=...2
  )

## aggregated ====

# full model
fit_zinb5 <- pscl::zeroinfl(
  formula = count_formula3,
  data = dat,
  dist = "negbin",
  link = "logit")
summary(fit_zinb5)

# final model
fit_zinb6 <- pscl::zeroinfl(sfr_count_total ~ theme3 + theme4 + avg_temp
                          + avg_precip + rucc_value + vcas_value + dog_pop_sd,
                        data = dat,
                        dist = "negbin",
                        link = "logit")
summary(fit_zinb6)

fit_zinb6_pred <- predict(fit_zinb6, newdata = dat)
fit_zinb6_result <- dat %>%
  select(sfr_count_total) %>%
  bind_cols(fit_zinb6_pred)

# calculate rmse
fit_zinb6_result %>%
  yardstick::rmse(
    truth=sfr_count_total, 
    estimate=...2
  )
# calculate rsq
fit_zinb6_result %>%
  yardstick::rsq(
    truth=sfr_count_total, 
    estimate=...2
  )

# END OF SCRIPT ================================================================