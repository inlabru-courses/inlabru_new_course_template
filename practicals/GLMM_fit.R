## -----------------------------------------------------------------------------
#| warning: false
#| message: false

library(dplyr)
library(INLA)
library(ggplot2)
library(patchwork)
library(inlabru)     



## -----------------------------------------------------------------------------
grouseticks<- read.csv(here::here("datasets/grouseticks.csv"))


## -----------------------------------------------------------------------------
grouseticks  = grouseticks %>%
  group_by(BROOD, YEAR) %>%
  mutate(ij = cur_group_id()) %>%
  ungroup() %>%
  mutate(ijk = seq_along(INDEX)) 



## -----------------------------------------------------------------------------
#| echo: true
#| eval: false

# bru_options_set(control.compute = list(dic = T, waic = T))
# 
# cmp= ~ -1 + year(...) +
#   height(...) +
#   brood_year(...) +
#   brood_year_chicken(...)
# 
# lik = bru_obs(forumula = ...,
#               data = ...,
#               family = ...)
# 
# fit = bru(cmp, lik)
# 




## -----------------------------------------------------------------------------
fit$summary.fixed
fit$summary.hyperpar



## -----------------------------------------------------------------------------
tidy(fit)
tidy(fit, "hyperpar")


## -----------------------------------------------------------------------------
glance(fit)


## -----------------------------------------------------------------------------
#| fig-width: 4
#| fig-align: center
#| fig-height: 4
plot(fit, "height")





## -----------------------------------------------------------------------------
#| fig-width: 6
#| fig-height: 4
#| fig-align: center
#| code-fold: show

plot(fit, "brood_year") 



## -----------------------------------------------------------------------------
#| code-fold: show
fit$marginals.fixed$height %>% 
  ggplot() +
  geom_line(aes(x,y)) + 
  ggtitle("Linear effect of altitude")
















## -----------------------------------------------------------------------------
fitted_values1 <- predict(fit, 
                         grouseticks, ~ exp(year + height + brood_year + brood_year_chicken),
               n.samples = 500L)

ggplot(fitted_values1, aes(x = TICKS, y = mean)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2, colour = "grey50") +
  geom_pointrange(aes(ymin = q0.025, ymax =  q0.975),
                  alpha = 0.4, colour = "#1B4F5E") +
  labs(x = "Observed ticks", y = "Expected counts") +
  theme_minimal(base_size = 14)



## -----------------------------------------------------------------------------
fitted_values <- augment(
  fit,
  data = grouseticks,
  pred_formula = ~ exp(year + height + brood_year + brood_year_chicken),
  n_samples = 500L,
  seed = 1L
)

ggplot(fitted_values, aes(x = TICKS, y = .fitted)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2, colour = "grey50") +
  geom_pointrange(aes(ymin = .fitted_low, ymax = .fitted_high),
                  alpha = 0.4, colour = "#1B4F5E") +
  labs(x = "Observed ticks", y = "Expected counts") +
  theme_minimal(base_size = 14)



## -----------------------------------------------------------------------------
#| code-fold: show
#| message: false
#| warning: false
#| 
# grid across the observed height range
yh_grid <- expand.grid(YEAR= c("95","96","97"),
  cHEIGHT = seq(min(grouseticks$cHEIGHT),
                max(grouseticks$cHEIGHT), 
                length.out = 100))

# predict the height and year components only

pred_yh <- predict(fit, yh_grid, ~ exp(height+year))

ggplot(pred_yh, aes(cHEIGHT, mean)) +
  geom_ribbon(aes(ymin = q0.025, ymax = q0.975), alpha = 0.25, fill = "#1B4F5E") +
  geom_line(colour = "#1B4F5E", linewidth = 0.8) +
  labs(x = "Height (centred)", y = "Expected Ticks") +
  geom_point(data=grouseticks,aes(cHEIGHT,TICKS),alpha=0.25)+
  facet_wrap(~YEAR)
  




## -----------------------------------------------------------------------------
inla.priors.used(fit)






## -----------------------------------------------------------------------------

# First we define the logGamma (0.01,0.01) prior 

prec.prior <- list(prec = list(prior = "loggamma",  # prior name
                               param = c(0.01, 0.01))) # prior parameters
                        

cmp3 =  ~ -1 + year(YEAR, model = "iid", initial = log(0.1), fixed = T) +
  height(cHEIGHT, model =  "linear", prec.linear = 0.1) + 
  brood_year(ij , model = "iid", hyper = prec.prior) +
  brood_year_chicken(ijk, model= "iid", hyper = prec.prior)


fit3 = bru(cmp3, lik) 


