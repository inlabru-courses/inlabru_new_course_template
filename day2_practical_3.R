## ----child="practicals/Ar1_ex.qmd"--------------------------------------------

## -----------------------------------------------------------------------------
#| warning: false
#| message: false
library(tidyverse)
library(INLA)
library(ggplot2)
library(patchwork)
library(inlabru)     


## -----------------------------------------------------------------------------
set.seed(123)

phi = 0.8
tau_u = 10
marg.prec = tau_u * (1-phi^2) # ar1 in INLA is parametrized as marginal variance
u_t =  as.vector(arima.sim(list(order = c(1,0,0), ar = phi), 
                          n = 100,
                          sd=sqrt(1/tau_u)))
a = 1
tau_e = 5
epsilon_t = rnorm(100, sd = sqrt(1/tau_e))
y = a + u_t + epsilon_t


ts_dat <- data.frame(y =y , x= 1:100)



## -----------------------------------------------------------------------------
#| echo: false
#| fig-align: center
#| fig-width: 4
#| fig-height: 4

ggplot(ts_dat,aes(y=y,x=x))+geom_line()



## -----------------------------------------------------------------------------
# Model components
cmp =  ~ -1 + alpha(1) + ut(x,model = "ar1")



## -----------------------------------------------------------------------------
# Model formula
formula = y ~ alpha + ut
# Observational model
lik =  bru_obs(formula = y ~.,
            family = "gaussian",
            data = ts_dat)



## -----------------------------------------------------------------------------
# fit the model
fit.ar1 = bru(cmp, lik)

# compare against the true values

data.frame(
  true = c(a,tau_e,marg.prec,phi),
  rbind(fit.ar1$summary.fixed[,c(1,3,5)],
        fit.ar1$summary.hyperpar[,c(1,3,5)])
        ) %>% round(2)


## -----------------------------------------------------------------------------
pred_ar1 = predict(fit.ar1, ts_dat, ~ alpha + ut)

ggplot(pred_ar1,aes(y=mean,x=x))+
  geom_line()+
    geom_ribbon(aes(x = x, y = mean, ymin = q0.025, ymax = q0.975),
                alpha = 0.5) +
  geom_point(aes(y=y,x=x))



## -----------------------------------------------------------------------------
ts.forecast <- rbind(ts_dat, 
  data.frame(y = rep(NA, 50), x = 101:150))



## -----------------------------------------------------------------------------

cmp =  ~ -1 + alpha(1) + ut(x,model = "ar1")

pred_lik =  bru_obs(formula = y ~.,
            family = "gaussian",
            data = ts.forecast)

fit.forecast = bru(cmp, pred_lik)




## -----------------------------------------------------------------------------
pred_forecast = predict(fit.forecast, ts.forecast, ~ alpha + ut)

p1= ggplot(pred_forecast,aes(y=mean,x=x))+
  geom_line()+
    geom_ribbon(aes(x = x, y = mean, ymin = q0.025, ymax = q0.975),
                alpha = 0.5) +
  geom_point(data=ts_dat, aes(y=y,x=x))



## ----child="practicals/Ar1_Rw1_ex.qmd"----------------------------------------

## -----------------------------------------------------------------------------
#| warning: false
#| message: false
library(tidyverse) 
library(INLA) 
library(ggplot2)
library(patchwork) 
library(inlabru)


## -----------------------------------------------------------------------------

greatLakes.df<- read.csv(here::here("datasets/greatLakes.csv"))



## -----------------------------------------------------------------------------
#| echo: false
#| fig-align: center
#| fig-width: 6
#| fig-height: 5


greatLakes.df %>%
  ggplot(aes(x=year,y=height)) + 
  geom_point() +
  facet_wrap(~Lakes,scales="free")



## -----------------------------------------------------------------------------
greatLakes.df$t.idx <- greatLakes.df$year-1917

Erie.df = greatLakes.df %>% filter(Lakes == "Erie")





## -----------------------------------------------------------------------------
pc_prior <- list(theta = list(prior = "pc.prec", param = c(1, 0.01)),
                 rho = list(prior = "pc.cor0", param = c(0.5, 0.3))) 

prec.tau_e <- list(prec = list(prior = "loggamma",   # prior name
                             param = c(1, 1))) # prior values

# Model components
cmp =  ~ -1 + alpha(1) + ut(t.idx, model = "ar1",  hyper = pc_prior)
# Model formula
formula = height ~ alpha + ut


# Observational model
lik =  bru_obs(formula = height  ~.,
            family = "gaussian",
            data = Erie.df,
            control.family = list(hyper = prec.tau_e))

# fit the model
fit.Erie_ar1 = bru(cmp, lik)






## -----------------------------------------------------------------------------

pc_prior <- list(theta = list(prior = "pc.prec", param = c(1, 0.01))) 

prec.tau_e <- list(prec = list(prior = "loggamma",   # prior name
                             param = c(1, 1))) # prior values



## -----------------------------------------------------------------------------
cmp_rw =  ~ -1 + alpha(1) + 
  ut(t.idx ,
     constr=FALSE,
     model = "rw1",
     hyper=pc_prior,
     scale.model = TRUE)


## -----------------------------------------------------------------------------

# Model formula
formula = height ~ alpha + ut
# Observational model
lik =  bru_obs(formula = height  ~.,
            family = "gaussian",
            data = Erie.df,
            control.family = list(hyper = prec.tau_e))
# fit the model
fit.Erie_rw1 = bru(cmp_rw, lik)
# Model predictions
pred_rw1.Erie = predict(fit.Erie_rw1, Erie.df, ~ alpha + ut)



## -----------------------------------------------------------------------------
#| echo: false
#| fig-align: center
#| fig-width: 4
#| fig-height: 4

ggplot(pred_rw1.Erie,aes(y=mean,x=year))+
  geom_line()+
    geom_ribbon(aes(x = year, y = mean, ymin = q0.025, ymax = q0.975),
                alpha = 0.5) +
  geom_point(aes(y=height,x=year))





## -----------------------------------------------------------------------------
greatLakes.df$lake_id <- as.numeric(as.factor(greatLakes.df$Lakes))

pc_prior <- list(theta = list(prior = "pc.prec", param = c(1, 0.01)),
                 rho = list(prior = "pc.cor0", param = c(0.5, 0.3))) 

prec.tau_e <- list(prec = list(prior = "loggamma",   # prior name
                             param = c(1, 10))) # prior values



## -----------------------------------------------------------------------------
# Model components
cmp =  ~ -1 + alpha(1) + ut(year,model = "ar1",
                            hyper = pc_prior,
                            group =lake_id,
                            control.group = 
                            list(model = "iid", 
                                 scale.model = TRUE))


## -----------------------------------------------------------------------------
# Model formula
formula = height ~ alpha + ut


# Observational model
lik =  bru_obs(formula = height  ~.,
            family = "gaussian",
            data = greatLakes.df,
            control.family = list(hyper = prec.tau_e))

# fit the model
fit.all_lakes_ar1 = bru(cmp, lik)

# Model predictions
pred_ar1.all = predict(fit.all_lakes_ar1, greatLakes.df, ~ alpha + ut)



## -----------------------------------------------------------------------------
ggplot(pred_ar1.all,aes(y=mean,x=year))+
  geom_line()+
    geom_ribbon(aes(x = year, y = mean, ymin = q0.025, ymax = q0.975),
                alpha = 0.5) +
  geom_point(aes(y=height,x=year)) + facet_wrap(~Lakes,scales = "free")




## ----child="practicals/Tokyo_rainfall_ex.qmd"---------------------------------

## -----------------------------------------------------------------------------
#| message: false
#| warning: false

library(INLA)
library(inlabru)
library(ggplot2)
library(tidyr)


data("Tokyo")


## -----------------------------------------------------------------------------

# define model component
cmp =  ~ -1 + beta0(1) + time_effect(time, model = "rw2", cyclic = TRUE)

# define model predictor
eta = y ~ beta0 + time_effect

# build the observation model
lik = bru_obs(formula = eta,
              family = "binomial",
              Ntrials = n,
              data = Tokyo)

# fit the model
fit = bru(cmp, lik)


## -----------------------------------------------------------------------------

pTokyo = predict(fit, Tokyo, ~ plogis(beta0 + time_effect))

ggplot(data=pTokyo , aes(x= time, y= y) ) +
  geom_point() + 
  ylab("") + xlab("") +
  # Custom the Y scales:
  scale_y_continuous(
    # Features of the first axis
    name = "",
    # Add a second axis and specify its features
    sec.axis = sec_axis( transform=~./2, name="Probability")
  )  + geom_line(aes(y=mean*2,x=time)) +
  geom_ribbon(aes( ymin = q0.025*2, 
                             ymax = q0.975*2), alpha = 0.5)
  


