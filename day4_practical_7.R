## ----child="practicals/Multiple_likelihood.qmd"-------------------------------

## -----------------------------------------------------------------------------
#| warning: false
#| message: false


library(dplyr)
library(INLA)
library(inlabru) 
library(sf)
library(terra)
library(tidyverse)
library(fmesher)
library(tidyterra)

# load some libraries to generate nice map plots
library(scico)
library(ggplot2)
library(patchwork)



## -----------------------------------------------------------------------------

N = 200
x =  runif(N)
df = data.frame(idx = 1:N,
                x = x)

# simulate data
df = df %>% 
  mutate(y_gaus = rnorm(N, mean = 1 + 1.5 * x), sd = 0.5) %>%
  mutate(y_pois = rpois(N, lambda  = exp( -1 + 1.5 * x))) 

# plot the data
df %>% ggplot() + 
  geom_point(aes(x, y_gaus, color = "Gaussian")) +
  geom_point(aes(x, y_pois, color = "Poisson")) 



## -----------------------------------------------------------------------------
cmp = ~ -1 + 
  Intercept_gaus(1) + 
  Intercept_pois(1) +
  covariate(x, model = "linear") 


## -----------------------------------------------------------------------------
lik_gaus = bru_obs(formula = y_gaus ~ Intercept_gaus + covariate,
                    data = df)

lik_pois = bru_obs(formula = y_pois ~ Intercept_pois + covariate,
                    data = df,
                   family = "poisson")




## -----------------------------------------------------------------------------
#| message: false
#| warning: false

load(here::here("datasets/pcod.RData"))

pcod_df = pcod_df %>% filter(year==2003)
pcod_sf =   st_as_sf(pcod_df, coords = c("lon","lat"), crs = 4326)
pcod_sf = st_transform(pcod_sf,
                       crs = "+proj=utm +zone=9 +datum=WGS84 +no_defs +type=crs +units=km" )

depth_r <- rast(qcs_grid, type = "xyz")
crs(depth_r) <- crs(pcod_sf)




## -----------------------------------------------------------------------------
#| echo: false
#| message: false
#| eval: true
#| fig-align: center
#| fig-width: 6
#| fig-height: 6
#| fig-cap: "Map of the locations where Pacfic Cod were caught and the depth if the study area"
#| label: fig-pcod_map
#| code-fold: true



ggplot()+
  geom_spatraster(data=depth_r$depth)+
      geom_sf(data=pcod_sf,aes(color=factor(present))) +
    scale_color_manual(name="Locations where Pacific Cod \nwere caught",
                     values = c("black","orange"),
                     labels= c("Absence","Presence"))+
  scale_fill_scico(name = "Depth",
                   palette = "nuuk",
                   na.value = "transparent" ) + xlab("") + ylab("")



## -----------------------------------------------------------------------------
#| warning: false
#| message: false

mesh = fm_mesh_2d(loc = pcod_sf,           # Build the mesh
                  cutoff = 2,
                  max.edge = c(10,20),     # The largest allowed triangle edge length.
                  offset = c(5,50))        # The automatic extension distance


spde_model =  inla.spde2.pcmatern(mesh,
                                   prior.sigma = c(1, 0.5),
                                   prior.range = c(100, 0.5))



## -----------------------------------------------------------------------------
#| warning: false
#| message: false


cmp_hurdle <- ~
  Intercept_biomass(1) +
    depth_biomass(depth_scaled, model = "linear") +
    depth2_biomass(depth_scaled2, model = "linear") +
    space_biomass(geometry, model = spde_model) +
    Intercept_caught(1) +
    depth_caught(depth_scaled, model = "linear") +
    depth2_caught(depth_scaled2, model = "linear") +
    space_caught(geometry, model = spde_model)



## -----------------------------------------------------------------------------
#| warning: false
#| message: false


biomass_obs <- bru_obs(formula = density ~  Intercept_biomass + depth_biomass + depth2_biomass + space_biomass,
      family = "lognormal",
      data = pcod_sf  %>% filter(density>0))

presence_obs <- bru_obs(formula = present ~ Intercept_caught + depth_caught + depth2_caught +
                          space_caught,
  family = "binomial",
  data = pcod_sf,
)

fit_hurdle <- bru(
  cmp_hurdle,
  biomass_obs,
  presence_obs
)



## -----------------------------------------------------------------------------
tidy(fit_hurdle)
tidy(fit_hurdle,"hyperpar")


## -----------------------------------------------------------------------------

cmp_joint <- ~
  Intercept_biomass(1) +
    depth_biomass(depth_scaled, model = "linear") +
    depth2_biomass(depth_scaled2, model = "linear") +
    Intercept_caught(1) +
    depth_caught(depth_scaled, model = "linear") +
    depth2_caught(depth_scaled2, model = "linear") +
    space(geometry, model = spde_model) +
    space_copy(geometry, copy = "space", fixed = FALSE)



## -----------------------------------------------------------------------------

biomass_obs <- bru_obs(formula = density ~  Intercept_biomass + depth_biomass + depth2_biomass + space,
      family = "lognormal",
      data = pcod_sf  %>% filter(density>0))

presence_obs <- bru_obs(formula = present ~ Intercept_caught + depth_caught + depth2_caught +space_copy,
  family = "binomial",
  data = pcod_sf,
)



## -----------------------------------------------------------------------------

fit_hurdle_shared <- bru(
  cmp_joint,
  biomass_obs,
  presence_obs
)









## -----------------------------------------------------------------------------
rMatern <- function(n, coords, sigma=1, range, 
                    kappa = sqrt(8*nu)/range, 
                    variance = sigma^2, 
                    nu=1) {
  m <- as.matrix(dist(coords))
  m <- exp((1-nu)*log(2) + nu*log(kappa*m)-
             lgamma(nu))*besselK(m*kappa, nu)
  diag(m) <- 1
  return(drop(crossprod(chol(variance*m),
                        matrix(rnorm(nrow(coords)*n), ncol=n))))
}


## -----------------------------------------------------------------------------
# Intercept on reparametrized model
beta <- c(-5, 3) 
# Random field marginal variances for omega1 and omega2:
m.var <- c(0.5, 0.4) 
# GRF range parameters for omega1 and omega2:
range <- c(4, 6)
# Copy parameters: reparameterization of coregionalization 
# parameters
lambda <- c(0.7) 
# Standard deviations of error terms
e.sd <- c(0.3, 0.2)



## -----------------------------------------------------------------------------
# define the area of interest
poly_geom = st_polygon(list(cbind(c(0,10,10,0,0), c(0,0,5,5,0)) ))
# Wrap it in an sfc (simple feature collection)
poly_sfc <- st_sfc(poly_geom)
# Now create the sf object
border <- st_sf(id = 1, geometry = poly_sfc)



# how many observation we have
n1 <- 200
n2 <- 150
n_common = 50

# simulate observation locations

loc_common = st_sf(geometry = st_sample(border, n_common))
loc_only1 = st_sf(geometry = st_sample(border, n1-n_common))
loc_only2 = st_sf(geometry = st_sample(border, n2-n_common))



# simulate the two gaussian field at the locations
z1 <- rMatern(1, st_coordinates(rbind( loc_common,loc_only1, loc_only2)), range = range[1],
                  sigma = sqrt(m.var[1]))

z2 <- rMatern(1, st_coordinates(rbind(loc_common, loc_only2)), range = range[2],
                  sigma = sqrt(m.var[2]))


## Create data.frame
loc1 = rbind( loc_common, loc_only1)
loc2 = rbind( loc_common, loc_only2)

df1 =  loc1 %>% mutate(z1 = z1[1:n1])
df2 =  loc2 %>% mutate(z1 = z1[-c(1:(n1-n_common))], z2 =z2)


## create the linear predictors

df1  = df1 %>%
  mutate(eta1 = beta[1] + z1)

df2  = df2 %>%
  mutate(eta2 = beta[2] + lambda * z1 + z2)


# simulate data by addint the obervation noise

df1  = df1 %>%
  mutate(y = rnorm(n1, mean = eta1, sd = e.sd[1]))

df2  = df2 %>%
  mutate(y = rnorm(n2, mean = eta2, sd = e.sd[1]))


## ----out.width="95%"----------------------------------------------------------
p1 = ggplot(data = df1) + geom_sf(aes(color = z1)) 
p2 = ggplot(data = df2) + geom_sf(aes(color = z2)) 
p1+p2+plot_layout(ncol = 1)


## -----------------------------------------------------------------------------
mesh <-  fm_mesh_2d(loc = rbind(loc1, loc2), 
                   boundary = border,
                     max.edge = c(0.5, 1.5), 
                     offset = c(0.1, 2.5), 
                     cutoff = 0.1)



## ----echo = F-----------------------------------------------------------------
ggplot() + 
  gg(mesh) + 
  geom_sf(data =  df1,  size = 2, aes(color = "data 1")) +
    geom_sf(data =  df2, aes(color = "data 2")) + xlab("") + ylab("")




## -----------------------------------------------------------------------------
cmp = ~ -1 +  Intercept1(1) + Intercept2(1) +
  omega1(geometry, model = spde) +
  omega1_copy(geometry, copy = "omega1", fixed = FALSE) +
  omega2(geometry, model = spde)



