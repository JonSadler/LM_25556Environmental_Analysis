library(tidyverse)
library(car)
library(performance)
library(ggfortify)
library(MuMIn)
library(tibble)
library(dplyr)
library(MASS)

# ERS beetle
# patch age (years) - 3 to 20 - uniform
# patch size (ha) - 1 to 30 - uniform
# patch isolation (m) - 10 - 1500 - normal
# veg - abundance of veg
# n = 150
# Response  = Beetle SR


# Set seed for reproducibility
set.seed(123)

# Simulate data with reduced variability
df <- tibble(
  flood_n = round(rpois(150, 4), 0),  # Keep lambda for flood_n same
  patch_size = round(runif(150, 100, 1200), 0),  # Narrow range for patch_size
  patch_isolation = abs(rnorm(150, 1000, 200)),  # Reduce variability for patch_isolation
  veg = round(runif(150, 5, 100), 0)  # Narrow range for veg
) %>% 
  mutate(log_mean = 3 + 0.3*scale(flood_n) + 0.1*scale(patch_size) - 0.4*scale(veg) - 0.4*scale(patch_isolation) - 0.4*scale(flood_n) * scale(veg),
         SR = round(rpois(150, exp(log_mean)), 0))

scatterplotMatrix(~SR + flood_n + patch_size + veg + patch_isolation, data = df, diag = list(method = "boxplot"))

# Fit Poisson regression
mod_poisson <- glm(SR ~ flood_n + patch_isolation + patch_size + veg+ flood_n * veg + flood_n * patch_isolation, data = df, family = "poisson")
summary(mod_poisson)
vif(mod_poisson)

#remove highest interaction vif flood_n:patch_isolation  
mod_poisson <- glm(SR ~ + patch_size + patch_isolation + veg + flood_n * veg, data = df, family = "poisson")
summary(mod_poisson)
vif(mod_poisson)

#remove highest interaction vif flood_n:veg  
mod_poisson <- glm(SR ~ veg + flood_n+ patch_size + patch_isolation, data = df, family = "poisson")
summary(mod_poisson)
vif(mod_poisson)

# Check for overdispersion
overdispersion <- sum(residuals(mod_poisson, type = "pearson")^2) / mod_poisson$df.residual
overdispersion

# test using performance package
library(performance)
check_overdispersion(mod_poisson)

autoplot(mod_poisson) # big mess

# run -vebin model
# library(MASS)

mod_nb <- glm.nb(SR ~ veg + flood_n+ patch_size + patch_isolation, data = df)
summary(mod_nb) 
vif(mod_nb)

# Check for overdispersion
overdispersion <- sum(residuals(mod_nb, type = "pearson")^2) / mod_nb$df.residual
overdispersion

# test using performance package
library(performance)
check_overdispersion(mod_nb)

# export data file
write_csv(df[,-5], file = "ERS_SR2024.csv")

