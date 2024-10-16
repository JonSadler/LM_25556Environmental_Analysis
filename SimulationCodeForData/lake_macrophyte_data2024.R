# Lake Macrophyte example
# March 2024
# Set seed for reproducibility
set.seed(123)

# Generate land use data
land_use <- rep(c("peatland", "woodland", "agriculture"), each = 50)

# Generate TP values for each level of land use
TP <- c(
  round(rnorm(50, mean = 15, sd = 5),2),  # Peatland
  round(rnorm(50, mean = 25, sd = 6),2),  # Woodland
  round(rnorm(50, mean = 35, sd = 8),2)   # Agriculture
)

# Generate species richness data with a stronger negative relationship with TP for each land use
SR <- c(
  rpois(50, lambda = 25 - 0.5 * TP[land_use == "peatland"]),    # Peatland
  rpois(50, lambda = 20 - 0.3 * TP[land_use == "woodland"]),    # Woodland
  rpois(50, lambda = 15 - 0.2 * TP[land_use == "agriculture"])  # Agriculture
)

# Create a data frame
df <- data.frame(
  lake_id = 1:150,  # Lake IDs
  SR = pmax(SR, 0),  # Ensure non-negative species richness
  TP = TP,  # Total phosphorus data
  land_use = factor(land_use, levels = c("peatland", "woodland", "agriculture"))  # Land use factor variable
)

# Check summary statistics
summary(df$SR)
summary(df$TP)
table(df$land_use)

# Check the relationship between SR and TP by land use
plot(df$SR, df$TP, xlab = "Species Richness", ylab = "Total Phosphorus", main = "Relationship Between SR and TP by Land Use", col = as.numeric(df$land_use))
legend("topright", legend = levels(df$land_use), col = 1:3, pch = 1)

boxplot(SR ~ land_use, data = df)
boxplot(TP ~ land_use, data = df)

glm <- glm(SR~TP+land_use+land_use*TP,data=df, family=poisson)
summary(glm)
library(car)
vif(glm)

# remove interaction term
glm <- glm(SR~TP+land_use,data=df, family=poisson)
summary(glm)
vif(glm)

library(performance)
check_overdispersion(glm)

library(ggfortify)
autoplot(glm)

write.csv(df,"Macrophyte_lake2024.csv")  

