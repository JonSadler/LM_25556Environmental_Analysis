# permutation ANOVA example

# agricultural experiment testing the Yield of a crop based on two factors:
# Fertilizer: Two types (A and B).
# SoilType: Two types (Sandy and Clay).

# --- Install packages if you don't have them ---
# install.packages("lmPerm")
# install.packages("dplyr")
# install.packages("ggplot2")

# --- Load libraries ---
library(lmPerm)
library(dplyr)
library(ggplot2)

# --- Load libraries ---
library(dplyr)
library(ggplot2)

# --- Create a reproducible, NON-NORMAL dataset (CORRECTED) ---
set.seed(42)

n_per_group <- 30

# --- STEP 1: Create the 'blueprint' of the experiment first ---
# This creates a data frame with all 120 combinations of your factors.
crop_data <- tibble(
  Fertilizer = factor(rep(c("A", "B"), each = n_per_group * 2)),
  SoilType = factor(rep(c("Sandy", "Clay", "Sandy", "Clay"), each = n_per_group))
)

# --- STEP 2: Now, use mutate() and case_when() to add the Yield column ---
# This approach is much safer and clearer.
crop_data <- crop_data |>
  mutate(
    Yield = case_when(
      Fertilizer == "A" & SoilType == "Sandy" ~ rgamma(n(), shape = 3, rate = 0.5),
      Fertilizer == "A" & SoilType == "Clay"  ~ rgamma(n(), shape = 5, rate = 0.5),
      Fertilizer == "B" & SoilType == "Sandy" ~ rgamma(n(), shape = 6, rate = 0.5),
      Fertilizer == "B" & SoilType == "Clay"  ~ rgamma(n(), shape = 9, rate = 0.5)
    )
  )

# --- Visualize the data to see the skewness ---
ggplot(crop_data, aes(x = Fertilizer, y = Yield, fill = SoilType)) +
  geom_boxplot() +
  labs(title = "Crop Yield (Skewed Data)") +
  theme_minimal()


# --- 1. Run the Permutation ANOVA ---
# The aovp() function uses the same formula as aov().
# It will automatically perform thousands of permutations.
perm_model <- aovp(Yield ~ Fertilizer * SoilType, data = crop_data)

# --- 2. View and Interpret the Results ---
summary(perm_model)
