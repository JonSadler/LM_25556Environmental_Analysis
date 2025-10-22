# Script for random intercept fix for Rikz data
We see an issue with the variances on the 'fExp' factor (top left); everything else is fine. Let's try and see why. We'll use boxplots, scatterplots and the `table()` function to explore the issue.

```{r}
# boxplot of the errant factor fExp
ggplot(Benthic, aes(fExp, Richness)) +
  geom_boxplot(fill = "lightblue", outlier.shape = NA) +
  geom_point(alpha = 0.5) +
  theme_bw()
```

We can see an issue here. There is a much greater spread in level 10 that 11 and fewer values in 11 too. The `table()` function can help here.

```{r}
table(Benthic$fExp, Benthic$Richness)
```

Here we see the likely culprit. The rows on top are the unique values for species richness and the first column is the factor fExp. The number in the cells (or rows) are the number of cases of counts of each species richness level. You can see a lot of zeros in the level (11). Most of the counts are species richness levels of 6 or below. So in effect the counts for level 11 are heavily zero inflated. This is what is causing the problem.

There might also be interactions between 'fExp' and other variables in the model notable 'NAP'. We can visualise this with a scatterplot.

```{r}
ggplot(Benthic, aes(x = NAP, y = Richness, colour = fExp)) + # colour plots lines for each level of the fExp factor.
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = FALSE, formula = 'y ~ x', linewidth = 1.1) +
  labs(
    x = "NAP (continuous variable)",
    y = "Species richness",
    colour = "Exposure level"
  ) +
  theme_classic()
```

We can see the lines converge indicating an interaction between the variables. Species richness declines along the NAP variable are much greater at exposure level 10 than 11.

This is a complex fix because glmmTMB's more esoteric options for different error structure, i.e. Negative Binomial (`family =nbinom2`), Conway-Maxwell-Poisson (`family = compois`), Zeroinflated and Hurdle models operate on the response variable, not subsets of it. The only route we have is to use the `dispformula()` function which allows us to model the dispersion parameter directly. The rub is that it doesn't work with a straight Poisson model because these do not have a dispersion parameter; variance is meant to be equal to the mean. This is why Poisson models can be over or under dispersed (Table 9.1).

**Table 9.1: Causes of overdispersion**
  
  | Case                | Condition   | Explanation                        | Possible causes                                                                           |
  |-------------|-------------|-------------|----------------------------------|
  | **Equidispersion**  | Var(Y) = μ  | Ideal Poisson behaviour            | Random, independent counts                                                                |
  | **Overdispersion**  | Var(Y) \> μ | More variable than Poisson expects | Unmodelled heterogeneity, clustering, zero-inflation, temporal/spatial dependence         |
  | **Underdispersion** | Var(Y) \< μ | Less variable than Poisson expects | Constrained counts, regular spacing, or strong limiting processes (e.g. extreme toxicity) |
  
  We need to use one of the other families to effect a fix. You already know that the dispersion test was not significant, but the parameter was 1.26, tending towards overdispersion. Our suspicion is that our fExp factor is the cause of that. Look at the table above. We have unmodelled heterogeneity so use Conway-Maxwell-Poisson (`family = compois`) as it also flexible enough to deal with over- and under-dispersion. We combine this with a component that models the dispersion term directly. Our exploration of 'fExp' leads us to believe it might interact with 'NAP'. This makes sense as both are indirectly related beach geomorphology. We add an interaction term into the model - `dispformula = ~NAP * fExp`. This is akin to running an ANCOVA directly on 'fExp' factor allowing different slopes for each factor level. Our new model looks like this.

```{r}
compois_M <- glmmTMB(Richness ~ fExp + humus + NAP  + humus: NAP + (1 | fbeach), data = Benthic, family = compois, dispformula = ~NAP * fExp)
```

Well go through most of the validation steps but only show the residual spreads for 'fExp'. First we simulate the residuals and run the standard plots.

```{r}
# Create the DHARMa simulation object
simulation_output <- simulateResiduals(fittedModel = compois_M)
# This one plot checks for multiple issues at once. Notice is the same as the base R function call
plot(simulation_output, rank=FALSE) # we have turned off the ranking here so we see the true patterns of the residuals
```

Our dispersion test is still not significant but it would be interesting to know what the parameter is; and it is a little smaller at 1.1624.

```{r}
testDispersion(simulation_output)
```

Test for outliers. None found.

```{r}
testOutliers(simulation_output)
```

Now we just need to confirm that the `dispformula = ~NAP * fExp` has actually sorted out the heterogeneity in the 'fExp' variable (Fig. ). **If you have time add in the other covariates by using the code provided above**. There are no problems so we can go forward and interpret the model and create some supporting pictures.

```{r}
plotResiduals(simulation_output, form = Benthic$fExp, rank=FALSE)
```

**Fig: The residual spreads for the fExp factor**
  Ri