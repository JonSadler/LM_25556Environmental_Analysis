# Create the summary data frame with ordered factors 
Rye_summary <- Rye %>%
  # Convert 'Grazing' to a factor with a specific order - needed so the size classes are displayed properly on the X axis. Otherwise, R will order them alphanumerically as High, Low, Mid, which does not make sense!
  mutate(Grazing = factor(Grazing, levels = c("Low", "Mid", "High"))) %>%
  
  # Group by both categorical variables
  group_by(Grazing, Field) %>%
  
  # Calculate the mean, count, standard deviation, and standard error
  summarise(
    n = n(),
    mean_abund = mean(Abund),
    sd_abund = sd(Abund),
    sem = sd_abund / sqrt(n),
    .groups = 'drop' # Recommended to drop grouping after summarise
  ) %>%
  
  # Calculate the 95% confidence interval boundaries
  mutate(
    ic = sem * 1.96, # Interval of confidence
    upper_ci = mean_abund + ic,
    lower_ci = mean_abund - ic
  )


# Create the interaction plot 

ggplot(Rye_summary, aes(x = Grazing, y = mean_abund, group = Field, color = Field)) +
  
  # Add the error bars 
  # This geom will draw the vertical lines for the confidence intervals
  geom_errorbar(
    aes(ymin = lower_ci, ymax = upper_ci),
    width = 0.1,  # Width of the horizontal caps on the error bars
    linewidth = 0.8 # thickness of the line
  ) +
  
  # Add the lines connecting the means 
  geom_line(linewidth = 1.2) +
  
  # Add the points for the means
  geom_point(size = 4) +
  
  # Add labels, colors, and the BW theme
  labs(
    title = "Interaction of Grazing and Field Position on Mean Abundance",
    subtitle = "Error bars represent 95% confidence intervals",
    x = "Grazing Level",
    y = "Mean Abundance",
    color = "Field Position"
  ) +
  scale_color_brewer(palette = "Set1") +
  theme_bw(base_size = 14)