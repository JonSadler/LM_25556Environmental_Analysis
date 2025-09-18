# Contrasts plots for the Fertilizer example dataset
# Check week for the code that generates is

# --- (Previous code to calculate fertilizer_diffs and mean_diffs remains the same) ---

# --- Create the Horizontal Treatment Effects Plot with Manual Axis Limits ---

ggplot() +
  
  # The vertical "Control" line at x = 0
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.8) +
  
  # The raw data points (as differences)
  geom_jitter(
    data = fertilizer_diffs,
    aes(x = diff_from_control, y = group, color = group),
    height = 0.2,
    alpha = 0.5,
    size = 2.5
  ) +
  
  # The bolded mean difference points
  geom_point(
    data = mean_diffs,
    aes(x = mean_diff, y = group, fill = group),
    size = 5,
    shape = 21,
    color = "black",
    stroke = 0.5
  ) +
  
  # --- THE FIX IS HERE ---
  # Manually set the limits of the x-axis to ensure the negative portion is visible.
  # Adjust these values to best fit the range of your data.
  scale_x_continuous(limits = c(-15, 25)) + 
  
  # Aesthetics and Labels
  scale_color_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Dark2") +
  
  labs(
    title = "Treatment Effects of Fertilizers Compared to Control",
    subtitle = "Difference between each plant's height and the control group's average height",
    x = "Difference from Control Group Mean (cm)",
    y = "Treatment Group"
  ) +
  
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank()
  )
