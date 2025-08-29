# Excellent hydrology plot code

# Data is:
# Load data
data <- read_csv("~/Documents/GitHub/Teaching/LM_25556Environmental_Analysis/Data/Riv_hydrol.csv")

ggplot(data, aes(x = Ran, y = Mean, size = Carea)) +
  geom_point(
    shape = 21, # shape 21 is a circle but we are going to remove the default theme so we specify it.
    colour = "grey10", # change the outline of the points to light grey 
    fill = "#4682B4", # change the point colour to light blue
    alpha = 0.7 # Modify the opacity to make it more translucent
  ) +
  scale_size_area(max_size = 12) +
  
  # Set the axis labels here for clarity
  scale_x_continuous(name = expression(paste("Mean Daily Discharge ", "(", m^3, "/",s^-1,")"))) +
  scale_y_continuous(name = expression(paste("Mean Daily Discharge ", "(", m^3, "/",s^-1,")"))) +
  
  # Use coord_cartesian to set the viewport, start axes at 0, and turn off clipping.
  coord_cartesian(
    xlim = c(0, 600),
    ylim = c(0, 60),
    expand = FALSE,  # This creates the tight axis look, starting at 0
    clip = "off"     # This allows points to be drawn outside the panel
  ) +
  
  # Add the labels. Note expression(paste) functions to add the superscript
  labs(
    title = "River Discharge Increases with Catchment Area",
    subtitle = "A comparison of mean daily discharge across various rivers",
    size = expression(paste("Catchment Area ", "(",km^2,")")),
    caption = "Data points represent individual rivers. Bubble size is proportional to catchment area."
  ) +
  theme_bw(base_size = 14) + # change the theme from default to black and white. Set base font size to 14
  theme(
    panel.grid.minor = element_blank(), #remove the minor grids
    panel.border = element_blank(), # remove the plot border
    axis.line = element_line(colour = "grey50"), #Change axis line colour to grey
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0, color = "grey40"),
    legend.position = "top",
    # Add a little margin to the whole plot to ensure there's room
    plot.margin = margin(10, 15, 10, 10) # Top, Right, Bottom, Left
  )