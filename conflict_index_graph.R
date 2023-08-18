# Assuming you've loaded the necessary libraries
library(ggplot2)

# Scatter plot
p <- ggplot(trips, aes(x = total_bikers_normalized, y = total_hikers_normalized)) +
  geom_point(aes(color = conflict_index), alpha = 0.6, size = 3) +
  scale_color_gradient(low = "blue", high = "red") +
  geom_vline(xintercept = 0.5, linetype = "dashed") +  # Midpoint line for x-axis
  geom_hline(yintercept = 0.5, linetype = "dashed") +  # Midpoint line for y-axis
  labs(
    title = "Conflict Index Distribution based on Bikers and Hikers",
    x = "Normalized Biker Trips",
    y = "Normalized Hiker Trips",
    color = "Conflict Index"
  ) +
  theme_minimal()

print(p)
