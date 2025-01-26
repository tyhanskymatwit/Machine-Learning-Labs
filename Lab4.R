library(ggplot2)

# Read the data
data <- read.csv("data_DTR.csv")

# Function to create the plot
create_tree_plot <- function(data, split_value, title) {
  ggplot(data, aes(x = x1, y = x2)) +
    geom_point(aes(color = y), alpha = 0.6) +
    geom_vline(xintercept = split_value, linetype = "dashed", color = "red") +
    geom_hline(yintercept = split_value, linetype = "dashed", color = "red") +
    scale_color_gradient(low = "blue", high = "red") +
    annotate("text", x = split_value + 0.1, y = max(data$x2), label = paste("x1 =", split_value), hjust = 0) +
    annotate("text", x = max(data$x1), y = split_value + 0.1, label = paste("x2 =", split_value), vjust = 0) +
    annotate("text", x = mean(data$x1[data$x1 >= split_value]), 
             y = mean(data$x2[data$x1 >= split_value]), label = "r1") +
    annotate("text", x = mean(data$x1[data$x1 < split_value & data$x2 < split_value]), 
             y = mean(data$x2[data$x1 < split_value & data$x2 < split_value]), label = "r2") +
    annotate("text", x = mean(data$x1[data$x1 < split_value & data$x2 >= split_value]), 
             y = mean(data$x2[data$x1 < split_value & data$x2 >= split_value]), label = "ra") +
    labs(title = title,
         x = "x1",
         y = "x2") +
    theme_minimal()
}

# Create plots
plot1 <- create_tree_plot(data, 2.25, "Tree 1: Split at x1 = 2.25")
plot2 <- create_tree_plot(data, 2.0, "Tree 2: Split at x1 = 2.0")

# Display plots
print(plot1)
print(plot2)

# Optionally, save plots
ggsave("tree1_plot.png", plot1, width = 10, height = 8)
ggsave("tree2_plot.png", plot2, width = 10, height = 8)