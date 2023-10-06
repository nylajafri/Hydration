led_x <- 	0.0477  # Replace with actual x-coordinate of the LED light
led_y <- 0.0106 # Replace with actual y-coordinate of the LED light
# to get angles using the LED reference point. angles will include negative values
fed_angle_7 <- Hydra.Vectors...7.F %>%
  mutate(
    reference_vector_x = led_x - x1,
    reference_vector_y = led_y - y1,
    angle_radians_1 = atan2(Hydra.Vectors...7.F$y1, Hydra.Vectors...7.F$x1) - atan2(reference_vector_y, reference_vector_x),
    angle_degrees_1 = angle_radians_1 * (180 / pi),
    
    reference_vector_x2 = led_x - x2,
    reference_vector_y2 = led_y - y2,
    angle_radians_2 = atan2(Hydra.Vectors...7.F$y2, Hydra.Vectors...7.F$x2) - atan2(reference_vector_y2, reference_vector_x2),
    angle_degrees_2 = angle_radians_2 * (180 / pi),
    
    reference_vector_x3 = led_x - x3,
    reference_vector_y3 = led_y - y3,
    angle_radians_3 = atan2(Hydra.Vectors...7.F$y3, Hydra.Vectors...7.F$x3) - atan2(reference_vector_y3, reference_vector_x3),
    angle_degrees_3 = angle_radians_3 * (180 / pi),
    
    reference_vector_x4 = led_x - x4,
    reference_vector_y4 = led_y - y4,
    angle_radians_4 = atan2(Hydra.Vectors...7.F$y4, Hydra.Vectors...7.F$x4) - atan2(reference_vector_y4, reference_vector_x4),
    angle_degrees_4 = angle_radians_4 * (180 / pi),
    
    reference_vector_x5 = led_x - x5,
    reference_vector_y5 = led_y - y5,
    angle_radians_5 = atan2(Hydra.Vectors...7.F$y5, Hydra.Vectors...7.F$x5) - atan2(reference_vector_y5, reference_vector_x5),
    angle_degrees_5 = angle_radians_5 * (180 / pi),
    
    
  )







led_x <- 	0.0812  # Replace with actual x-coordinate of the LED light
led_y <-  0.0398 # Replace with actual y-coordinate of the LED light
# to get angles using the LED reference point. angles will include negative values
Starve_angle_7 <- Hydra.Vectors...7.S%>%
  mutate(
    reference_vector_x = led_x - x1,
    reference_vector_y = led_y - y1,
    angle_radians_1 = atan2(Hydra.Vectors...7.S$y1, Hydra.Vectors...7.S$x1) - atan2(reference_vector_y, reference_vector_x),
    angle_degrees_1 = angle_radians_1 * (180 / pi),
    
    reference_vector_x2 = led_x - x2,
    reference_vector_y2 = led_y - y2,
    angle_radians_2 = atan2(Hydra.Vectors...7.S$y2, Hydra.Vectors...7.S$x2) - atan2(reference_vector_y2, reference_vector_x2),
    angle_degrees_2 = angle_radians_2 * (180 / pi),
    
    reference_vector_x3 = led_x - x3,
    reference_vector_y3 = led_y - y3,
    angle_radians_3 = atan2(Hydra.Vectors...7.S$y3, Hydra.Vectors...7.S$x3) - atan2(reference_vector_y3, reference_vector_x3),
    angle_degrees_3 = angle_radians_3 * (180 / pi),
    
    reference_vector_x4 = led_x - x4,
    reference_vector_y4 = led_y - y4,
    angle_radians_4 = atan2(Hydra.Vectors...7.S$y4, Hydra.Vectors...7.S$x4) - atan2(reference_vector_y4, reference_vector_x4),
    angle_degrees_4 = angle_radians_4 * (180 / pi),
    
    reference_vector_x5 = led_x - x5,
    reference_vector_y5 = led_y - y5,
    angle_radians_5 = atan2(Hydra.Vectors...7.S$y5, Hydra.Vectors...7.S$x5) - atan2(reference_vector_y5, reference_vector_x5),
    angle_degrees_5 = angle_radians_5 * (180 / pi),
    
    
  )
fed7_combined_angles <- c(
  fed_angle_7$angle_degrees_1,
  fed_angle_7$angle_degrees_2,
  fed_angle_7$angle_degrees_3,
  fed_angle_7$angle_degrees_4,
  fed_angle_7$angle_degrees_5
)

starved7_combined_angles <- c(
  Starve_angle_7$angle_degrees_1,
  Starve_angle_7$angle_degrees_2,
  Starve_angle_7$angle_degrees_3,
  Starve_angle_7$angle_degrees_4,
  Starve_angle_7$angle_degrees_5
)


ggplot(data.frame(Group = c(rep("Fed", length(fed7_combined_angles)), rep("Starved", length(starved7_combined_angles))),
                  Angle = c(fed7_combined_angles, starved7_combined_angles)), 
       aes(x = Group, y = Angle, fill = Group)) +
  geom_violin() +
  xlab("Group") +
  ylab("Angle (degrees)") +
  ggtitle("Distribution of Angles (Fed vs. Starved 7 Days)")

# Load the 'circular' package if not already loaded
library(circular)

# Define the number of bins for your rose diagram
num_bins <- 12  # You can adjust this as needed

# Create a function to plot a rose diagram with angle labels
plot_rose_with_labels <- function(angles, title) {
  # Convert angle data to circular data objects
  circular_angles <- circular(angles, units = "degrees")
  
  # Create the rose diagram
  rose_diagram <- rose.diag(circular_angles, bins = num_bins, main = title, col = "lightblue")
  
  # Add custom angle labels
  angle_labels <- seq(0, 360, by = 360 / num_bins)
  text(rose_diagram$xy, labels = angle_labels, col = "black", pos = 3)
}

# Create rose diagrams for each starved day
plot_rose_with_labels(starved7_combined_angles, "Starved Day 7")
plot_rose_with_labels(starved8_combined_angles, "Starved Day 8")
plot_rose_with_labels(starved14_combined_angles, "Starved Day 14")


plot_rose_with_labels(fed7_combined_angles, "Fed That Day")
plot_rose_with_labels(fed8_combined_angles, "Fed 1 Day Ago")
plot_rose_with_labels(fed14_combined_angles, "Fed That Day")






# Load the ggplot2 library if you haven't already
library(ggplot2)

# Combine angle data for all hydra within the fed and starved groups
fed_combined_angles <- c(
  fed_angle_7$angle_degrees_1,
  fed_angle_7$angle_degrees_2,
  fed_angle_7$angle_degrees_3,
  fed_angle_7$angle_degrees_4,
  fed_angle_7$angle_degrees_5,
  fed_angle_8$angle_degrees_1,
  fed_angle_8$angle_degrees_2,
  fed_angle_8$angle_degrees_3,
  fed_angle_8$angle_degrees_4,
  fed_angle_8$angle_degrees_5,
  fed_angle_14$angle_degrees_1,
  fed_angle_14$angle_degrees_2,
  fed_angle_14$angle_degrees_3,
  fed_angle_14$angle_degrees_4,
  fed_angle_14$angle_degrees_5
)

starved_combined_angles <- c(
  Starve_angle_7$angle_degrees_1,
  Starve_angle_7$angle_degrees_2,
  Starve_angle_7$angle_degrees_3,
  Starve_angle_7$angle_degrees_4,
  Starve_angle_7$angle_degrees_5,
  starve_angle_8$angle_degrees_1,
  starve_angle_8$angle_degrees_2,
  starve_angle_8$angle_degrees_3,
  starve_angle_8$angle_degrees_4,
  starve_angle_8$angle_degrees_5,
  starve_angle_14$angle_degrees_1,
  starve_angle_14$angle_degrees_2,
  starve_angle_14$angle_degrees_3,
  starve_angle_14$angle_degrees_4,
  starve_angle_14$angle_degrees_5
)

# Create violin plots for the combined angle data
ggplot(data.frame(Group = c(rep("Fed", length(fed_combined_angles)), rep("Starved", length(starved_combined_angles))),
                  Angle = c(fed_combined_angles, starved_combined_angles)), 
       aes(x = Group, y = Angle, fill = Group)) +
  geom_violin() +
  xlab("Group") +
  ylab("Angle (degrees)") +
  ggtitle("Distribution of Angles (Fed vs. Starved)")





# Define the minimum sample size
min_sample_size <- min(length(fed_combined_angles), length(starved_combined_angles))

# Create temporary variables for equalized samples
equalized_fed_angles <- sample(fed_combined_angles, size = min_sample_size)
equalized_starved_angles <- sample(starved_combined_angles, size = min_sample_size)

# Perform Mann-Whitney U test without modifying the original dataframe
result <- wilcox_test(equalized_fed_angles ~ equalized_starved_angles)
print(result)



# Calculate the Mann-Whitney U test for equalized samples
result <- wilcox.test(equalized_fed_angles, equalized_starved_angles)
print(result)





