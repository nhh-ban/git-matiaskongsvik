# Assignment 5 BAN400 ####



#
# Problem 2 ------
  # read this data to a tidy data frame in R, without
  # changing the raw data file and without hard coding line numbers.
#
# Shortcuts for folding sections -----

# # Edit -> Folding:

# # Expand — Cmd+Shift+Option+L

# # Collapse — Cmd+Option+L

# # Collapse All — Cmd+Option+O


# # Expand All — Cmd+Shift+Option+O
# 
# # Code:
# # Insert Section — Cmd+Shift+R
# # Jump To — Cmd+Shift+Option+J

#
## Load necessary libraries ------------------------------------------------
library(tidyverse)

## Read raw file into memory -----------------------------------------------
raw_file <- readLines(con = "suites_dw_Table1.txt", warn = FALSE)

### View raw file ---------
raw_file

### Identifying separator line ----------------------------------------------

L <- 
  (substr(x = raw_file, start = 1, stop = 2) == "--") %>%
  which() %>% 
  min()

cat(raw_file[1:(L-2)], sep = "\n", file = "variable_descriptions.csv")
var_des <- readLines("variable_descriptions.csv")


### View variable descriptions ---------
var_des


#
## Extract variable names --------------------------------------------------
variable_names <- 
  str_split(string = raw_file[1:(L-1)], pattern = "\\|") %>%
  unlist() %>% 
  str_trim()

## returns a vectors with 24 values. 


variable_names <- variable_names[!grepl("[-;+]", variable_names) & grepl("^\\w+$", variable_names)]

### View variable names -------
variable_names
# Let´s only include the variable names, which totals 12 variable in our case, but should be filtered
# on entries that don´t contain hyphens, semicolons and +- signs. In other words, only word characters.
#--------------------------------------------------------------------------------------#
# Worth noting: Variable "vlg" is present in the table, but not in the 
# L-2 first lines of variable descriptions. "delta_vlg", on the other hand, is present in both. Strange.



#
## Create data frame with data entries ##------------------------------------

### Data entries in table separated by comma ---------
comma_separated_values <- 
  raw_file[L+1:(length(raw_file)-L)] %>% 
  gsub("\\|", ",", .) %>% 
  gsub(" ", "", .)



#### View vector with comma separated values ---------
comma_separated_values

### Values linked with variable names

comma_separated_values_with_names <- 
  c(paste(variable_names, collapse = ","),
    comma_separated_values)

#### View values linked with variable names ------
comma_separated_values_with_names


### Links the variable names on each data entry separated by comma and create .csv --------------

cat(comma_separated_values_with_names, sep = "\n", file = "galaxies.csv")





#
## Final data table of galaxies --------------------------------------------

galaxies <- read_csv("galaxies.csv", show_col_types = FALSE)












#
# Problem 3 ======

# The authors of the papers referenced above claim that their galaxy catalog is 
# approximately complete because it is a representative sample of a particular 
# volume in space (the ball of radius 11 megaparsecs centered on you and me). 
# There are, however, some signs that the smaller objects are under-represented in the sample.
# Can you make a plot that reveals this tendency and a likely explanation?
#
## Load necessary packages -------
library(ggplot2)





#
## Histogram and scatterplot-------
histogram <- ggplot(galaxies, aes(x = a_26)) + 
  geom_histogram(binwidth = 0.8, fill = "red", alpha = 0.9) + 
  labs(title = "Distribution of Galaxies (by size)", 
       x = "Size", 
       y = "Number of galaxies") +
  scale_x_continuous(breaks = seq(0, 70, by = 5), minor_breaks = seq(0, 70, by = 1)) +
  scale_y_continuous(breaks = seq(0, 170, by = 25)) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_line(color = "grey50", linetype = 1, size = 0.2),
    panel.grid.minor = element_line(color = "grey", linetype = "dotted", size = 0.5)
  )
histogram


scatterplot_distance <- ggplot(galaxies, aes(x = a_26, y = D, color = m_b)) + 
  geom_point(alpha = 0.5) +
  geom_smooth(formula = y ~ x, method = loess) +
  guides(color = guide_legend(override.aes = list(linetype = 1)), fill = TRUE) +
  labs(title = "Scatterplot of Size and Distance", 
       x = "Size (kpc)", 
       y = "Distance (Mpc)") +
  scale_x_continuous(breaks = seq(0, 60, by = 5)) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.minor = element_line(color = "grey", linetype = "dotted", size = 0.5)
  )

scatterplot_distance

### Explanation ------
# Histogram display the distribution of galaxies by size, in which the majority of galaxies are distributed at
# the lower range of sizes. Somewhat surprisingly, there are relatively few of the smallest galaxies in the first bin,
# compared to the subsequent 6 bins. From looking at the histogram alone, it could be inferred that the smallest
# galaxies of size ~ 1 kpc are significantly less numerous than galaxies of size ~2-5 kpc. Galaxies larger than 5 kpc
# are decreasing in size, e.g., increasingly rare. However, it cannot be inferred from the histogram alone that
# smaller objects are under-represented in the sample, and not displaying the natural distribution of galaxies´ size.
# 
# Looking at the scatterplot, the relationship between size and distance to galaxy is displayed. There are
# no obvious signs that our detection methods are less effective for smaller galaxies that are farther away.
# What we do see is that the smoother line using the default LOESS method to fit a smooth curve does not include the smallest galaxies,
# which could indicate that smaller galaxies might be underrepresented in the dataset.
# However, it might also indicating a bias in the dataset towards larger galaxies. Using the lm-method fixes this - displaying the
# line using linear regression method.
