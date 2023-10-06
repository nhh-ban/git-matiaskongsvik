# Assignment 5 BAN400 ####



# Problem 2 ------
  # read this data to a tidy data frame in R, without
  # changing the raw data file and without hard coding line numbers.
# Shortcuts for folding sections -----

# # Edit -> Folding:
# 
# # Collapse — Cmd+Option+L
# 
# # Expand — Cmd+Shift+Option+L
# 
# # Collapse All — Cmd+Option+O
# 
# # Expand All — Cmd+Shift+Option+O
# 
# 
# # Code:
# 
# # Insert Section — Cmd+Shift+R
# 
# # Jump To — Cmd+Shift+Option+J

## Load necessary libraries ------------------------------------------------
library(tidyverse)

## Read raw file into memory -----------------------------------------------
raw_file <- readLines(con = "suites_dw_Table1.txt", warn = FALSE)

### View raw file ---------
raw_file

## Identifying separator line ----------------------------------------------

L <- 
  (substr(x = raw_file, start = 1, stop = 2) == "--") %>%
  which() %>% 
  min()

cat(raw_file[1:(L-2)], sep = "\n", file = "variable_descriptions.csv")
var_des <- readLines("variable_descriptions.csv")


### View variable descriptions ---------
var_des


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



## Create a .csv file with data entries ##------------------------------------

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





## Final data table of galaxies --------------------------------------------

galaxies <- read_csv("galaxies.csv", show_col_types = FALSE)












# Problem 3 ======
# The authors of the papers referenced above claim that their galaxy catalog is 
# approximately complete because it is a representative sample of a particular 
# volume in space (the ball of radius 11 megaparsecs centered on you and me). 
# There are, however, some signs that the smaller objects are under-represented in the sample.
# Can you make a plot that reveals this tendency and a likely explanation?




##

## Load necessary packages -------
library(ggplot2)




