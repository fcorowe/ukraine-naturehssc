# Load required libraries
library(kableExtra)
library(tidyverse)

# Create the data frame
population_displacement <- data.frame(
  Month = c("February", "March", "April", "May", "June", "July", "August"),
  "Median Displacement" = format(c(3436536, 6191498, 7389048, 7526610, 7234083, 6930409, 9273980), big.mark = ","),
  "IOM Mean Estimate" = format(c(NA, 6478000, 7423000, 7581500, 6275000, 6645000, 6975000), big.mark = ","),
  Difference = format(c(NA, -286502, -33952, -54890, 959083, 285409, 2298980), big.mark = ",")
)

names(population_displacement) <- c("Month", "Median Displacement", "IOM Mean Estimate", "Difference")

# Print the table with kable
kbl(booktabs = TRUE, population_displacement, align = "c") %>%
  kable_styling(full_width = FALSE, font_size = 17) %>%
  row_spec(0, bold = TRUE) %>%
  kable_styling(full_width = F, position = "left") %>% 
  save_kable("/Users/franciscorowe/Dropbox/Francisco/Research/in_progress/ukraine-displacement/github/ukraine-pop-displacement/outputs/sm/tables/table_all_estimates.png", density = 600)


