# install.packages("rebus")
library(stringr)

# format(income, digits = 2)
# format(income, digits = 2, big.mark = ",")
# format(p_values, scientific = FALSE)
# paste("$", pretty_income, sep = "")
# paste(pretty_percent, "%", sep = "")
# # Create vector with elements like 2010: +4.0%`
# year_percent <- paste(years, ": ", pretty_percent, "%", sep = "")
# # Collapse all years into single string
# paste(year_percent, collapse = ", ")
#can use last
str_sub(girl_names, -1, -1)
# detect - logical vector for subsetting
#sub the atual strings
#str_count - occurances
library(rebus)
str_view(x, pattern =START %R%  "cat"%R%  END)

# Match with alternate endings

vowels <- char_class("aeiouAEIOU")

by_parts <- or("Je", "Geo") %R% "ff" %R% or("ry", "ery", "rey", "erey")
str_view(boy_names, 
         pattern = by_parts, 
         match = TRUE)