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

# optional(SPC) %R% or("YO", "YR", "MO")
# str_extract(time_units,WRD)

#capture 
# Capture part between @ and . and after .
email <- capture(one_or_more(WRD)) %R% 
  capture("@" %R% one_or_more(WRD) %R% 
            DOT) %R% one_or_more(WRD)

# Check match hasn't changed
str_view(hero_contacts,email)

# Pull out match and captures -match will split the 2 catches
email_parts <- str_match(hero_contacts,email)

#True email capture
# (?:[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*|"(?:[\x01-\x08\x0b\x0c\x0e-\x1f\x21\x23-\x5b\x5d-\x7f]|\\[\x01-\x09\x0b\x0c\x0e-\x7f])*")@
# (?:(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?|\[(?:(?:(2(5[0-5]|[0-4][0-9])|1[0-9][0-9]|[1-9]?[0-9]))\.){3}(?:(2(5[0-5]|[0-4][0-9])|1[0-9][0-9]|[1-9]?[0-9])|[a-z0-9-]*
# [a-z0-9]:(?:[\x01-\x08\x0b\x0c\x0e-\x1f\x21-\x5a\x53-\x7f]|\\[\x01-\x09\x0b\x0c\x0e-\x7f])+)\])

# Add capture() to get digit parts
phone_pattern <- capture(three_digits) %R% zero_or_more(separator) %R% 
  capture(three_digits) %R% zero_or_more(separator) %R%
  capture(four_digits)

# Pull out the parts with str_match()
phone_numbers <- str_match(contact,phone_pattern)


#backreference###
# See names with three repeated letters
repeated_three_times <- capture(WRD) %R% REF1%R% REF1
str_view(boy_names, 
         pattern = repeated_three_times, 
         match = TRUE)

# See six letter palindrome names
six_letter_palindrome <- exactly(
  capture(LOWER) %R% capture(LOWER) %R% capture(LOWER) %R% 
    REF3 %R% REF2 %R% REF1)
str_view(boy_names, 
         pattern = six_letter_palindrome, 
         match = TRUE)


# Create vector of characters
characters <- c("Algernon", "Jack", "Lane", "Cecily", "Gwendolen", "Chasuble", 
                "Merriman", "Lady Bracknell", "Miss Prism")

# Match start, then character name, then .
pattern_3 <- START%R%or1(characters)%R%DOT

# View matches of pattern_3
str_view(play_lines, pattern = pattern_3, 
         match = T) 

# View non-matches of pattern_3
str_view(play_lines, pattern = pattern_3, 
         match = F)

# Pull out matches
lines <- str_subset(play_lines, pattern = pattern_3)

# Extract match from lines
who <-  str_extract(lines, pattern = pattern_3)
unique(who)
# Count lines per character
table(who)

#cases from case study####
trip_pattern <- regex("TRIP", ignore_case = T)

writeLines(str_to_title(cat5))

# Transform to title case with stringi
writeLines( stri_trans_totitle(cat5))

# Transform to sentence case with stringi
writeLines(stri_trans_totitle(cat5, type = "sentence"))