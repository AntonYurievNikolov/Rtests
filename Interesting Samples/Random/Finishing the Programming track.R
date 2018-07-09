lapply(logs, `[[`, "timestamp")
#safe version
vapply(logs, `[[`, "success",FUN.VALUE =logical(1))

# Return vector with uppercase version of message elements in log entries
extract_caps <- function(x) {
  toupper(x$details$message)
}

vapply(logs, extract_caps, FUN.VALUE = character(1))

# vapply matrixmatrix
pass_names <- titanic$Name
titles <- paste(",", c("Mr\\.", "Master", "Don", "Rev", "Dr\\.", "Major", "Sir", "Col", "Capt", "Jonkheer"))

# Finish the vapply() command
hits <- vapply(titles,
               FUN = grepl,
               FUN.VALUE = logical(length(pass_names)),
               pass_names)

# Calculate the sum() of hits
sum(hits)

# Count number of men based on gender
sum(titanic$Sex == "male")