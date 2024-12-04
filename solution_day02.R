input_url = "https://raw.githubusercontent.com/juansanar/advent_of_code/refs/heads/main/input_day2.txt"

input = readr::read_lines(input_url)

##### Challenge 01 ####
a <-lapply(unlist(lapply(input, function(x) strsplit(x, " ")), recursive = FALSE), function(x) as.integer(x))

# Define the custom function to check if a report is safe
is_safe_report <- function(levels) {
  # Convert the levels to a numeric vector in case they are provided as a list
  levels <- unlist(levels)
  # Calculate the differences between adjacent levels
  diffs <- diff(levels)
  # Check that there are no zero differences (levels must be strictly increasing or decreasing)
  if (any(diffs == 0)) {
    return(FALSE)
  }
  # Determine if the sequence is strictly increasing or decreasing
  if (all(diffs > 0)) {
    direction <- "increasing"
  } else if (all(diffs < 0)) {
    direction <- "decreasing"
  } else {
    # The sequence changes direction, so it's unsafe
    return(FALSE)
  }
  # Check that all differences are at least 1 and at most 3
  if (all(abs(diffs) >= 1 & abs(diffs) <= 3)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

results <- sapply(a, is_safe_report)
sum(results)

##### Challenge 02 ####
