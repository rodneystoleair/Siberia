# Loading functions ---- 

# remove_na ----
# written by : Copilot
# purpose    : a function that removes all NA columns and NA rows from the first
#              ones in a DF

remove_na = function(df) {
  first_all_na = which(rowSums(is.na(df)) == ncol(df))[1]
  if (!is.na(first_all_na)) {
    df = df[1:(first_all_na - 1), ]  # Keep rows before the first all NA row
  }
  df = df[rowSums(is.na(df)) < ncol(df), ]  # Remove rows with all NA
  df = df[, colSums(is.na(df)) < nrow(df)]  # Remove columns with all NA
  return(df)
}
