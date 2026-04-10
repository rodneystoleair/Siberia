library(tidyverse)
library(stringdist)

# 1. Read the datasets
# Adjust the sheet names or ranges if necessary
modern = readxl::read_excel("data/modern/modern.xlsx")
core_name = readline('Write the exact core name for a reconstruction: ')

fossil = readxl::read_excel(paste0('data/fossil/original_fossil_',
                                   core_name, '.xlsx')) |> 
  gather(variable, value, -depth) |>
  spread(depth, value) |>
  transform(variable = as.numeric(variable)) |>
  arrange(variable)

if ('Pinus sylvestris' %in% colnames(fossil) || 
    'Pinus sibirica' %in% colnames(fossil)){
  fossil = fossil |> 
    mutate(Pinus = `Pinus sylvestris` + `Pinus sibirica`) |> 
    select(-`Pinus sylvestris`, -`Pinus sibirica`)
}

# 2. Extract taxa lists
# Modern: column names (assuming column 1 is an index/point name)
modern_taxa = colnames(modern)[-1]

# Fossil: first column contains taxa names
fossil_taxa = colnames(fossil)
fossil_taxa = fossil_taxa[!(fossil_taxa == 'variable')]

# Clean up whitespace that might interfere with matching
modern_taxa = trimws(modern_taxa)
fossil_taxa = trimws(fossil_taxa)

# 3. Exact Matching
exact_matches = intersect(fossil_taxa, modern_taxa)

# Identify taxa that didn't match exactly
unmatched_fossil = setdiff(fossil_taxa, modern_taxa)
unmatched_modern = setdiff(modern_taxa, fossil_taxa)

# 4. Fuzzy Matching (Close matches)
# We will use the Jaro-Winkler distance to find small spelling differences
close_matches = list()
threshold = 0.25 # Distance threshold (lower = stricter matching). Adjust if needed.

for (f_tax in unmatched_fossil) {
  # Calculate distance between the unmatched fossil taxa and all unmatched modern taxa
  dists = stringdist(f_tax, unmatched_modern, method = "jw")
  
  # Find the closest match
  min_dist = min(dists)
  if (min_dist < threshold) {
    best_match = unmatched_modern[which.min(dists)]
    close_matches[[f_tax]] = best_match
  }
}

# Combine exact and close matches
matched_fossil_cols <- c(exact_matches, names(close_matches))

# 5. Categorize the Taxa for Reporting
excluded_taxa = setdiff(fossil_taxa, matched_fossil_cols)
missing_taxa = setdiff(modern_taxa, c(exact_matches, unlist(close_matches)))

# 6. Print the Comparison Reports
cat("=========================================\n")
cat(sprintf("Total Modern Taxa: %d\n", length(modern_taxa)))
cat(sprintf("Total Fossil Taxa: %d\n", length(fossil_taxa)))
cat("=========================================\n\n")

cat("=== 1. EXACT MATCHES ===\n")
print(exact_matches)
cat("\n")

cat("=== 2. CLOSE MATCHES FOUND (Fossil -> Modern) ===\n")
if(length(close_matches) > 0) {
  for (name in names(close_matches)) {
    cat(sprintf("'%s' matched to '%s'\n", name, close_matches[[name]]))
  }
} else {
  cat("No close matches found.\n")
}
cat("\n")

cat("=== 3. EXCLUDED TAXA (In Fossil, but NOT in Modern) ===\n")
cat("These will be removed from the fossil dataset:\n")
print(excluded_taxa)
cat("\n")

cat("=== 4. MISSING TAXA (In Modern, but NOT in Fossil) ===\n")
cat("These are expected by the model but missing in the fossil record:\n")
print(missing_taxa)
cat("\n")

# 7. Create the Synchronized Fossil Dataset
# Filter rows to only keep those that matched (exactly or closely)
fossil_sync = fossil[, c("variable", matched_fossil_cols), drop = FALSE]

# Rename the close matches in the fossil dataset to exactly match the modern dataset
colnames(fossil_sync) <- sapply(colnames(fossil_sync), function(x) {
  if (x %in% names(close_matches)) close_matches[[x]] else x
})

# Ensure the final synchronized dataset is printed/saved
cat("=== SYNCHRONIZATION COMPLETE ===\n")
cat(sprintf("\nSynchronized fossil dataset: %d taxa columns\n",
            ncol(fossil_sync) - 1))

fossil_sync_full <- fossil_sync %>%
  bind_cols(
    setNames(
      as.data.frame(
        matrix(0,
               nrow = nrow(fossil_sync),
               ncol = length(missing_taxa))
      ),
      missing_taxa
    )
  ) %>%
  select(variable, all_of(modern_taxa))  # reorder columns to exactly match modern

# Optional: Save the synchronized dataset to a new Excel/CSV file
writexl::write_xlsx(fossil_sync_full, (paste0('data/fossil/fossil_',
                                              core_name, '.xlsx')))
