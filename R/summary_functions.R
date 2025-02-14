# Summary functions ----

# summary_to_table ----
# written by : Rodion Andreev
# purpose    : convert raw summary lists into a data frame
# params     : summary -- a generated summary list for each method

summary_to_table = function(summary){
  table = summary |> 
    lapply(as.data.frame) |> 
    bind_rows() |> 
    mutate(variable = names(summary_rf)) |> 
    relocate(variable)
  return(table)
}
