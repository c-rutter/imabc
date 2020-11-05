combine_results <- function(list1, list2) {
  # Just row bind the items
  items <- as.data.frame(rbind(list1, list2))

  return(items)
}
