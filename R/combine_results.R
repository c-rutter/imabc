combine_results <- function(list1, list2) {
  # Returned sub-elements
  stored_items <- names(list1)

  # Join sub-elements together
  items <- list()
  for (i1 in stored_items) {
    items[[i1]] <- as.data.frame(rbind(list1[[i1]], list2[[i1]]))
  }

  return(items)
}
