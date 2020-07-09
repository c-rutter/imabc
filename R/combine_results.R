combine_results <- function(list1, list2) {
  # If results is a list of lists then you can use the commented code to join the sublists into a list of data.frames
  # # Returned sub-elements
  # stored_items <- names(list1)
  #
  # # Join sub-elements together
  # items <- list()
  # for (i1 in stored_items) {
  #   items[[i1]] <- as.data.frame(rbind(list1[[i1]], list2[[i1]]))
  # }

  items <- as.data.frame(rbind(list1, list2))

  return(items)
}
