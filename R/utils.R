
#' Shift coordinates to nearest within a set of provided locations
#'
#' @param org_coords A two column dataframe containing the X and Y coordinates to be shifted. Must be [X,Y] order.
#' @param valid_coords A two column dataframe containing the X,Y coordinates used to reference.
#'
#' @return A two column dataframe (or matrix) with the updated X,Y coordinates.
#' @export
#'

shift_coord <- function(org_coords, valid_coords) {

  n <- nrow(org_coords)
  x <- y <- numeric(length = n)

  for(i in 1:n){
    new_loc <-  valid_coords[which.min(
      (valid_coords[,1]-org_coords[i,1])^2 +
        (valid_coords[,2]-org_coords[i,2])^2),]

    x[i] <- new_loc[1]
    y[i] <- new_loc[2]
  }
  out <- cbind(x, y)

  # If input is a dataframe, return a dataframe with the same column names
  if(class(org_coords) == "data.frame") {
    out <- as.data.frame(out)
    names(out) <- names(org_coords)
  }

  return(out)
}
