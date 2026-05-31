
#' Shift coordinates to nearest within a set of provided locations
#'
#' @param org_coords A two column dataframe containing the X and Y coordinates to be shifted. Must be [X,Y] order.
#' @param valid_coords A two column dataframe containing the X,Y coordinates used to reference.
#'
#' @return A two column dataframe (or matrix) with the updated X,Y coordinates.
#' @export
#'

shift_coord <- function(org_coords, valid_coords) {

  # Work with matrices so indexing returns numeric scalars, not data frame rows.
  org_mat <- as.matrix(org_coords)
  valid_mat <- as.matrix(valid_coords)

  n <- nrow(org_mat)
  x <- y <- numeric(length = n)

  for(i in 1:n){
    j <- which.min((valid_mat[, 1] - org_mat[i, 1])^2 +
                     (valid_mat[, 2] - org_mat[i, 2])^2)
    x[i] <- valid_mat[j, 1]
    y[i] <- valid_mat[j, 2]
  }
  out <- cbind(x, y)

  # If input is a dataframe, return a dataframe with the same column names
  if(inherits(org_coords, "data.frame")) {
    out <- as.data.frame(out)
    names(out) <- names(org_coords)
  }

  return(out)
}
