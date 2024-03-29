% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/initial_access_surface.R
\name{initial_access_surface}
\alias{initial_access_surface}
\title{Create the initial accessibility surface}
\usage{
initial_access_surface(
  travel_matrix,
  transform = "inverse_dist_squared",
  minimum_time = 10,
  force_threshold = 300,
  n_fac_limit = NULL,
  normalized = TRUE,
  sparse = TRUE
)
}
\arguments{
\item{travel_matrix}{A matrix containing travel times, which pixels indexed by rows and locations indexed by columns. Typically created with catchment::create_travel_matrix_from_folder().}

\item{transform}{A character, numeric, or function. If character, a predefined transformation will be applied. Currently available options are "inverse_dist_squared". If numeric, then an inverse function with a defined value for the exponential term (e.g., 1/x^value). If a function, then the user-supplied transformation is applied.}

\item{minimum_time}{A numeric used to defined a minimum travel time. Travel times below this value will be set to the minimum time, this helps prevent the initial probabilities from being overly skewed by small numbers.}

\item{force_threshold}{A numeric used to define the maximum travel distance value, beyond which pixels will be forced to their nearest facility.}

\item{n_fac_limit}{either NULL or integer >1 and less than number of facitiies. Used to set a limit on the number of possible facilities an individual picel can attend.}

\item{normalized}{TRUE/FALSE Fix probabilities such that all rows (i.e., pixels) sum to 1.}

\item{sparse}{TRUE/FALSE return a sparse matrix used by catchment_model}
}
\value{
A matrix. If sparse == FALSE, then N_pixel rows by N_locations matrix containing initial assess surface will be returned. If sparse == TRUE, then a "sparse" Matrix object, used by model_catchment, is returned.
}
\description{
Create the initial accessibility surface
}
