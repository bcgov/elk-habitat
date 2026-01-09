# Step length calculations

# We will just use the implementation from the `bayesmove` package for this.

# Step lengths are filtered down to only include those btwn 3 hour fixes
# within the main targets pipeline ('step_lengths_3hr' target).

# Large step lengths between large GPS time gaps might be legitimate, but
# they skew summary statistics.

steplength <- function(elk) {
  dat <- data.frame(elk$animal_id, elk$dttm, sf::st_coordinates(elk))
  names(dat) <- c("animal_id", "date", "X", "Y")
  out <- bayesmove::prep_data(dat = dat, coord.names = c("X", "Y"), id = "animal_id")
  return(out)
}


# Helpful example for lead/lag distances between points
# Ultimately wasn't useful for overlap, but may be useful for step-length.
# https://stackoverflow.com/questions/49853696/distances-of-points-between-rows-with-sf
#dplyr::mutate(dist = st_distance(geometry, dplyr::lead(geometry), by_element = T))
