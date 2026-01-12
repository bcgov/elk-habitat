# Step length calculations

# Step lengths are filtered down to only include those btwn 3 hour fixes
# within the main targets pipeline ('step_lengths_3hr' target).

# Large step lengths between large GPS time gaps might be legitimate, but
# they skew summary statistics.

# OUTDATED
# We will just use the implementation from the `bayesmove` package for this
# within the `clean_collar_data()` fxn.
step_length <- function(elk) {
  dat <- data.frame(elk$animal_id, elk$dttm, sf::st_coordinates(elk))
  names(dat) <- c("animal_id", "date", "X", "Y")
  out <- bayesmove::prep_data(dat = dat, coord.names = c("X", "Y"), id = "animal_id")
  return(out)
}


# This function takes a given shape, arranges it by the supplied
# group_by id x date_col, then calculates the centroid-to-centroid
# distance between successive shapes. 
# Unfortunately didn't keep collar ID in the shps... so we will 
# have centroid distances btwn the few separate collar deployments
# on a single individual
centroid_step <- function(shp, group_by, date_col) {
  c <- suppressWarnings(sf::st_centroid(shp))
  out <- c |>
    dplyr::group_by_at(group_by) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(c(group_by, date_col)))) |>
    dplyr::mutate(lag = dplyr::lag(geometry),
                  centroid_dist = sf::st_distance(lag, geometry, by_element = TRUE)) |>
    sf::st_drop_geometry() |>
    dplyr::select(-lag)
  return(out)
}
