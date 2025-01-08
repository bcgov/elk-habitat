# Step length calculations

# We will just use the implementation from the `bayesmove` package for this.

steplength <- function(elk) {
  dat <- data.frame(elk$animal_id, elk$dttm, sf::st_coordinates(elk))
  names(dat) <- c("animal_id", "date", "X", "Y")
  out <- bayesmove::prep_data(dat = dat, coord.names = c("X", "Y"), id = "animal_id")
  return(out)
}