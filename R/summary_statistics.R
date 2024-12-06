# Summary stats for the elk GPS dataset

# We need the following:
# 1. N elk online per month for the study period
# 2. Mean/median/min/max etc. N days tracked 
# 3. Fix success per elk (N detections / total expected number of detections
#     PER DEPLOYMENT - be aware to not artificially reduce detection efficiency
#     for elk that have a gap mid-deployment from being re-collared)

# 1. N elk online per month
n_elk_per_month <- function(elk) {
  n_elk_per_month <- elk |> 
    sf::st_drop_geometry() |>  # takes forever to run if you keep geoms
    dplyr::group_by(year, month) |>
    dplyr::summarise(N = dplyr::n_distinct(animal_id)) #|> dplyr::pull(N) |> summary()
  return(n_elk_per_month)
}

# 2. Mean/median/min/max detections per deployment
n_dets_per_elk <- function(elk) {
  n_dets_per_elk <- elk |>
    sf::st_drop_geometry() |>
    dplyr::group_by(animal_id, collar_id) |>
    dplyr::summarise(N_dets = dplyr::n()) |>
    dplyr::pull(N_dets) |>
    summary()
  return(n_dets_per_elk)
}

# 3. Fix success per deployment
fix_rate <- function(elk) {
  # Since not all elk have the same ping schedule, you'd need to group
  # by ping schedule and have different 100% efficiency denominator for
  # each collar id
  
  # First, calc the median daily detections per elk. Presumably,
  # unless there's a crazy malfunction, this should match the ping schedule
  # set by the researcher. Unfortunately without metadata on the schedule
  # it's otherwise impossible to know the schedule, so this is the next
  # best way to guess at that.
  
  # TODO: account for efficiency of half days, e.g. first and 
  # last days of deployment
  # TODO: account for oscillating ping schedule (e.g., 2023 elk 
  # with higher ping rate during calving season)
  efficiency <- 
    elk |>
    sf::st_drop_geometry() |>
    dplyr::group_by(animal_id, collar_id, lubridate::date(dttm)) |>
    dplyr::summarise(n_dets = dplyr::n()) |>
    dplyr::group_by(animal_id, collar_id) |>
    dplyr::summarise(actual_dets = sum(n_dets),
                     med_daily_dets = median(n_dets),
                     n_days = dplyr::n()) |>
    dplyr::mutate(n_pts_100_prct_efficiency = med_daily_dets * n_days,
                  efficiency = actual_dets / n_pts_100_prct_efficiency) #|>
    #dplyr::select(animal_id, collar_id, actual_dets, n_days, efficiency) |>
    #dplyr::rename(n_dets = actual_dets)
  
  return(efficiency)

}



