# Percent overlap functions


prct_overlap <- function(shp_1, shp_2, 
                         join_cols = c("animal_id", "year"),
                         shp_1_name = NA, # e.g., if shp_1 is winter season, `shp_1_name` could be "winter"
                         shp_2_name = NA,
                         area_unit = "ha") {
  # Data health checks
  stopifnot("`shp_1` and `shp_2` must be in the same CRS." = sf::st_crs(shp_1) == sf::st_crs(shp_2))
  # Pull list of unique join_cols (by default, animal_id + year)
  # The function will then iterate over this list to calc % overlap
  # between the two shapes
  all_shp <- dplyr::bind_rows(shp_1, shp_2)
  join_list <- unique(sf::st_drop_geometry(all_shp[join_cols]))
  # Now for each animal_id + year in the list, pull the correct polygon,
  # intersect, and calculate % overlap
  ovrlp_out <- pbapply::pblapply(1:nrow(join_list), function(x) {
    row_to_pull <- join_list[x, ]
    #print(x) # for bugfixing
    shp_1a <- merge(shp_1, row_to_pull)
    shp_2a <- merge(shp_2, row_to_pull)
    
    # Calculate area of each shp
    shp_1a$area <- sf::st_area(shp_1a)
    shp_2a$area <- sf::st_area(shp_2a)
    
    # Set unit of area - so units are consistent in output, even if no overlap present
    if (nrow(shp_1a) > 0) shp_1a$area <- units::set_units(shp_1a$area, value = area_unit, mode = "standard")
    if (nrow(shp_2a) > 0) shp_2a$area <- units::set_units(shp_2a$area, value = area_unit, mode = "standard")
    
    # Set overlap colnames, if provided
    if (is.na(shp_1_name)) shp_1_name <- "shp_1" 
    if (is.na(shp_2_name)) shp_2_name <- "shp_2"
    
    shp_1_name <- janitor::make_clean_names(shp_1_name)
    shp_2_name <- janitor::make_clean_names(shp_2_name)
    
    overlap_1_name <- paste0("prct_", shp_1_name, "_within_", shp_2_name)
    overlap_2_name <- paste0("prct_", shp_2_name, "_within_", shp_1_name)
    
    # Prepare output colnames
    out_colnames <- c(join_cols, paste0(shp_1_name, "_area"), paste0(shp_2_name, "_area"),
                      "overlap_area", overlap_1_name, overlap_2_name)
    
    # Only run the rest of the overlap function if both 
    # shps have area > 0, i.e. both are not-NULL geometry
    if (nrow(shp_1a) > 0 & nrow(shp_2a) > 0) {
      
      # Intersect the two shps
      overlap <- suppressWarnings(sf::st_intersection(shp_1a, shp_2a))
      overlap <- overlap[,c("geometry")] # drop everything except for geometry
      
      # Make dummy df for overlap if it returns empty geometry (i.e.,
      # no overlap). Otherwise, return the actual overlap geometry
      if (nrow(overlap) == 0) {
        overlap <- data.frame(area = units::set_units(0, value = area_unit, mode = "standard"),
                              geometry = NA)
      } else {
        overlap$area <- sf::st_area(overlap) 
        overlap$area <- units::set_units(overlap$area, value = area_unit, mode = "standard")
      }
      
      # Calc % overlap
      shp_1a[[overlap_1_name]] <- units::set_units(overlap$area / shp_1a$area, "percent")
      shp_2a[[overlap_2_name]] <- units::set_units(overlap$area / shp_2a$area, "percent")
      
      # Merge them all together
      out <- data.frame(c(sf::st_drop_geometry(shp_1a[join_cols]), 
                          sf::st_drop_geometry(shp_1a["area"]), # doing it this way rather than simply `shp_1a$area` to preserve units
                          sf::st_drop_geometry(shp_2a["area"]),
                          sf::st_drop_geometry(overlap["area"]),
                          sf::st_drop_geometry(shp_1a[overlap_1_name]),
                          sf::st_drop_geometry(shp_2a[overlap_2_name]))) |>
        setNames(out_colnames)
      
    } else {
      # Return modified df if overlap == 0
      if (nrow(shp_1a) > 0) {
        first_cols_values <- sf::st_drop_geometry(shp_1a[join_cols])
      } else {
        first_cols_values <- sf::st_drop_geometry(shp_2a[join_cols])
      }
      out <- data.frame(c(first_cols_values,
                          ifelse(nrow(shp_1a) > 0, shp_1a$area, 0),
                          ifelse(nrow(shp_2a) > 0, shp_2a$area, 0),
                          0, # overlap area
                          0, # %
                          0)) |> # % overlap
        setNames(out_colnames)
      
      # Set units of `out`
      area_cols <- names(out)[grep("_area", names(out))]
      out[area_cols] <- lapply(area_cols, function(x){
        units::set_units(out[[x]], value = area_unit, mode = "standard")
      })
      prct_cols <- names(out)[grep("prct_", names(out))]
      out[prct_cols] <- lapply(prct_cols, function(x){
        units::set_units(out[[x]], value = "percent")
      })
    }
    
    return(out)
    
  })
  
  ovrlp_out <- dplyr::bind_rows(ovrlp_out)
  return(ovrlp_out)
  
  
}
