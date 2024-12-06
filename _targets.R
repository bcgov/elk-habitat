# Created by use_targets().

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # for tar_map()

# Set target options:
tar_option_set(
  packages = c("sf",
               "ggplot2") # Packages that your targets need for their tasks.
  # format = "qs", # Optionally set the default storage format. qs is fast.
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()

# Set static variables
winter <- c("01-01", "03-31")
spring <- c("04-01", "05-15")
summer <- c("07-01", "08-31")

# Set max date for the study overall - we only want to use detections
# up to March 31 2024.
cutoff_date <- "2024-04-01"

# Replace the target list below with your own:
list(
  #### SETUP ####
  # Pull and track all collar keys files
  tar_target(collar_keys, collar::get_paths("data/Collar Keys"), format = "file"),
  # Download off Vectronix website
  tar_target(raw_collar_data, load_collar_data(collar_keys)),
  # Pull and track capture data file
  tar_target(capture_data_path, "data/DATABASE_Elk Capture_Updtd_20241127.xlsx", format = "file"),
  tar_target(capture_data, load_capture_data(capture_data_path)),
  # Assign capture ID to collar data
  tar_target(full_collar_data, attribute_animal_id(raw_collar_data, capture_data)),
  tar_target(unassigned_detections, full_collar_data[is.na(full_collar_data$animal_id), ]),
  tar_target(collar_data, full_collar_data[!is.na(full_collar_data$animal_id), ]),
  # Clean collar data AND filter to cutoff date
  tar_target(elk, clean_collar_data(collar_data) |> 
               dplyr::filter(dttm < cutoff_date)),
  #### SUMMARY STATS + PLOTS ####
  # Logger dotplot
  tar_target(elk_dotplot, logger_dotplot(elk)),
  # N elk online per month
  tar_target(elk_per_month, n_elk_per_month(elk)),
  tar_target(p_elk_per_month, elk_per_month_plot(elk)),
  # N detections per elk summary
  tar_target(dets_per_elk, n_dets_per_elk(elk)),
  tar_target(p_dets_per_elk, elk_dets_hist(elk)),
  # Fix success / detection efficiency
  tar_target(detection_efficiency, fix_rate(elk)),
  tar_target(p_efficiency, elk_fix_hist(detection_efficiency)),
  tar_render(daily_dets_plots, 
             "reports/elk_dets_per_day.Rmd", 
             output_file = "elk_dets_per_day.pdf",
             params = list(elk_data = elk)),
  #### HOME RANGE ESTIMATES ####
  ## MINIMUM CONVEX POLYGONS
  # For now, just doing 100% MCP, because getting 95% MCP takes an inordinate
  # amount of time.
  # TODO: got some kind of warning:
  #There were 2 warnings in `dplyr::summarise()`.
  #The first warning was:
  #  ℹ In argument: `n_days = ceiling(max(dttm) - min(dttm))`.
  #Caused by warning in `max.default()`:
  #  ! no non-missing arguments to max; returning -Inf
  #ℹ Run `dplyr::last_dplyr_warnings()` to see the 1 remaining warning. 
  tar_target(winter_mcps, elk_mcp(elk = elk, 
                                  season = winter, 
                                  min_days = 0.9, # we want a sample size of a minimum of 90% days in the dataset covered
                                  percent = 0.95)), # 95% MCP - convex hull that encompasses 95% of points. Defaults to Delaunay triangulation to find the center of the points.  
  tar_target(spring_mcps, elk_mcp(elk = elk, 
                                  season = spring, 
                                  min_days = 0.9, 
                                  percent = 0.95)),
  tar_target(summer_mcps, elk_mcp(elk = elk, 
                                  season = summer, 
                                  min_days = 0.9,
                                  0.95)),
  tar_target(MCP, dplyr::bind_rows(winter_mcps, spring_mcps, summer_mcps)),
  tar_target(mcp_summary, summarize_area(MCP)),
  # TODO: summary plots of MCP areas
  ## DYNAMIC BROWNIAN BRIDGES
  tar_target(winter_dbbmm, elk_dbbmm(elk = elk, season = winter, min_days = 0.9)),
  tar_target(spring_dbbmm, elk_dbbmm(elk = elk, season = spring, min_days = 0.9)),
  tar_target(summer_dbbmm, elk_dbbmm(elk = elk, season = summer, min_days = 0.9)),
  tar_target(dBBMM, dplyr::bind_rows(winter_dbbmm, spring_dbbmm, summer_dbbmm)),
  tar_target(dbbmm_summary, summarize_area(dBBMM))
  # TODO: summary plots of dBBMM areas
)
