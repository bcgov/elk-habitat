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
winter <- c("01-01", "03-31") # month-day format
spring <- c("04-01", "05-15") # month-day format
summer <- c("07-01", "08-31") # month-day format

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
  tar_target(elk, clean_collar_data(collar_data, 
                                    rarefy_pts = TRUE) |> 
               dplyr::filter(dttm < cutoff_date) |>
               sf::st_write("temp/Pipeline outputs/elk_positions.shp", append = FALSE)),
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
  #### SEASONAL HOME RANGE ESTIMATES ####
  ## MINIMUM CONVEX POLYGONS
  tar_target(winter_mcp, seasonal_mcp(elk = elk,
                                       season = winter,
                                       min_days = 0.9, # we want a sample size of a minimum of 90% days in the dataset covered
                                       percent = 0.95) |> # 95% MCP - convex hull that encompasses 95% of points. Defaults to Delaunay triangulation to find the center of the points.
               sf::st_write("temp/Pipeline outputs/Winter_MCP.shp", append = FALSE)),
  tar_target(spring_mcp, seasonal_mcp(elk = elk,
                                       season = spring,
                                       min_days = 0.9,
                                       percent = 0.95) |>
               sf::st_write("temp/Pipeline outputs/Spring_MCP.shp", append = FALSE)),
  tar_target(summer_mcp, seasonal_mcp(elk = elk,
                                       season = summer,
                                       min_days = 0.9,
                                       percent = 0.95) |>
               sf::st_write("temp/Pipeline outputs/Summer_MCP.shp", append = FALSE)),
  tar_target(all_seasons_mcp, dplyr::bind_rows(winter_mcp, spring_mcp, summer_mcp)),
  tar_target(mcp_seasonal_summary, summarize_area(all_seasons_mcp)),
  # TODO: summary plots of MCP areas (currently stored in `dBBMM_MCP_summary_plots.R`)
  ## DYNAMIC BROWNIAN BRIDGES
  tar_target(winter_dbbmm, seasonal_dbbmm(elk = elk,
                                          season = winter,
                                          min_days = 0.9,
                                          margin = 9, # 9 points ~= about ~1 day margin
                                          window.size = 57, # 57 points / 8 points per day = window size of ~7 days long
                                          location.error = 11.5, # GPS error in meters. Vectronic documentation indicates GPS error is on average 8-15m.
                                          ud_percent = 0.95) |>
               sf::st_write("temp/Pipeline outputs/Winter_dBBMM_window57_le11m.shp", append = FALSE)),
  tar_target(spring_dbbmm, seasonal_dbbmm(elk = elk,
                                          season = spring,
                                          min_days = 0.9,
                                          margin = 9,
                                          window.size = 57,
                                          location.error = 11.5,
                                          ud_percent = 0.95) |>
               sf::st_write("temp/Pipeline outputs/Spring_dBBMM_window57_le11m.shp", append = FALSE)),
  tar_target(summer_dbbmm, seasonal_dbbmm(elk = elk,
                                          season = summer,
                                          min_days = 0.9,
                                          margin = 9,
                                          window.size = 57,
                                          location.error = 11.5,
                                          ud_percent = 0.95) |>
               sf::st_write("temp/Pipeline outputs/Summer_dBBMM_window57_le11m.shp", append = FALSE)),
  tar_target(all_seasons_dbbmm, dplyr::bind_rows(winter_dbbmm, spring_dbbmm, summer_dbbmm)),
  tar_target(dbbmm_seasonal_summary, summarize_area(all_seasons_dbbmm)),
  # TODO: summary plots of dBBMM areas (currently stored in `dBBMM_MCP_summary_plots.R`)
  #### WEEKLY HOME RANGE ESTIMATES ####
  ## MINIMUM CONVEX POLYGONS
  # For now, only doing MCPs for weekly estimates. dBBMMs are a bit more
  # sensitive to lower sample sizes. 
  tar_target(weekly_mcps, weekly_mcp(elk = elk, 
                                     min_days = 1, # percentage of days - we want 100% of days
                                     min_dets_per_day = 7, # we also want at minimum 7 detections per day, otherwise that week of data is thrown out 
                                     percent = 0.95) |> # 95% MCP - convex hull that encompasses 95% of points. Defaults to Delaunay triangulation to find the center of the points.
               sf::st_write("temp/Pipeline outputs/Weekly_MCP.shp", append = FALSE)
             ),
  tar_target(weekly_dbbmms, weekly_dbbmm(elk = elk,
                                         min_days = 1,
                                         min_dets_per_day = 7,
                                         percent = 0.95) |>
               sf::st_write("temp/Pipeline outputs/Weekly_dBBMM.shp", append = FALSE)),
  #### DAILY HOME RANGE ESTIMATES ####
  ## MINIMUM CONVEX POLYGONS
  # Only doing MCPs for weekly estimates. dBBMM window/margin params are too
  # sensitive to lower sample sizes for daily home range estimates. 
  tar_target(daily_mcps, daily_mcp(elk = elk, 
                                    min_dets_per_day = 8, # Minimum 8 detections per day (100% fix rate)
                                    percent = 0.95) |> # 95% MCP - convex hull that encompasses 95% of points. Defaults to Delaunay triangulation to find the center of the points.
               sf::st_write("temp/Pipeline outputs/Daily_MCP.shp", append = FALSE)
  ),
  #### SEASONAL HOME RANGE OVERLAP ####
  # Winter to Spring
  tar_target(mcp_winter_spring_overlap, prct_overlap(shp_1 = winter_mcp,
                                                     shp_2 = spring_mcp,
                                                     shp_1_name = "winter",
                                                     shp_2_name = "spring")),
  tar_target(dbbmm_winter_spring_overlap, prct_overlap(shp_1 = winter_dbbmm,
                                                       shp_2 = spring_dbbmm,
                                                       shp_1_name = "winter",
                                                       shp_2_name = "spring")),
  #### YEARLY SEASONAL SITE FIDELITY ####
  # Year-to-year overlap within a season
  # MCP
  tar_target(mcp_winter_yearly_overlap, yearly_prct_overlap(shp = winter_mcp)),
  tar_target(mcp_spring_yearly_overlap, yearly_prct_overlap(shp = spring_mcp)),
  tar_target(mcp_summer_yearly_overlap, yearly_prct_overlap(shp = summer_mcp)),
  # dBBMM
  tar_target(dbbmm_winter_yearly_overlap, yearly_prct_overlap(shp = winter_dbbmm)),
  tar_target(dbbmm_spring_yearly_overlap, yearly_prct_overlap(shp = spring_dbbmm)),
  tar_target(dbbmm_summer_yearly_overlap, yearly_prct_overlap(shp = summer_dbbmm)),
  #### STEP LENGTHS ####
  tar_target(step_lengths, steplength(elk))
)
