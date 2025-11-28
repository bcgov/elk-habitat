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

#### STATIC VARIABLES ####

# Seasons
winter <- c("01-01", "03-31") # month-day format
spring <- c("04-01", "05-15") # month-day format
summer <- c("07-01", "08-31") # month-day format

# Set max date for the study overall - we only want to use detections
# up to March 31 2024.
cutoff_date <- "2024-04-01"

# Two elk did not experience severe winter conditions, per Mario's
# work looking at snow depth data on cameras deployed across the
# study region. Remove these two elk from the severe winter
# comparison.
non_swp_elk <- c("20-1001", "20-1002")

# Severe winter period was 18 Dec 2021 - 14 Jan 2022
swp_dates <- seq(lubridate::ymd("2021-12-18"), lubridate::ymd("2022-01-14"), by = "1 day")

#lubridate::isoweek("2021-12-18")
#lubridate::isoweek("2022-01-14")
swp_weeks <- c(50, 51, 52, 53, 1, 2) # weeks 51 thru 1

#lubridate::yday("2021-12-18")
#lubridate::yday("2022-01-14")
swp_days <- c(lubridate::yday("2021-12-18"):366, 1:14)

# MCP percentile 
# E.g., 100% = all locations are included in creating the MCP
# 95% = all locations within the 95th percentile distance from the geographic centre
#       are included in creating the MCP
MCP_pctl <- 0.95

# dBBMM params
# dBBMM_ud - utilization distr isopleth to extract
#   E.g., 100% = full utilization distribution extracted for dBBMM
#   95% = 95th percentile isopleth extracted for dBBMM
# szn_window / szn_margin
#   dBBMM algorithm window & margin params for seasonal scale dBBMMs
# weekly_window / weekly_margin
#   dBBMM algorithm window & margin params for weekly scale dBBMMs
# TODO: would def be more efficient to save the entire raster stack of 
# calculated dBBMMs, then simply extract the isopleth of interest, so
# don't need to recalc the time consuming process of dBBMMs...
# do this if there's going to be lots of exploration of various UD levels.
# This comes at the cost of storage space - lots of rasters = lots of space.

# Previously used settings...
# szn_window <- 57
# szn_margin <- 9 
# weekly_window <- 7
# weekly_margin <- 3

# Now using default margin & window settings
# but keeping them stored as vars so the filename 
# can be built correctly
dBBMM_seasonal_ud <- 0.99 # To capture corridors of use btwn hotspots
dBBMM_weekly_ud <- 0.95 # To exclude transitory behavior
szn_window <- 31
szn_margin <- 11 
weekly_window <- 31
weekly_margin <- 11

# UWR LiDAR columns to extract
lidar_cols <- c("canopy_height",
                "Edge_Category",
                "Edge_Distance_LiDAR",
                "elevation",
                "slope_percent")

# VRI columns to extract
vri_cols <- c("Shape_Area",
              "INTERPRETATION_DATE",
              "REFERENCE_YEAR",
              "ATTRIBUTION_BASE_DATE",
              "PROJECTED_DATE",
              "HARVEST_DATE",
              "Disturbance_Start_Date",
              "Disturbance_End_Date",
              "Harvest_Year",
              "PROJ_AGE_1",
              "NEW_VRI_CC_RES_AGE",
              "BEST_AGE_CL_STS",
              "PROJ_HEIGHT_1",
              "VRI_LIVE_STEMS_PER_HA",
              "CROWN_CLOSURE",
              "SHRUB_HEIGHT",
              "SHRUB_CROWN_CLOSURE",
              "BCLCS_LEVEL_4", # broad species composition
              "BCLCS_LEVEL_5", # broad spp composition, but with density
              "SPECIES_CD_1", # spp composition code - leading species
              "SPECIES_CD_2", 
              "SPECIES_CD_3",
              "Creation_Date")

#### PIPELINE ####
list(
  #### SETUP ####
  # Pull and track all collar keys files
  tar_target(collar_keys, collar::get_paths("data/Collar Keys"), format = "file"),
  # Download off Vectronix website
  tar_target(raw_collar_data, load_collar_data(collar_keys)),
  # Pull and track capture data file
  tar_target(capture_data_path, "data/DATABASE_Elk Capture_Updtd_20250415.xlsx", format = "file"),
  tar_target(capture_data, load_capture_data(capture_data_path)),
  # Assign capture ID to collar data
  tar_target(full_collar_data, attribute_animal_id(raw_collar_data, capture_data)),
  tar_target(unassigned_detections, full_collar_data[is.na(full_collar_data$animal_id), ]),
  tar_target(collar_data, full_collar_data[!is.na(full_collar_data$animal_id), ]),
  # Clean collar data AND filter to cutoff date
  tar_target(cleaned_collar_data, clean_collar_data(collar_data, rarify_pts = FALSE)), # full dataset, cleaned of any spikes, but NOT rarified and NOT filtered to study time period
  tar_target(flagged_pts, collar_data[!(collar_data$idposition %in% cleaned_collar_data$idposition), ]),
  # Make our main `elk` df for further analysis
  tar_target(elk, collar_data |> # USE JUST `collar_data`!! We're rarifying this one
               clean_collar_data(rarify_pts = TRUE) |> # then it's going to recalc the velocity/angles etc. -> so that'll be different btwn `elk` and `cleaned_collar_data` dfs
               dplyr::filter(dttm < cutoff_date) |>
               assign_daily_seasons(seasons = list("winter" = winter, # defined toward the top of this document
                                                   "spring" = spring, # defined toward the top of this document
                                                   "summer" = summer), # defined toward the top of this document
                                    date_col = "dttm") |>
               sf::st_write("temp/Pipeline outputs/elk_positions.shp", append = FALSE)),
  # TODO: only re-run time consuming GIS stuff if the elk *geometry* changes?
  tar_target(elk_geom, sf::st_geometry(elk)),
  #### SUMMARY STATS + PLOTS ####
  # Logger dotplot
  tar_target(elk_dotplot, logger_dotplot(elk)),
  # N elk online per year-month
  tar_target(elk_per_year_month, n_elk_per_year_month(elk)),
  tar_target(p_elk_per_year_month, elk_per_year_month_plot(elk)),
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
  # Net squared displacement
  tar_render(nsd_plots,
             "reports/elk_nsd.Rmd",
             output_file = "elk_nsd.pdf",
             params = list(elk_data = elk)),

  # >> HOME RANGE ####

  #### SEASONAL HOME RANGE ESTIMATES ####
  ##### Minimum Convex Polygons (MCPs) #####
  tar_target(winter_mcp, seasonal_mcp(elk = elk,
                                       season = winter,
                                       min_days = 0.9, # we want a sample size of a minimum of 90% days in the dataset covered
                                       percent = MCP_pctl) |> # 95% MCP - convex hull that encompasses 95th distance-from-centre percentile points. Defaults to Delaunay triangulation to find the center of the points.
               sf::st_write(paste0("temp/Pipeline outputs/MCP_Winter_", (MCP_pctl * 100), "pctl.shp"),
                            append = FALSE)),
  tar_target(spring_mcp, seasonal_mcp(elk = elk,
                                       season = spring,
                                       min_days = 0.9,
                                       percent = MCP_pctl) |>
               sf::st_write(paste0("temp/Pipeline outputs/MCP_Spring_", (MCP_pctl * 100), "pctl.shp"),
                            append = FALSE)),
  tar_target(summer_mcp, seasonal_mcp(elk = elk,
                                       season = summer,
                                       min_days = 0.9,
                                       percent = MCP_pctl) |>
               sf::st_write(paste0("temp/Pipeline outputs/MCP_Summer_", (MCP_pctl * 100), "pctl.shp"),
                            append = FALSE)),
  tar_target(all_seasons_mcp, dplyr::bind_rows(winter_mcp, spring_mcp, summer_mcp)),
  tar_target(mcp_seasonal_summary, summarize_area(all_seasons_mcp, group_by = "season")),
  # TODO: summary plots of MCP areas (currently stored in `dBBMM_MCP_summary_plots.R`)
  ##### Dynamic Brownian Bridge Movement Models #####
  tar_target(winter_dbbmm, seasonal_dbbmm(elk = elk,
                                          season = winter,
                                          min_days = 0.9,
                                          margin = szn_margin, # 9 points ~= about ~1 day margin
                                          window.size = szn_window, # 57 points / 8 points per day = window size of ~7 days long
                                          location.error = 11.5, # GPS error in meters. Vectronic documentation indicates GPS error is on average 8-15m.
                                          ud_percent = dBBMM_seasonal_ud) |>
               sf::st_write(paste0("temp/Pipeline outputs/dBBMM_Winter_window", szn_window, "_margin", szn_margin, "_", (dBBMM_seasonal_ud * 100), "ud.shp"),
                            append = FALSE)),
  tar_target(spring_dbbmm, seasonal_dbbmm(elk = elk,
                                          season = spring,
                                          min_days = 0.9,
                                          margin = szn_margin,
                                          window.size = szn_window,
                                          location.error = 11.5,
                                          ud_percent = dBBMM_seasonal_ud) |>
               sf::st_write(paste0("temp/Pipeline outputs/dBBMM_Spring_window", szn_window, "_margin", szn_margin, "_", (dBBMM_seasonal_ud * 100), "ud.shp"),
                            append = FALSE)),
  tar_target(summer_dbbmm, seasonal_dbbmm(elk = elk,
                                          season = summer,
                                          min_days = 0.9,
                                          margin = szn_margin,
                                          window.size = szn_window,
                                          location.error = 11.5,
                                          ud_percent = dBBMM_seasonal_ud) |>
               sf::st_write(paste0("temp/Pipeline outputs/dBBMM_Summer_window", szn_window, "_margin", szn_margin, "_", (dBBMM_seasonal_ud * 100), "ud.shp"),
                            append = FALSE)),
  tar_target(all_seasons_dbbmm, dplyr::bind_rows(winter_dbbmm, spring_dbbmm, summer_dbbmm)),
  tar_target(dbbmm_seasonal_summary, summarize_area(all_seasons_dbbmm, group_by = "season")),
  # TODO: summary plots of dBBMM areas (currently stored in `dBBMM_MCP_summary_plots.R`)
  #### WEEKLY HOME RANGE ESTIMATES ####
  ##### MCP #####
  tar_target(weekly_mcps, weekly_mcp(elk = elk,
                                     min_days = 1, # percentage of days - we want 100% of days
                                     min_dets_per_day = 7, # we also want at minimum 7 detections per day, otherwise that week of data is thrown out
                                     percent = MCP_pctl) |> # 95% MCP - convex hull that encompasses 95th distance-from-centre percentile points. Defaults to Delaunay triangulation to find the center of the points.
               assign_weekly_seasons(seasons = list("winter" = winter, # defined toward the top of this document
                                                    "spring" = spring, # defined toward the top of this document
                                                    "summer" = summer) # defined toward the top of this document
               ) |>
               sf::st_write(paste0("temp/Pipeline outputs/MCP_Weekly_", (MCP_pctl * 100), "pctl.shp"),
                            append = FALSE)
             ),
  tar_target(weekly_mcp_seasonal_summary, weekly_mcps |>
               summarize_area(group_by = c("season"))),
  ###### Severe Winter Period MCPs ######
  tar_target(weekly_mcp_swp_summary, weekly_mcps |>
               dplyr::filter(!animal_id %in% non_swp_elk) |>
               dplyr::filter(week %in% swp_weeks) |>
               dplyr::mutate(year = isoyear) |>
               dplyr::mutate(year = dplyr::if_else(week < 3, year-1, year)) |>
               dplyr::mutate(year = paste0(year, "-", year+1)) |>
               summarize_area(group_by = "year")),
  ##### dBBMM #####
  tar_target(weekly_dbbmms, weekly_dbbmm(elk = elk,
                                         min_days = 1,
                                         min_dets_per_day = 7,
                                         window.size = weekly_window, # 21 hours
                                         margin = weekly_margin, # 9 hours
                                         location.error = 11.5,
                                         ud_percent = dBBMM_weekly_ud) |>
               assign_weekly_seasons(seasons = list("winter" = winter, # defined toward the top of this document
                                                    "spring" = spring, # defined toward the top of this document
                                                    "summer" = summer) # defined toward the top of this document
               ) |>
               sf::st_write(paste0("temp/Pipeline outputs/dBBMM_Weekly_window", weekly_window, "_margin", weekly_margin, "_", (dBBMM_weekly_ud * 100), "ud.shp"),
                            append = FALSE)),
  tar_target(weekly_dbbmm_seasonal_summary, weekly_dbbmms |>
               summarize_area(group_by = c("season"))),
  ###### Severe Winter Period dBBMMs ######
  tar_target(weekly_dbbmm_swp_summary, weekly_dbbmms |>
               dplyr::filter(!animal_id %in% non_swp_elk) |>
               dplyr::filter(week %in% swp_weeks) |>
               dplyr::mutate(year = isoyear) |>
               dplyr::mutate(year = dplyr::if_else(week < 3, year-1, year)) |>
               dplyr::mutate(year = paste0(year, "-", year+1)) |>
               summarize_area(group_by = "year")),
  #### DAILY HOME RANGE ESTIMATES ####
  ##### MCP #####
  # Only doing MCPs for weekly estimates. dBBMM window/margin params are too
  # sensitive to lower sample sizes for daily home range estimates.
  tar_target(daily_mcps, daily_mcp(elk = elk,
                                    min_dets_per_day = 8, # Minimum 8 detections per day (100% fix rate)
                                    percent = 1) |> # 100% MCP - no outliers in daily MCPs if all 8 points passed the data cleaning step!
               assign_daily_seasons(seasons = list("winter" = winter, # defined toward the top of this document
                                                   "spring" = spring, # defined toward the top of this document
                                                   "summer" = summer), # defined toward the top of this document
                                    date_col = "date") |>
               sf::st_write(paste0("temp/Pipeline outputs/MCP_Daily_", 100, "pctl.shp"),
                            append = FALSE)),
  tar_target(daily_mcp_seasonal_summary, daily_mcps |>
               summarize_area(group_by = c("season"))),
  ###### Severe Winter Period daily MCPs ######
  tar_target(daily_mcp_swp_summary, daily_mcps |>
               dplyr::mutate(doy = lubridate::yday(date)) |>
               dplyr::filter(!animal_id %in% non_swp_elk) |>
               dplyr::filter(doy %in% swp_days) |>
               dplyr::mutate(year = dplyr::if_else(doy < 15, year-1, year)) |>
               dplyr::mutate(year = paste0(year, "-", year+1)) |>
               summarize_area(group_by = "year")),
  #### SEASONAL HOME RANGE OVERLAP ####
  ##### Winter to Spring #####
  tar_target(mcp_winter_spring_overlap, prct_overlap(shp_1 = winter_mcp,
                                                     shp_2 = spring_mcp,
                                                     shp_1_name = "winter",
                                                     shp_2_name = "spring")),
  tar_target(dbbmm_winter_spring_overlap, prct_overlap(shp_1 = winter_dbbmm,
                                                       shp_2 = spring_dbbmm,
                                                       shp_1_name = "winter",
                                                       shp_2_name = "spring")),
  # Winter to Spring summary stats
  tar_target(mcp_wso_summary, summarize_overlap(mcp_winter_spring_overlap,
                                                group_by = "year",
                                                prct_1_col = "prct_winter_within_spring",
                                                prct_2_col = "prct_spring_within_winter")),
  tar_target(dbbmm_wso_summary, summarize_overlap(dbbmm_winter_spring_overlap,
                                                group_by = "year",
                                                prct_1_col = "prct_winter_within_spring",
                                                prct_2_col = "prct_spring_within_winter")),
  #### YEARLY SEASONAL SITE FIDELITY ####
  ##### Year-to-year overlap within a season #####
  ###### MCP ######
  tar_target(mcp_winter_yearly_overlap, yearly_prct_overlap(shp = winter_mcp)),
  tar_target(mcp_spring_yearly_overlap, yearly_prct_overlap(shp = spring_mcp)),
  tar_target(mcp_summer_yearly_overlap, yearly_prct_overlap(shp = summer_mcp)),
  tar_target(mcp_all_szn_yearly_overlap, merge_dfs(df_list = list("Winter" = mcp_winter_yearly_overlap,
                                                                  "Spring" = mcp_spring_yearly_overlap,
                                                                  "Summer" = mcp_summer_yearly_overlap)) |>
               dplyr::mutate(method = "MCP") |>
               dplyr::select(season, method, dplyr::everything())),
  ###### dBBMM ######
  tar_target(dbbmm_winter_yearly_overlap, yearly_prct_overlap(shp = winter_dbbmm)),
  tar_target(dbbmm_spring_yearly_overlap, yearly_prct_overlap(shp = spring_dbbmm)),
  tar_target(dbbmm_summer_yearly_overlap, yearly_prct_overlap(shp = summer_dbbmm)),
  tar_target(dbbmm_all_szn_yearly_overlap, merge_dfs(df_list = list("Winter" = dbbmm_winter_yearly_overlap,
                                                                    "Spring" = dbbmm_spring_yearly_overlap,
                                                                    "Summer" = dbbmm_summer_yearly_overlap)) |>
               dplyr::mutate(method = "dBBMM") |>
               dplyr::select(season, method, dplyr::everything())),
  ###### Year-to-year overlap summary stats ######
  # Merge all seasonal yearly overlap dfs into a single df
  tar_target(all_szn_yearly_overlap, dplyr::bind_rows(mcp_all_szn_yearly_overlap, dbbmm_all_szn_yearly_overlap)),
  # Summarize by season & year
  tar_target(yearly_seasonal_overlap_summary, summarize_overlap(all_szn_yearly_overlap,
                                                                group_by = c("season", "method", "year_to_year"),
                                                                prct_1_col = "prct_year_1_within_year_2",
                                                                prct_2_col = "prct_year_2_within_year_1")),
  # Summarize by just season (i.e. average across all years)
  tar_target(seasonal_overlap_summary, summarize_overlap(all_szn_yearly_overlap,
                                                         group_by = c("season", "method"),
                                                         prct_1_col = "prct_year_1_within_year_2",
                                                         prct_2_col = "prct_year_2_within_year_1")),
  ##### Aggregate seasonal overlap #####
  # Pool all winter MCPs (e.g.), calculate max overlap metrics
  # for the pooled seasonal areas. Using two methods:
  # 1) max overlap area / union of all areas
  # 2) GOI metric described in Ferrarini et al. (2021)
  tar_target(mcp_winter_agg_overlap, lapply(unique(winter_mcp$animal_id),
                                            function(x){
                                              shp <- winter_mcp[winter_mcp$animal_id == x, ]
                                              n <- nrow(shp)
                                              out <- aggregate_overlap(shp)
                                              out$animal_id <- x
                                              out$N <- n
                                              out$method <- "MCP"
                                              out <- out[,c("animal_id", "N", "method", "cumulative_overlap", "ferrarini_goi")]
                                              return(out)
                                            }) |>
               dplyr::bind_rows()),
  tar_target(dbbmm_winter_agg_overlap, lapply(unique(winter_dbbmm$animal_id),
                                              function(x){
                                                shp <- winter_dbbmm[winter_dbbmm$animal_id == x, ]
                                                n <- nrow(shp)
                                                out <- aggregate_overlap(shp)
                                                out$animal_id <- x
                                                out$N <- n
                                                out$method <- "dBBMM"
                                                out <- out[,c("animal_id", "N", "method", "cumulative_overlap", "ferrarini_goi")]
                                                return(out)
                                              }) |>
               dplyr::bind_rows()),
  ##### Cumulative home range #####
  tar_target(cumulative_winter_mcp, cumulative_shp(winter_mcp)),
  tar_target(cumulative_winter_dbbmm, cumulative_shp(winter_dbbmm)),
  #### STEP LENGTHS ####
  # Step lengths filtered to only include 3 hour timegaps
  # (Otherwise you might get large step lengths that are legit,
  # but 6+ hours apart if fixes were dropped or filtered)
  tar_target(step_lengths_3hr, elk |>
               sf::st_drop_geometry() |>
               dplyr::filter(dt > 10700 & dt < 10900) |> # filter to only include gaps of 3 hours
               dplyr::mutate(severe_winter_yn = lubridate::yday(dttm) %in% swp_days) |>
               dplyr::select(idposition, animal_id, collar_id, dttm,
                             lat, long, doy, step, angle, NSD, mps, kph, 
                             season, severe_winter_yn)),
  # Step length data is calculated during the GPS cleanup
  # Use the dataset that's filtered down to 3 hours for
  # the seasonal summaries
  tar_target(step_length_seasonal_summary, step_lengths_3hr |>
               dplyr::group_by(season) |>
               dplyr::summarise(mean_step = mean(step, na.rm = TRUE),
                                sd_step = sd(step, na.rm = TRUE),
                                median_step = median(step, na.rm = TRUE),
                                mean_nsd = mean(NSD, na.rm = TRUE),
                                sd_nsd = sd(NSD, na.rm = TRUE),
                                median_nsd = median(NSD, na.rm = TRUE),
                                N = dplyr::n())),
  # Step lengths for the SWP days only
  # 2021-2022 is the severe year
  tar_target(step_length_swp_summary, step_lengths_3hr |>
               dplyr::filter(severe_winter_yn == TRUE) |>
               dplyr::mutate(year = lubridate::year(dttm)) |>
               dplyr::mutate(year = dplyr::if_else(lubridate::month(dttm) == 12,
                                                   year + 1, 
                                                   year)) |>
               dplyr::mutate(season = paste0(year - 1, "-", year)) |>
               dplyr::group_by(season) |>
               dplyr::summarise(mean_step = mean(step, na.rm = TRUE),
                                sd_step = sd(step, na.rm = TRUE),
                                median_step = median(step, na.rm = TRUE),
                                mean_nsd = mean(NSD, na.rm = TRUE),
                                sd_nsd = sd(NSD, na.rm = TRUE),
                                median_nsd = median(NSD, na.rm = TRUE),
                                N = dplyr::n())),
  ##### 99 pctl step length #####
  # The 99th pctl step length is the distance that 99% of the elk are 
  # moving within the 3 hr gap between successive fixes. This will be
  # used to buffer the RSF polygons down the line.
  tar_target(step_length_buffer, step_lengths_3hr |> 
               dplyr::select(step) |> 
               dplyr::pull() |> 
               quantile(0.99)),

  # >> HABITAT SELECTION ANALYSIS ####

  #### GPS DATA EXTRACTION ####
  ##### DEM attributes #####
  # Download the BC CDED 30km DEM tiles, then for each elk GPS point,
  # extract elevation, slope grade (%), slope aspect (degrees), and
  # roughness.
  tar_target(cded, query_cded(elk = elk, output_dir = "GIS/DEM"), format = "file"),
  tar_target(elk_dem, extract_dem(pts = elk, cded_path = cded)),
  ##### LiDAR attributes #####
  # Pull the LiDAR-derived data products off the W:/ drive onto local
  # machine + keep track of it if it changes on the server, then extract
  # the data from it (elevation, slope grade (%), canopy height, edge
  # category, edge distance).
  # Keep track of the W:/ drive LiDAR file
  # This makes the pipeline a lot slower bc the server is slow. 
  # So commenting out. 
  # tar_target(uwr_lidar_gdb_path,
  #            "W:/wlap/nan/Workarea/Ecosystems_share/LiDAR/LiDAR_Project2020/Forsite_NOGO_UWR_Deliverables_Sept2021/UWR_Deliverables/uwr_intermediate_north_island.gdb",
  #            format = "file"),
  # Make a local copy of the W:/ drive LiDAR file (this will get re-downloaded
  # if the W:/ drive copy is ever updated/modified)
  tar_target(uwr_lidar_gdb,
             download_from_server(#server_path = uwr_lidar_gdb_path,
                                  server_path = "W:/wlap/nan/Workarea/Ecosystems_share/LiDAR/LiDAR_Project2020/Forsite_NOGO_UWR_Deliverables_Sept2021/UWR_Deliverables/uwr_intermediate_north_island.gdb",
                                  local_path = "GIS/LiDAR products",
                                  download = FALSE), # set to FALSE bc I just manually moved it over in the end
             format = "file"),
  tar_target(elk_uwr, extract_uwr(pts = elk,
                                  gdb = uwr_lidar_gdb,
                                  layers = lidar_cols)),
  # Since the UWR layers might not be suitable for this analysis, let's
  # also extract data from a crown height model that was provided to us
  # by BCTS.
  tar_target(chm_path, "GIS/LiDAR products/crown_height.tif", format = "file"),
  tar_target(elk_chm, extract_chm(pts = elk,
                                  path = chm_path)),
  ##### VRI attributes #####
  # Note we are using the improved VRI layer that was provided by Madrone.
  # tar_target(madrone_vri_gdb_path,
  #            "W:/wlap/nan/Workarea/Ecosystems_share/WHR_Models/2023/SEPT2023_v4/SEPT2023_v4_Elk Models and Spatial/Spatial/Operational_Data_6636.gdb",
  #            format = "file"),
  tar_target(madrone_vri_gdb_path, "W:/wlap/nan/Workarea/Ecosystems_share/WHR_Models/2023/SEPT2023_v4/SEPT2023_v4_Elk Models and Spatial/Spatial/Operational_Data_6636.gdb"),
  tar_target(madrone_vri_gdb, download_from_server(server_path = madrone_vri_gdb_path,
                                                   local_path = "GIS/VRI",
                                                   download = FALSE)), # set to FALSE bc I just manually moved it over in the end
  tar_target(vri, read_vri(gdb = madrone_vri_gdb)),
  #tar_target(vri_edges, extract_vri_edges(elk = elk, vri = vri)), # fails: not enough memory
  tar_target(elk_vri, extract_vri(pts = elk,
                                  vri = vri,
                                  cols = vri_cols)),
  # tar_target(elk_edge_dist, st_edge_dist(feature = elk,
  #                                        edges = vri_edges))
  #### DEFINE AVAILABILITY ####
  ##### Availability MCPs - Seasonal #####
  # Rather than pull from the 95 percentile MCPs, known available habitat
  # should pull from 100% of the area covered by the GPS points. The area
  # we draw from for availability is just that - *available* space - and it
  # is *not* equivalent to a home range. So, draw MCPs around any points
  # that have passed our data QC filters.
  # First create a polygon that is the shapefile of our overall study area
  # This will be used to clip our RSF MCPs to land (i.e. ensure our RSF MCPs
  # all occur in areas elk can actually access)
  tar_target(study_area, study_area_poly(elk)),
  # Winter RSF MCP
  tar_target(winter_rsf_mcp, seasonal_mcp(elk = elk,
                                      season = winter,
                                      min_days = 0, # we want to include the full dataset, regardless of minimum N points
                                      percent = 100) |> # 100% MCP - include all points
               sf::st_union() |>
               sf::st_buffer(dist = step_length_buffer / 2) |> # buffer outermost points. In theory, an elk could move half it's step length out, and then half it's step length back in within the 3 hour gap btwn fixes.
               sf::st_intersection(study_area) |>
               sf::st_write("temp/Pipeline outputs/MCP_RSF_Winter.shp",
                            append = FALSE)),
  # Spring RSF MCP
  tar_target(spring_rsf_mcp, seasonal_mcp(elk = elk,
                                          season = spring,
                                          min_days = 0, # we want to include the full dataset, regardless of minimum N points
                                          percent = 100) |> # 100% MCP - include all points
               sf::st_union() |>
               sf::st_buffer(dist = step_length_buffer / 2) |> # buffer outermost points
               sf::st_intersection(study_area) |>
               sf::st_write("temp/Pipeline outputs/MCP_RSF_Spring.shp",
                            append = FALSE)),
  # Summer RSF MCP
  tar_target(summer_rsf_mcp, seasonal_mcp(elk = elk,
                                          season = summer,
                                          min_days = 0, # we want to include the full dataset, regardless of minimum N points
                                          percent = 100) |> # 100% MCP - include all points
               sf::st_union() |>
               sf::st_buffer(dist = step_length_buffer / 2) |> # buffer outermost points
               sf::st_intersection(study_area) |>
               sf::st_write("temp/Pipeline outputs/MCP_RSF_Summer.shp",
                            append = FALSE)),
  ##### Availability MCPs - SWP #####
  # The severe winter period avialability polygon is a special case
  # of the Winter polygons. It is the merged Winter MCPs of only elk
  # that experienced the SWP (i.e. were collared and we have tracking
  # for).
  # First subset to SWP elk
  # These are the elk IDs that specifically experienced the 2021
  # severe winter (i.e. cuts out any that also weren't collared
  # yet) 
  tar_target(swp_elk, elk |> 
               sf::st_drop_geometry() |>
               dplyr::filter(lubridate::date(dttm) %in% swp_dates) |>
               dplyr::select(animal_id) |> 
               dplyr::distinct() |> 
               dplyr::pull()),
  # Severe Winter Period RSF MCP
  tar_target(swp_rsf_mcp, elk |>
               dplyr::filter(animal_id %in% swp_elk) |>
               seasonal_mcp(season = winter,
                            min_days = 0,
                            percent = 100) |> # 100% MCP - include all points
               # Merge in two weekly MCPs from two individuals whose 
               # Dec 14-Dec 31 data is not actually captured within the Winter MCP
               dplyr::bind_rows(weekly_mcps |>
                                dplyr::filter((animal_id == '20-1000' & isoyear_week == '2021.51')|
                                              (animal_id == '20-0982' & isoyear_week == '2021.52'))
                                ) |>
               sf::st_union() |>
               sf::st_buffer(dist = step_length_buffer / 2) |> # buffer outermost points
               sf::st_intersection(study_area) |>
               sf::st_write("temp/Pipeline outputs/MCP_RSF_SWP.shp",
                            append = FALSE)
             ),
  #### RANDOM POINTS ####
  ##### Sample random pts #####
  # Sample random points within each of our availability MCPs to use in RSFs
  tar_target(random_winter, sf::st_sample(winter_rsf_mcp, size = nrow(elk)) |> 
               sf::st_as_sf() |>
               dplyr::mutate(idposition = dplyr::row_number())),
  tar_target(random_spring, sf::st_sample(spring_rsf_mcp, size = nrow(elk)) |> 
               sf::st_as_sf() |>
               dplyr::mutate(idposition = dplyr::row_number())),
  tar_target(random_summer, sf::st_sample(summer_rsf_mcp, size = nrow(elk)) |> 
               sf::st_as_sf() |>
               dplyr::mutate(idposition = dplyr::row_number())),
  tar_target(random_swp, sf::st_sample(swp_rsf_mcp, size = nrow(elk)) |> 
               sf::st_as_sf() |>
               dplyr::mutate(idposition = dplyr::row_number())),
  #### RANDOM DATA EXTRACTION ####
  ##### DEM attributes #####
  tar_target(random_winter_dem, extract_dem(pts = random_winter, cded_path = cded)),
  tar_target(random_spring_dem, extract_dem(pts = random_spring, cded_path = cded)),
  tar_target(random_summer_dem, extract_dem(pts = random_summer, cded_path = cded)),
  tar_target(random_swp_dem, extract_dem(pts = random_swp, cded_path = cded)),
  ##### LiDAR attributes #####
  # TODO: add CHM
  tar_target(random_winter_uwr, extract_uwr(pts = random_winter,
                                            gdb = uwr_lidar_gdb,
                                            layers = lidar_cols)),
  tar_target(random_spring_uwr, extract_uwr(pts = random_spring,
                                            gdb = uwr_lidar_gdb,
                                            layers = lidar_cols)),
  tar_target(random_summer_uwr, extract_uwr(pts = random_summer,
                                            gdb = uwr_lidar_gdb,
                                            layers = lidar_cols)),
  tar_target(random_swp_uwr, extract_uwr(pts = random_swp,
                                            gdb = uwr_lidar_gdb,
                                            layers = lidar_cols)),
  ##### VRI attributes #####
  tar_target(random_winter_vri, extract_vri(pts = random_winter,
                                            vri = vri,
                                            cols = vri_cols)),
  tar_target(random_spring_vri, extract_vri(pts = random_spring,
                                            vri = vri,
                                            cols = vri_cols)),
  tar_target(random_summer_vri, extract_vri(pts = random_summer,
                                            vri = vri,
                                            cols = vri_cols)),
  tar_target(random_swp_vri, extract_vri(pts = random_swp,
                                            vri = vri,
                                            cols = vri_cols))

)

