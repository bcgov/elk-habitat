#### SETUP ####

# Wishlist
# TODO: Summary stats of weekly/daily MCP area x Season

# Libraries
library(targets)
library(sf)
library(ggplot2)
library(ggsignif)
library(units)

# Color palette
okabe <- c("#CC79A7", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00")

# How many days per season?
lubridate::date("2024-03-31") - lubridate::date("2024-01-01") # Winter
lubridate::date("2024-05-15") - lubridate::date("2024-04-01") # Spring
lubridate::date("2024-08-31") - lubridate::date("2024-07-01") # Summer

# Load data
MCP <- tar_read(all_seasons_mcp)
dBBMM <- tar_read(all_seasons_dbbmm)
tar_load(mcp_summary)
tar_load(dbbmm_summary)

# Make a column of area normalized by N days
MCP <- MCP |>
  dplyr::mutate(n_days = dplyr::case_when(season == "Winter" ~ 90,
                                          season == "Spring" ~ 44,
                                          season == "Summer" ~ 61))
MCP$area_per_day <- MCP$area / MCP$n_days

dBBMM <- dBBMM |>
  dplyr::mutate(n_days = dplyr::case_when(season == "Winter" ~ 90,
                                          season == "Spring" ~ 44,
                                          season == "Summer" ~ 61))
dBBMM$area_per_day <- dBBMM$area / dBBMM$n_days


#### SEASONAL MCP PLOTS ####

# Summary plots of MCPs
ggplot(MCP) +
  geom_density(aes(x = area,
                   color = season,
                   fill = season),
               alpha = 0.1) +
  scale_color_manual(values = okabe[1:3]) +
  scale_fill_manual(values = okabe[1:3]) +
  labs(title = "95% MCP areas",
       caption = "Note that this is not normalized to # of days per season.") +
  theme_minimal()

ggplot(MCP) +
  geom_density(aes(x = area_per_day,
                   color = season,
                   fill = season),
               alpha = 0.1) +
  scale_color_manual(values = okabe[1:3]) +
  scale_fill_manual(values = okabe[1:3]) +
  labs(title = "95% MCP areas", 
       subtitle = "Normalized by the number of days per season",
       x = "Area per day") +
  theme_minimal()

ggplot(MCP, 
       aes(x = season, y = area, color = season)) +
  geom_boxplot() +
  geom_jitter() +
  scale_color_manual(values = okabe[1:3]) +
  scale_fill_manual(values = okabe[1:3]) +
  geom_signif(comparisons = list(c("Spring", "Summer"),
                                 c("Summer", "Winter")),
              map_signif_level = TRUE,
              color = "black") +
  geom_signif(comparisons = list(c("Spring", "Winter")),
              map_signif_level = TRUE,
              y_position = 30000,
              color = "black") +
  labs(title = "Log 95% MCP areas",
       caption = "Note that this is not normalized to # of days per season.") +
  coord_trans(y = "log10") +
  theme_minimal()

ggplot(MCP, 
       aes(x = season, y = area_per_day, color = season)) +
  geom_boxplot() +
  geom_jitter() +
  scale_color_manual(values = okabe[1:3]) +
  scale_fill_manual(values = okabe[1:3]) +
  geom_signif(comparisons = list(c("Spring", "Summer"),
                                 c("Summer", "Winter")),
              map_signif_level = TRUE,
              color = "black") +
  geom_signif(comparisons = list(c("Spring", "Winter")),
              map_signif_level = TRUE,
              y_position = 430,
              color = "black") +
  labs(title = "95% MCP areas",
       subtitle = "Normalized by the number of days per season",
       y = "Area per day") +
  theme_minimal()

# Yearly variation plots
# Winter
MCP |> 
  dplyr::mutate(year = as.factor(year)) |>
  dplyr::filter(season == "Winter") |>
  ggplot(aes(x = year,
             y = area, 
             color = year)) +
  geom_boxplot() +
  geom_jitter() +
  scale_color_manual(values = okabe) +
  coord_trans(y = "log10") +
  labs(title = "95% MCP Winter areas",
       subtitle = "Visualizing yearly variation in Winter range size") +
  theme_minimal()

# Spring
MCP |> 
  dplyr::mutate(year = as.factor(year)) |>
  dplyr::filter(season == "Spring") |>
  ggplot(aes(x = year,
             y = area, 
             color = year)) +
  geom_boxplot() +
  geom_jitter() +
  scale_color_manual(values = okabe) +
  coord_trans(y = "log10") +
  labs(title = "95% MCP Spring areas",
       subtitle = "Visualizing yearly variation in Spring range size") +
  theme_minimal()

# Summer
MCP |> 
  dplyr::mutate(year = as.factor(year)) |>
  dplyr::filter(season == "Summer") |>
  ggplot(aes(x = year,
             y = area, 
             color = year)) +
  geom_boxplot() +
  geom_jitter() +
  scale_color_manual(values = okabe) +
  coord_trans(y = "log10") +
  labs(title = "95% MCP Summer areas",
       subtitle = "Visualizing yearly variation in Summer range size",
       caption = "PNW Heat Dome was in June 25-July 07, 2021. 'Summer' data starts July 01.") +
  theme_minimal()


#### SEASONAL DBBMM PLOTS ####

# Summary plots of dBBMM
ggplot(dBBMM) +
  geom_density(aes(x = area,
                   color = season,
                   fill = season),
               alpha = 0.1) +
  scale_color_manual(values = okabe[1:3]) +
  scale_fill_manual(values = okabe[1:3]) +
  labs(title = "dBBMM 95% UD areas",
       caption = "Note that this is not normalized to # of days per season.") +
  theme_minimal()

ggplot(dBBMM) +
  geom_density(aes(x = area_per_day,
                   color = season,
                   fill = season),
               alpha = 0.1) +
  scale_color_manual(values = okabe[1:3]) +
  scale_fill_manual(values = okabe[1:3]) +
  labs(title = "dBBMM 95% UD areas", 
       subtitle = "Normalized by the number of days per season",
       x = "Area per day") +
  theme_minimal()

ggplot(dBBMM, 
       aes(x = season, y = area, color = season)) +
  geom_boxplot() +
  geom_jitter() +
  scale_color_manual(values = okabe[1:3]) +
  scale_fill_manual(values = okabe[1:3]) +
  geom_signif(comparisons = list(c("Spring", "Summer"),
                                 c("Summer", "Winter")),
              map_signif_level = TRUE,
              color = "black") +
  geom_signif(comparisons = list(c("Spring", "Winter")),
              map_signif_level = TRUE,
              y_position = 2800,
              color = "black") +
  labs(title = "dBBMM 95% UD areas",
       caption = "Note that this is not normalized to # of days per season.") +
  theme_minimal()

ggplot(dBBMM, 
       aes(x = season, y = area_per_day, color = season)) +
  geom_boxplot() +
  geom_jitter() +
  scale_color_manual(values = okabe[1:3]) +
  scale_fill_manual(values = okabe[1:3]) +
  geom_signif(comparisons = list(c("Spring", "Summer"),
                                 c("Summer", "Winter")),
              map_signif_level = TRUE,
              color = "black") +
  geom_signif(comparisons = list(c("Spring", "Winter")),
              map_signif_level = TRUE,
              y_position = 47,
              color = "black") +
  labs(title = "dBBMM 95% UD areas",
       subtitle = "Normalized by the number of days per season",
       y = "Area per day") +
  theme_minimal()


# Yearly variation plots
# Winter
dBBMM |> 
  dplyr::mutate(year = as.factor(year)) |>
  dplyr::filter(season == "Winter") |>
  ggplot(aes(x = year,
             y = area, 
             color = year)) +
    geom_boxplot() +
    geom_jitter() +
    scale_color_manual(values = okabe) +
    labs(title = "dBBMM 95% UD Winter areas",
         subtitle = "Visualizing yearly variation in Winter range size") +
    theme_minimal()

# Spring
dBBMM |> 
  dplyr::mutate(year = as.factor(year)) |>
  dplyr::filter(season == "Spring") |>
  ggplot(aes(x = year,
             y = area, 
             color = year)) +
  geom_boxplot() +
  geom_jitter() +
  scale_color_manual(values = okabe) +
  labs(title = "dBBMM 95% UD Spring areas",
       subtitle = "Visualizing yearly variation in Spring range size") +
  theme_minimal()

# Summer
dBBMM |> 
  dplyr::mutate(year = as.factor(year)) |>
  dplyr::filter(season == "Summer") |>
  ggplot(aes(x = year,
             y = area, 
             color = year)) +
  geom_boxplot() +
  geom_jitter() +
  scale_color_manual(values = okabe) +
  labs(title = "dBBMM 95% UD Summer areas",
       subtitle = "Visualizing yearly variation in Summer range size",
       caption = "PNW Heat Dome was in June 25-July 07, 2021. 'Summer' data starts July 01.") +
  theme_minimal()


#### WEEKLY MCP PLOTS ####

tar_load(weekly_mcps)

# # Remove weekly MCP outliers
# # Pull out area data
# dat <- sf::st_drop_geometry(weekly_mcps["area"])
# dat <- units::drop_units(dat)
# # Construct isolation forest model
# if_model <- isotree::isolation.forest(dat, sample_size = 30, ndim=1, ntrees=10, nthreads=1)
# # Predict outliers from non-outliers
# scores <- isotree::predict.isolation_forest(if_model, dat, type="avg_depth")
# # Visualize
# plot(dat$area, scores)
# # Choose cutoff for what counts as an 'outlier score'. 
# # Choose scores that 99% of the data fall into
# outlier_threshold <- quantile(scores, 0.01)[[1]]
# # Add scores back to dat
# dat$isolation_score <- scores
# # Compare 
# hist(dat$area)
# hist(dat[dat$isolation_score > outlier_threshold, "area"])
# max(dat[dat$isolation_score > outlier_threshold, "area"])
# 
# # So our max cutoff will be 1583 hectares. 
# outlier_cutoff <- ceiling(max(dat[dat$isolation_score > outlier_threshold, "area"]))
# outlier_cutoff <- units::set_units(outlier_cutoff, "ha")

weekly_mcps |>
  dplyr::mutate(year = as.factor(isoyear),
                area = units::drop_units(area)) |>
  ggplot() +
  # Winter
  annotate("rect",
           xmin = 0, xmax = 14, ymin = 0, ymax = Inf,
           fill = "#e1eeff",
           #fill = "#F0F0F0",
           alpha = 0.6) +
  # Spring
  annotate("rect",
           xmin = 14, xmax = 21, ymin = 0, ymax = Inf,
            #fill = "#F0F0F0",
            fill = "#f7ffe1",
            alpha = 0.6) +
  # Summer
  annotate("rect",
           xmin = 27, xmax = 36, ymin = 0, ymax = Inf,
            #fill = "#F0F0F0",
            fill = "#fff4e1",
            alpha = 0.6) +
  geom_smooth(aes(x = week,
                  y = area,
                  color = year,
                  group = year),
              method = "loess",
              se = FALSE) +
  scale_color_manual(values = okabe) +
  labs(title = "Weekly MCP size, colored by year",
       y = "Area [ha]",
       x = "Week",
       caption = "Blue rectangle = 'Winter'\nGreen rectangle = 'Spring'\nOrange rectangle = 'Summer'") +
  theme_minimal()

# weekly_mcps |>
#   dplyr::mutate(year = isoyear,
#                 beginning = lubridate::ymd(paste(year, "-01-01")),
#                 date = beginning + lubridate::weeks(week - 1),
#                 area = units::drop_units(area)) |>
#   ggplot(aes(x = date,
#              y = area,
#              color = year,
#              group = week)) +
#   geom_boxplot(outlier.shape = NA,
#                fill = NA) +
#   scale_y_continuous(limits = c(0, 800)) +
#   labs(title = "Mean weekly MCP size through time",
#        subtitle = "Outliers removed") +
#   theme_minimal()
  

weekly_mcps |>
  dplyr::mutate(year = isoyear,
                beginning = lubridate::ymd(paste(year, "-01-01")),
                date = beginning + lubridate::weeks(week - 1)) |>
  ggplot(aes(x = date,
             y = area,
             color = year)) +
  geom_point(alpha = 0.3) +
  labs(title = "All weekly MCP areas through time",
       subtitle = "Including outliers") +
  theme_minimal()

weekly_mcps |>
  dplyr::mutate(year = isoyear,
                beginning = lubridate::ymd(paste(year, "-01-01")),
                date = beginning + lubridate::weeks(week - 1)) |>
  ggplot(aes(x = date,
             y = area)) +
  #geom_point() +
  geom_smooth(method = "loess",
              span = 0.1,
              se = FALSE) +
  theme_minimal()

weekly_mcps |>
  sf::st_drop_geometry() |>
  dplyr::mutate(year = isoyear,
                beginning = lubridate::ymd(paste(year, "-01-01")),
                date = beginning + lubridate::weeks(week - 1)) |>
  dplyr::group_by(date) |>
  dplyr::summarise(mean_area = mean(area),
                   n = dplyr::n(),
                   sd_area = sd(area),
                   se = sd_area/sqrt(n),
                   t_score = qt(p = 0.05/2, 
                                df = n - 1,
                                lower.tail=F),
                   margin_error = units::set_units(t_score * se, "ha"),
                   lower_bound = mean_area - margin_error,
                   upper_bound = mean_area + margin_error) |>
  units::drop_units() |>
  ggplot() +
  annotate("rect",
           xmin = lubridate::date("2021-12-18"), 
           xmax = lubridate::date("2022-01-14"), 
           ymin = 0, 
           ymax = Inf,
           fill = "#9ac4fa",
           #fill = "#F0F0F0",
           alpha = 0.6) +
  annotate("rect",
           xmin = lubridate::date("2021-06-25"), 
           xmax = lubridate::date("2021-07-07"), 
           ymin = 0, 
           ymax = Inf,
           fill = "#faa44e",
           #fill = "#F0F0F0",
           alpha = 0.6) +
  geom_ribbon(aes(x = date, 
                  ymin = lower_bound,
                  ymax = upper_bound),
              fill = "#DADADA") +
  geom_line(aes(x = date,
                y = mean_area)) +
  coord_cartesian(expand = FALSE,
                  ylim=c(0, 3700)) +
  labs(title = "Mean weekly MCP area over time",
       caption = "Confidence intervals narrow over time as more samples are added.\nIn blue, the severe winter period for 2021-2022.\nIn red, the 2021 PNW Heat Dome.") +
  theme_minimal()


#### WEEKLY DBBMM PLOTS ####


tar_load(weekly_dbbmms)

# First remove outliers!! A few really funky large dBBMM blobs

# Remove weekly dBBMM outliers
# Pull out area data
dat <- sf::st_drop_geometry(weekly_dbbmms["area"])
dat <- units::drop_units(dat)
# Construct isolation forest model
if_model <- isotree::isolation.forest(dat, sample_size = 30, ndim=1, ntrees=10, nthreads=1)
# Predict outliers from non-outliers
scores <- isotree::predict.isolation_forest(if_model, dat, type="avg_depth")
# Visualize
plot(dat$area, scores)
# Choose cutoff for what counts as an 'outlier score'. 
# Choose scores that 99% of the data fall into
outlier_threshold <- quantile(scores, 0.01)[[1]]
# Add scores back to dat
dat$isolation_score <- scores
# Compare 
hist(dat$area)
hist(dat[dat$isolation_score > outlier_threshold, "area"])
max(dat[dat$isolation_score > outlier_threshold, "area"])

# So our max cutoff will be 745 hectares. 
outlier_cutoff <- ceiling(max(dat[dat$isolation_score > outlier_threshold, "area"]))
outlier_cutoff <- units::set_units(outlier_cutoff, "ha")

# Now plot stuff
weekly_dbbmms |>
  dplyr::filter(area < outlier_cutoff) |>
  dplyr::mutate(year = as.factor(isoyear),
                area = units::drop_units(area)) |>
  ggplot() +
  # Winter
  annotate("rect",
           xmin = 0, xmax = 14, ymin = 0, ymax = Inf,
           fill = "#e1eeff",
           #fill = "#F0F0F0",
           alpha = 0.6) +
  # Spring
  annotate("rect",
           xmin = 14, xmax = 21, ymin = 0, ymax = Inf,
           #fill = "#F0F0F0",
           fill = "#f7ffe1",
           alpha = 0.6) +
  # Summer
  annotate("rect",
           xmin = 27, xmax = 36, ymin = 0, ymax = Inf,
           #fill = "#F0F0F0",
           fill = "#fff4e1",
           alpha = 0.6) +
  geom_smooth(aes(x = week,
                  y = area,
                  color = year,
                  group = year),
              method = "gam",
              se = FALSE) +
  scale_color_manual(values = okabe) +
  labs(title = "Weekly dBBMM size, colored by year",
       y = "Area [ha]",
       x = "Week",
       caption = "Blue rectangle = 'Winter'\nGreen rectangle = 'Spring'\nOrange rectangle = 'Summer'") +
  theme_minimal()


weekly_dbbmms |>
  dplyr::mutate(year = isoyear,
                beginning = lubridate::ymd(paste(year, "-01-01")),
                date = beginning + lubridate::weeks(week - 1)) |>
  ggplot(aes(x = date,
             y = area,
             color = year)) +
  geom_point(alpha = 0.3) +
  labs(title = "All weekly dBBMM areas through time",
       subtitle = "") +
  theme_minimal()

weekly_dbbmms |>
  dplyr::mutate(year = isoyear,
                beginning = lubridate::ymd(paste(year, "-01-01")),
                date = beginning + lubridate::weeks(week - 1)) |>
  ggplot(aes(x = date,
             y = area)) +
  #geom_point() +
  geom_smooth(method = "loess",
              span = 0.1,
              se = FALSE) +
  theme_minimal()

weekly_dbbmms |>
  sf::st_drop_geometry() |>
  dplyr::mutate(year = isoyear,
                beginning = lubridate::ymd(paste(year, "-01-01")),
                date = beginning + lubridate::weeks(week - 1)) |>
  dplyr::group_by(date) |>
  dplyr::summarise(mean_area = mean(area),
                   n = dplyr::n(),
                   sd_area = sd(area),
                   se = sd_area/sqrt(n),
                   t_score = qt(p = 0.05/2, 
                                df = n - 1,
                                lower.tail=F),
                   margin_error = units::set_units(t_score * se, "ha"),
                   lower_bound = mean_area - margin_error,
                   upper_bound = mean_area + margin_error) |>
  units::drop_units() |>
  ggplot() +
  annotate("rect",
           xmin = lubridate::date("2021-12-18"), 
           xmax = lubridate::date("2022-01-14"), 
           ymin = 0, 
           ymax = Inf,
           fill = "#9ac4fa",
           #fill = "#F0F0F0",
           alpha = 0.6) +
  annotate("rect",
           xmin = lubridate::date("2021-06-25"), 
           xmax = lubridate::date("2021-07-07"), 
           ymin = 0, 
           ymax = Inf,
           fill = "#faa44e",
           #fill = "#F0F0F0",
           alpha = 0.6) +
  geom_ribbon(aes(x = date, 
                  ymin = lower_bound,
                  ymax = upper_bound),
              fill = "#DADADA") +
  geom_line(aes(x = date,
                y = mean_area)) +
  coord_cartesian(expand = FALSE,
                  ylim=c(0, 1200)) +
  labs(title = "Mean weekly dBBMM area over time",
       caption = "Confidence intervals narrow over time as more samples are added.\nIn blue, the severe winter period for 2021-2022.\nIn red, the 2021 PNW Heat Dome.") +
  theme_minimal()





#### WEEKLY SEVERE WINTER PERIOD PLOTS ####

# Severe winter period was 18 Dec 2021 - 14 Jan 2022
#lubridate::isoweek("2021-12-18")
#lubridate::isoweek("2022-01-14")
swp <- c(50, 51, 52, 53, 1, 2) # weeks 51 thru 1

# MCP
weekly_mcps |>
  dplyr::filter(week %in% swp) |>
  dplyr::mutate(year = isoyear) |>
  dplyr::mutate(year = dplyr::if_else(week < 3, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  ggplot(aes(x = as.factor(year), 
             y = area,
             color = as.factor(year))) +
  geom_boxplot(fill = NA) +
  geom_jitter() +
  scale_color_manual(values = okabe) +
  geom_signif(comparisons = list(c("2019-2020", "2020-2021"),
                                 c("2021-2022", "2020-2021"),
                                 c("2021-2022", "2022-2023"),
                                 c("2022-2023", "2023-2024")),
              map_signif_level = TRUE,
              color = "black",
              tip_length = 0.2) +
  geom_signif(comparisons = list(c("2021-2022", "2019-2020"),
                                 c("2021-2022", "2023-2024")),
              map_signif_level = TRUE,
              y_position = 5000,
              color = "black",
              tip_length = 0.3) +
  coord_trans(y = "log10") +
  labs(title = "Weekly MCP area during the severe winter \nperiod weeks, across years",
       subtitle = "18 Dec - 14 Jan",
       caption = "Each boxplot contains weekly MCP sizes during week 50 through week 2 (inclusive).\nEach dot is the MCP size for one individual for one week.",
       x = "Year",
       y = "Area", 
       color = "Year") +
  theme_minimal()

# dBBMM
weekly_dbbmms |>
  dplyr::filter(week %in% swp) |>
  dplyr::mutate(year = isoyear) |>
  dplyr::mutate(year = dplyr::if_else(week < 3, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  ggplot(aes(x = as.factor(year), 
             y = area,
             color = as.factor(year))) +
  geom_boxplot(fill = NA) +
  geom_jitter() +
  scale_color_manual(values = okabe) +
  geom_signif(comparisons = list(c("2019-2020", "2020-2021"),
                                 c("2021-2022", "2020-2021"),
                                 c("2021-2022", "2022-2023"),
                                 c("2022-2023", "2023-2024")),
              map_signif_level = TRUE,
              color = "black",
              tip_length = 0.2) +
  geom_signif(comparisons = list(c("2021-2022", "2019-2020"),
                                 c("2021-2022", "2023-2024")),
              map_signif_level = TRUE,
              y_position = 5000,
              color = "black",
              tip_length = 0.3) +
  coord_trans(y = "log10") +
  labs(title = "Weekly dBBMM area during the severe winter \nperiod weeks, across years",
       subtitle = "18 Dec - 14 Jan",
       caption = "Each boxplot contains weekly dBBMM sizes during week 50 through week 2 (inclusive).\nEach dot is the MCP size for one individual for one week.",
       x = "Year",
       y = "Area", 
       color = "Year") +
  theme_minimal()


#### HEAT DOME PERIOD PLOTS ####

# Heat dome was June 25 2021 - July 07 2021
hd <- c(lubridate::week("2021-06-25"):lubridate::week("2021-07-7")) # weeks 26-27

weekly_mcps |>
  dplyr::filter(week %in% hd) |>
  ggplot(aes(x = as.factor(isoyear), 
             y = area,
             color = as.factor(isoyear))) +
  geom_boxplot(fill = NA) +
  geom_jitter() +
  scale_color_manual(values = okabe) +
  geom_signif(comparisons = list(c("2019", "2020"),
                                 c("2020", "2021"),
                                 c("2021", "2022"),
                                 c("2022", "2023")),
              map_signif_level = TRUE,
              color = "black",
              tip_length = 0.1) +
  geom_signif(comparisons = list(c("2021", "2019"),
                                 c("2021", "2023")),
              map_signif_level = TRUE,
              y_position = 5000,
              color = "black",
              tip_length = 0.3) +
  coord_trans(y = "log10") +
  labs(title = "Weekly MCP area during the heat dome weeks, across years",
       subtitle = "Heat dome was 25 June 2021 - 07 July 2021 (weeks 26 and 27 of the year)",
       caption = "Each boxplot contains weekly MCP sizes during weeks 26-27.\nEach dot is the MCP size for one individual for one week.",
       x = "Year",
       y = "Area", 
       color = "Year") +
  theme_minimal()

weekly_dbbmms |>
  dplyr::filter(week %in% hd) |>
  ggplot(aes(x = as.factor(isoyear), 
             y = area,
             color = as.factor(isoyear))) +
  geom_boxplot(fill = NA) +
  geom_jitter() +
  scale_color_manual(values = okabe) +
  geom_signif(comparisons = list(c("2019", "2020"),
                                 c("2020", "2021"),
                                 c("2021", "2022"),
                                 c("2022", "2023")),
              map_signif_level = TRUE,
              color = "black",
              tip_length = 0.1) +
  geom_signif(comparisons = list(c("2021", "2019"),
                                 c("2021", "2023")),
              map_signif_level = TRUE,
              y_position = 2500,
              color = "black",
              tip_length = 0.3) +
  coord_trans(y = "log10") +
  labs(title = "Weekly dBBMM area during the heat dome weeks, across years",
       subtitle = "Heat dome was 25 June 2021 - 07 July 2021 (weeks 26 and 27 of the year)",
       caption = "Each boxplot contains weekly dBBMM sizes during weeks 26-27.\nEach dot is the MCP size for one individual for one week.",
       x = "Year",
       y = "Area", 
       color = "Year") +
  theme_minimal()



#### DAILY MCP SUMMARY PLOTS ####

tar_load(daily_mcps)

daily_mcps$doy <- lubridate::yday(daily_mcps$date)

daily_mcps |>
  dplyr::mutate(year = as.factor(year),
                area = units::drop_units(area)) |>
  ggplot() +
  geom_smooth(aes(x = doy,
                  y = area,
                  color = year,
                  group = year),
              method = "loess",
              se = FALSE) +
  scale_color_manual(values = okabe) +
  labs(title = "Daily MCP size, colored by year",
       y = "Area [ha]",
       x = "Day of Year",
       caption = "Blue rectangle = 'Winter'\nGreen rectangle = 'Spring'\nOrange rectangle = 'Summer'") +
  theme_minimal()


# daily_mcps |>
#   dplyr::mutate(area = units::drop_units(area)) |>
#   ggplot(aes(x = date,
#              y = area,
#              #color = year,
#              group = date)) +
#   geom_boxplot(outlier.shape = NA) +
#   scale_y_continuous(limits = c(0, 450)) +
#   #facet_wrap(~year, ncol = 1) +
#   labs(title = "Mean daily MCP size through time",
#        subtitle = "Outliers removed") +
#   theme_minimal()


daily_mcps |>
  sf::st_drop_geometry() |>
  ggplot(aes(x = date, y = area)) +
  geom_point(aes(color = year),
             alpha = 0.3) + 
  #geom_smooth(span = 0.1) +
  labs(title = "All daily MCP areas through time",
       subtitle = "Including outliers") +
  theme_minimal()


daily_mcps |>
  sf::st_drop_geometry() |>
  dplyr::group_by(date) |>
  dplyr::summarise(mean_area = mean(area),
                   n = dplyr::n(),
                   sd_area = sd(area),
                   se = sd_area/sqrt(n),
                   t_score = qt(p = 0.05/2, 
                                df = n - 1,
                                lower.tail=F),
                   margin_error = units::set_units(t_score * se, "ha"),
                   lower_bound = mean_area - margin_error,
                   upper_bound = mean_area + margin_error) |>
  units::drop_units() |>
  ggplot() +
  annotate("rect",
           xmin = lubridate::date("2021-12-18"), 
           xmax = lubridate::date("2022-01-14"), 
           ymin = 0, 
           ymax = Inf,
           fill = "#c1dcfe",
           #fill = "#F0F0F0",
           alpha = 0.6) +
  annotate("rect",
           xmin = lubridate::date("2021-06-25"), 
           xmax = lubridate::date("2021-07-07"), 
           ymin = 0, 
           ymax = Inf,
           fill = "#f9c56b",
           #fill = "#F0F0F0",
           alpha = 0.6) +
  geom_ribbon(aes(x = date, 
                  ymin = lower_bound,
                  ymax = upper_bound),
              fill = "#DADADA") +
  geom_line(aes(x = date,
                y = mean_area)) +
  coord_cartesian(expand = FALSE,
                  ylim=c(0, 300)) +
  labs(title = "Mean daily MCP area over time",
       caption = "Confidence intervals narrow over time as more samples are added.\nIn blue, the severe winter period for 2021-2022.\nIn red, the 2021 PNW Heat Dome.") +
  theme_minimal()


#### DAILY SEVERE WINTER PERIOD PLOTS ####


# Severe winter period was 18 Dec 2021 - 14 Jan 2022
#lubridate::yday("2021-12-18")
#lubridate::yday("2022-01-14")
swp <- c(lubridate::yday("2021-12-18"):max(daily_mcps$doy), 1:14)

daily_mcps |>
  dplyr::filter(doy %in% swp) |>
  dplyr::mutate(year = dplyr::if_else(doy < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  #units::drop_units() |>
  ggplot(aes(x = as.factor(year), 
             y = area,
             color = as.factor(year))) +
  geom_boxplot(fill = NA) +
  geom_jitter(alpha = 0.3,
              stroke = NA) +
  scale_color_manual(values = okabe) +
  geom_signif(comparisons = list(c("2019-2020", "2020-2021"),
                                 c("2021-2022", "2020-2021"),
                                 c("2021-2022", "2022-2023"),
                                 c("2022-2023", "2023-2024")),
              map_signif_level = TRUE,
              color = "black",
              tip_length = 0.3) +
  geom_signif(comparisons = list(c("2021-2022", "2019-2020"),
                                 c("2021-2022", "2023-2024")),
              map_signif_level = TRUE,
              y_position = 3000,
              color = "black",
              tip_length = 1.5) +
  geom_signif(comparisons = list(c("2020-2021", "2022-2023")),
              map_signif_level = TRUE,
              y_position = 8000,
              color = "black",
              tip_length = 5) +
  geom_signif(comparisons = list(c("2019-2020", "2023-2024")),
              map_signif_level = TRUE,
              y_position = 15000,
              color = "black",
              tip_length = 5) +
  coord_trans(y = "log10") +
  labs(title = "Daily MCP area during the severe winter \nperiod weeks, across years",
       subtitle = "18 Dec - 14 Jan",
       caption = "Each boxplot contains daily MCP sizes from Dec 18-Jan 14 (inclusive) for each year.\nEach dot is the MCP size for one individual for one day.",
       x = "Year",
       y = "Area", 
       color = "Year") +
  theme_minimal()



daily_mcps |>
  dplyr::filter(doy %in% swp) |>
  dplyr::mutate(year = dplyr::if_else(doy < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  units::drop_units() |>
  oneway.test(formula = area ~ year, var.equal = TRUE)



#### DAILY HEAT DOME PLOTS ####


# Severe winter period was 25 June 2021 - 07 July 2021
#lubridate::yday("2021-06-25")
#lubridate::yday("2021-07-07")
hd <- c(lubridate::yday("2021-06-25"):lubridate::yday("2021-07-07"))

daily_mcps |>
  dplyr::filter(doy %in% hd) |>
  #units::drop_units() |>
  ggplot(aes(x = as.factor(year), 
             y = area,
             color = as.factor(year))) +
  geom_boxplot(fill = NA) +
  geom_jitter(alpha = 0.3,
              stroke = NA) +
  scale_color_manual(values = okabe) +
  geom_signif(comparisons = list(c("2019", "2020"),
                                 c("2021", "2020"),
                                 c("2021", "2022"),
                                 c("2022", "2023")),
              map_signif_level = TRUE,
              color = "black",
              tip_length = 0.3) +
  geom_signif(comparisons = list(c("2021", "2019"),
                                 c("2021", "2023")),
              map_signif_level = TRUE,
              y_position = 3000,
              color = "black",
              tip_length = 1.5) +
  geom_signif(comparisons = list(c("2020", "2022")),
              map_signif_level = TRUE,
              y_position = 8000,
              color = "black",
              tip_length = 5) +
  geom_signif(comparisons = list(c("2019", "2023")),
              map_signif_level = TRUE,
              y_position = 15000,
              color = "black",
              tip_length = 5) +
  coord_trans(y = "log10") +
  labs(title = "Daily MCP area during the heat dome days, across years",
       subtitle = "25 June - 07 July",
       caption = "Each boxplot contains daily MCP sizes from June 25-July 07 (inclusive) for each year.\nEach dot is the MCP size for one individual for one day.",
       x = "Year",
       y = "Area", 
       color = "Year") +
  theme_minimal()





