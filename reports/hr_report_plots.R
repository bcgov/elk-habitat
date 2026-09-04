# Copyright 2026 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

library(targets)
library(ggplot2)
library(ggsignif)

non_swp_elk <- c("20-1001", "20-1002")
swp_dates <- seq(lubridate::ymd("2021-12-18"), lubridate::ymd("2022-01-14"), by = "1 day")
swp_weeks <- c(50, 51, 52, 53, 1, 2) 
swp_days <- c(lubridate::yday("2021-12-18"):366, 1:14)


# Weekly SWP MCP
tar_load(weekly_mcps)

weekly_mcps |>
  units::drop_units() |>
  dplyr::filter(isoyear_week < 2024.4) |>
  dplyr::filter(!animal_id %in% non_swp_elk) |>
  dplyr::filter(week %in% swp_weeks) |>
  dplyr::mutate(year = isoyear) |>
  dplyr::mutate(year = dplyr::if_else(week < 3, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  dplyr::mutate(swp = (year == "2021-2022"),
                year = ifelse(swp,
                              paste0(year, "\n(severe)"),
                              paste0(year, "\n(mild)"))) |>
  sf::st_drop_geometry() |>
  ggplot(aes(x = as.factor(year), 
             y = area,
             color = as.factor(swp))) +
  geom_jitter(alpha = 0.6, 
              pch = 16) +
  geom_boxplot(fill = NA, 
               outliers = FALSE,
               linewidth = 0.8) +
  scale_color_manual(values = c("grey10", "grey40")) +
  coord_transform(y = "log10") +
  labs(x = "",
       y = "Hectares") +
  theme_minimal() +
  theme(legend.position = "none")


# Daily SWP MCP
tar_load(daily_mcps)

daily_mcps |>
  units::drop_units() |>
  dplyr::filter(date < "2024-01-15") |>
  dplyr::filter(!animal_id %in% non_swp_elk) |>
  dplyr::mutate(doy = lubridate::yday(date)) |>
  dplyr::filter(doy %in% swp_days) |>
  dplyr::mutate(year = dplyr::if_else(doy < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  dplyr::mutate(swp = (year == "2021-2022"),
                year = ifelse(swp,
                              paste0(year, "\n(severe)"),
                              paste0(year, "\n(mild)"))) |>
  sf::st_drop_geometry() |>
  ggplot(aes(x = as.factor(year), 
             y = area,
             color = as.factor(swp))) +
  geom_jitter(alpha = 0.6, 
              pch = 16) +
  geom_boxplot(fill = NA, 
               outliers = FALSE,
               linewidth = 0.8) +
  scale_color_manual(values = c("grey10", "grey40")) +
  coord_transform(y = "log10") +
  labs(x = "",
       y = "Hectares") +
  theme_minimal() +
  theme(legend.position = "none")


# Step lengths SWP
tar_load(step_lengths_3hr)


step_lengths_3hr |>
  dplyr::filter(step > 0) |> # avoid log10 plotting issues
  dplyr::filter(lubridate::date(dttm) < "2024-01-15") |>
  dplyr::filter(severe_winter_yn == TRUE) |>
  dplyr::mutate(year = lubridate::year(dttm)) |>
  dplyr::mutate(year = dplyr::if_else(lubridate::month(dttm) == 12,
                                      year + 1,
                                      year)) |>
  dplyr::mutate(season = paste0(year - 1, "-", year)) |>
  dplyr::mutate(swp = (season == "2021-2022"),
                season = ifelse(swp,
                                paste0(season, "\n(severe)"),
                                paste0(season, "\n(mild)"))) |>
  ggplot(aes(x = as.factor(season), 
             y = step,
             color = as.factor(swp))) +
  geom_jitter(alpha = 0.6, 
              pch = 20) +
  geom_boxplot(fill = NA, 
               outliers = FALSE,
               linewidth = 0.8,
               color = "orange2") +
  scale_color_manual(values = c("grey10", "grey40")) +
  coord_transform(y = "log10") +
  labs(x = "",
       y = "Step-length (m)") +
  theme_minimal() +
  theme(legend.position = "none")


# Daily SWP centroid displacement
tar_load(daily_step)

daily_step |>
  units::drop_units() |>
  dplyr::filter(date < "2024-01-15") |>
  dplyr::filter(!animal_id %in% non_swp_elk) |>
  dplyr::mutate(doy = lubridate::yday(date)) |>
  dplyr::filter(doy %in% swp_days) |>
  dplyr::mutate(year = dplyr::if_else(doy < 15, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  dplyr::mutate(swp = (year == "2021-2022"),
                year = ifelse(swp,
                              paste0(year, "\n(severe)"),
                              paste0(year, "\n(mild)"))) |>
  ggplot(aes(x = as.factor(year), 
             y = centroid_dist,
             color = as.factor(swp))) +
  geom_jitter(alpha = 0.6, 
              pch = 16) +
  geom_boxplot(fill = NA, 
               outliers = FALSE,
               linewidth = 0.8) +
  scale_color_manual(values = c("grey10", "grey40")) +
  coord_transform(y = "log10") +
  labs(x = "",
       y = "Step-length (m)") +
  theme_minimal() +
  theme(legend.position = "none")


# Weekly SWP centroid displacement
tar_load(weekly_step)

weekly_step |>
  units::drop_units() |>
  dplyr::filter(isoyear_week < 2024.4) |>
  dplyr::filter(!animal_id %in% non_swp_elk) |>
  dplyr::filter(week %in% swp_weeks) |>
  dplyr::mutate(year = isoyear) |>
  dplyr::mutate(year = dplyr::if_else(week < 3, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |>
  dplyr::mutate(swp = (year == "2021-2022"),
                year = ifelse(swp,
                              paste0(year, "\n(severe)"),
                              paste0(year, "\n(mild)"))) |>
  ggplot(aes(x = as.factor(year), 
             y = centroid_dist,
             color = as.factor(swp))) +
  geom_jitter(alpha = 0.6, 
              pch = 16) +
  geom_boxplot(fill = NA, 
               outliers = FALSE,
               linewidth = 0.8) +
  scale_color_manual(values = c("grey10", "grey40")) +
  coord_transform(y = "log10") +
  labs(x = "",
       y = "Step-length (m)") +
  theme_minimal() +
  theme(legend.position = "none")


# Buy why tho?
weekly_step |>
  units::drop_units() |>
  dplyr::filter(isoyear_week < 2024.4) |>
  dplyr::filter(!animal_id %in% non_swp_elk) |>
  dplyr::filter(week %in% swp_weeks) |>
  dplyr::mutate(year = isoyear) |>
  dplyr::mutate(year = dplyr::if_else(week < 3, year-1, year)) |>
  dplyr::mutate(year = paste0(year, "-", year+1)) |> 
  dplyr::mutate(swp = (year == "2021-2022")) |>
  dplyr::group_by(animal_id, year, method) |>
  dplyr::mutate(nweek = 1:dplyr::n()) |>
  dplyr::mutate(nweek = ifelse(nweek == 6, 5, nweek)) |> # compress the one year that has a 53rd week (not entirely accurate to do it this way but works in a pinch)
  ggplot(aes(x = nweek,
             y = centroid_dist,
             color = as.factor(year),
             group = year)) +
  geom_smooth(se = FALSE) +
  scale_color_manual(values = c("#CC79A7", "#E69F00", "#56B4E9", "#009E73", "#F0E442")) +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5),
                     labels = c(50, 51, 52, 1, 2))

