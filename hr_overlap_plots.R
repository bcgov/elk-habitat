#### SETUP ####

# Wishlist
# None for now

# Libraries
library(targets)
library(sf)
library(ggplot2)
library(ggsignif)
library(units)

# Color palette
okabe <- c("#CC79A7", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00")

# Load data
tar_load(mcp_winter_spring_overlap)
tar_load(dbbmm_winter_spring_overlap)

tar_load(mcp_winter_yearly_overlap)
tar_load(dbbmm_winter_yearly_overlap)


# Prep data as needed

# Drop units... good for analysis, annoying to deal with in plots
mcp_winter_spring_overlap <- units::drop_units(mcp_winter_spring_overlap)
dbbmm_winter_spring_overlap <- units::drop_units(dbbmm_winter_spring_overlap)
mcp_winter_yearly_overlap <- units::drop_units(mcp_winter_yearly_overlap)
dbbmm_winter_yearly_overlap <- units::drop_units(dbbmm_winter_yearly_overlap)

# Trim out any winter-spring overlap where the area of either winter or spring = 0
# i.e., no polygon available for one of the seasons
mcp_winter_spring_overlap <- mcp_winter_spring_overlap[which(mcp_winter_spring_overlap$winter_area > 0 & mcp_winter_spring_overlap$spring_area > 0), ]
dbbmm_winter_spring_overlap <- dbbmm_winter_spring_overlap[which(dbbmm_winter_spring_overlap$winter_area > 0 & dbbmm_winter_spring_overlap$spring_area > 0), ]

# Add method column
mcp_winter_spring_overlap$method <- "MCP"
dbbmm_winter_spring_overlap$method <- "dBBMM"

mcp_winter_yearly_overlap$method <- "MCP"
dbbmm_winter_yearly_overlap$method <- "dBBMM"

# Remove yearly overlap where the gap between the two years is >1 year
mcp_winter_yearly_overlap <- mcp_winter_yearly_overlap[(mcp_winter_yearly_overlap$year_2 - mcp_winter_yearly_overlap$year_1) == 1, ]
dbbmm_winter_yearly_overlap <- dbbmm_winter_yearly_overlap[(dbbmm_winter_yearly_overlap$year_2 - dbbmm_winter_yearly_overlap$year_1) == 1, ]


#### WINTER TO SPRING PLOTS ####

length(unique(mcp_winter_spring_overlap$animal_id))
length(unique(dbbmm_winter_spring_overlap$animal_id)) # 52 unique elk had enough data to compare winter-spring overlap for both methods.

hist(mcp_winter_spring_overlap$prct_spring_within_winter, main = "MCP - Spring within Winter % Overlap", xlab = "% Spring within Winter")
hist(mcp_winter_spring_overlap$prct_winter_within_spring, main = "MCP - Winter within Spring % Overlap", xlab = "% Winter within Spring")

hist(dbbmm_winter_spring_overlap$prct_spring_within_winter, main = "dBBMM - Spring within Winter % Overlap", xlab = "% Spring within Winter")
hist(dbbmm_winter_spring_overlap$prct_winter_within_spring, main = "dBBMM - Winter within Spring % Overlap", xlab = "% Winter within Spring")

# Perhaps we can condense the frequency plots into one...
dplyr::bind_rows(mcp_winter_spring_overlap, dbbmm_winter_spring_overlap) |>
  tidyr::pivot_longer(cols = c(prct_spring_within_winter, prct_winter_within_spring),
                      values_to = "prct_overlap",
                      names_to = "prct_direction") |>
  dplyr::mutate(prct_direction = stringr::str_replace_all(prct_direction, "_", " "),
                prct_direction = stringr::str_replace(prct_direction, "prct", "%"),
                prct_direction = stringr::str_replace(prct_direction, "spring", "Spring"),
                prct_direction = stringr::str_replace(prct_direction, "winter", "Winter")) |>
  dplyr::mutate(group = paste(method, "-", prct_direction)) |>
  ggplot() +
  geom_density(aes(x = prct_overlap,
                   color = group,
                   fill = group),
               alpha = 0.1) +
  scale_color_manual(values = okabe[1:4]) +
  scale_fill_manual(values = okabe[1:4]) +
  labs(x = "% Overlap",
       y = "Density",
       color = "Group",
       fill = "Group") +
  theme_minimal()


# MCP
ggplot(mcp_winter_spring_overlap,
       aes(x = as.factor(year),
           y = prct_spring_within_winter)) +
  geom_boxplot(fill = NA) +
  geom_point(aes(color = animal_id),
             position = position_dodge(0.1)) +
  geom_line(aes(group = animal_id,
                color = animal_id),
            alpha = 0.3) +
  labs(title = "MCP Percent Overlap",
       subtitle = "Spring area within Winter",
       x = "Year",
       y = "% Spring within Winter",
       caption = "Paired dots connect % overlap of a single animal id through time.") +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(mcp_winter_spring_overlap,
       aes(x = as.factor(year),
           y = prct_winter_within_spring)) +
  geom_boxplot(fill = NA) +
  geom_point(aes(color = animal_id),
             position = position_dodge(0.1)) +
  geom_line(aes(group = animal_id,
                color = animal_id),
            alpha = 0.3) +
  labs(title = "MCP Percent Overlap",
       subtitle = "Winter area within Spring",
       x = "Year",
       y = "% Winter within Spring",
       caption = "Paired dots connect % overlap of a single animal id through time.") +
  theme_minimal() +
  theme(legend.position = "none")

# dBBMM
ggplot(dbbmm_winter_spring_overlap,
       aes(x = as.factor(year),
           y = prct_spring_within_winter)) +
  geom_boxplot(fill = NA) +
  geom_point(aes(color = animal_id),
             position = position_dodge(0.1)) +
  geom_line(aes(group = animal_id,
                color = animal_id),
            alpha = 0.3) +
  labs(title = "dBBMM Percent Overlap",
       subtitle = "Spring area within Winter",
       x = "Year",
       y = "% Spring within Winter",
       caption = "Paired dots connect % overlap of a single animal id through time.") +
  scale_y_continuous(limits = c(0, 100)) +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(dbbmm_winter_spring_overlap,
       aes(x = as.factor(year),
           y = prct_winter_within_spring)) +
  geom_boxplot(fill = NA) +
  geom_point(aes(color = animal_id),
             position = position_dodge(0.1)) +
  geom_line(aes(group = animal_id,
                color = animal_id),
            alpha = 0.3) +
  labs(title = "dBBMM Percent Overlap",
       subtitle = "Winter area within Spring",
       x = "Year",
       y = "% Winter within Spring",
       caption = "Paired dots connect % overlap of a single animal id through time.") +
  theme_minimal() +
  theme(legend.position = "none")




#### YEAR-TO-YEAR WINTER PLOTS ####


# Frequency over % overlap
# Not sure that this is saying anything meaningful
dplyr::bind_rows(mcp_winter_yearly_overlap, dbbmm_winter_yearly_overlap) |>
  units::drop_units() |>
  tidyr::pivot_longer(cols = c(prct_year_1_within_year_2, prct_year_2_within_year_1),
                      values_to = "prct_overlap",
                      names_to = "prct_direction") |>
  dplyr::mutate(prct_direction = stringr::str_replace_all(prct_direction, "_", " "),
                prct_direction = stringr::str_replace(prct_direction, "prct", "%")) |>
  dplyr::mutate(group = paste(method, "-", prct_direction)) |>
  ggplot() +
  geom_density(aes(x = prct_overlap,
                   color = group,
                   fill = group),
               alpha = 0.1) +
  scale_color_manual(values = okabe[1:4]) +
  scale_fill_manual(values = okabe[1:4]) +
  labs(x = "% Overlap",
       y = "Density",
       color = "Group",
       fill = "Group") +
  theme_minimal()


# MCP
ggplot(mcp_winter_yearly_overlap,
       aes(x = year_to_year, 
           y = prct_year_1_within_year_2)) +
  geom_boxplot(fill = NA) +
  geom_point(aes(color = animal_id),
             position = position_dodge(0.1)) +
  geom_line(aes(group = animal_id,
                color = animal_id),
            alpha = 0.3) +
  labs(title = "MCP Percent Overlap",
       subtitle = "Winter - Previous Year within Next Year",
       x = "Year Pair",
       y = "% Year 1 within Year 2",
       caption = "Paired dots connect % overlap of a single animal id through time.") +
  theme_minimal() +
  theme(legend.position = "none")

mcp_winter_yearly_overlap |>
  dplyr::mutate(year_to_year = paste0(year_2, "-", year_1)) |>
  ggplot(aes(x = year_to_year, 
             y = prct_year_2_within_year_1)) +
  geom_boxplot(fill = NA) +
  geom_point(aes(color = animal_id),
             position = position_dodge(0.1)) +
  geom_line(aes(group = animal_id,
                color = animal_id),
            alpha = 0.3) +
  labs(title = "MCP Percent Overlap",
       subtitle = "Winter - Next Year within Previous Year",
       x = "Year Pair",
       y = "% Year 2 within Year 1",
       caption = "Paired dots connect % overlap of a single animal id through time.") +
  theme_minimal() +
  theme(legend.position = "none")

# dBBMM
ggplot(dbbmm_winter_yearly_overlap,
       aes(x = year_to_year, 
           y = prct_year_1_within_year_2)) +
  geom_boxplot(fill = NA) +
  geom_point(aes(color = animal_id),
             position = position_dodge(0.1)) +
  geom_line(aes(group = animal_id,
                color = animal_id),
            alpha = 0.3) +
  labs(title = "dBBMM Percent Overlap",
       subtitle = "Winter - Previous Year within Next Year",
       x = "Year Pair",
       y = "% Year 1 within Year 2",
       caption = "Paired dots connect % overlap of a single animal id through time.") +
  theme_minimal() +
  theme(legend.position = "none")

dbbmm_winter_yearly_overlap |>
  dplyr::mutate(year_to_year = paste0(year_2, "-", year_1)) |>
  ggplot(aes(x = year_to_year, 
             y = prct_year_2_within_year_1)) +
  geom_boxplot(fill = NA) +
  geom_point(aes(color = animal_id),
             position = position_dodge(0.1)) +
  geom_line(aes(group = animal_id,
                color = animal_id),
            alpha = 0.3) +
  labs(title = "dBBMM Percent Overlap",
       subtitle = "Winter - Next Year within Previous Year",
       x = "Year Pair",
       y = "% Year 2 within Year 1",
       caption = "Paired dots connect % overlap of a single animal id through time.") +
  theme_minimal() +
  theme(legend.position = "none")



mean(c(mcp_winter_yearly_overlap$prct_year_1_within_year_2, 
       mcp_winter_yearly_overlap$prct_year_2_within_year_1, 
       dbbmm_winter_yearly_overlap$prct_year_1_within_year_2, 
       dbbmm_winter_yearly_overlap$prct_year_2_within_year_1))

sd(c(mcp_winter_yearly_overlap$prct_year_1_within_year_2, 
       mcp_winter_yearly_overlap$prct_year_2_within_year_1, 
       dbbmm_winter_yearly_overlap$prct_year_1_within_year_2, 
       dbbmm_winter_yearly_overlap$prct_year_2_within_year_1))











