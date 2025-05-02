# PLOTTING FXNS

# Elk data visualizations

logger_dotplot <- function(elk) {
  elk$collar_id <- as.factor(elk$collar_id)
  p <- ggplot2::ggplot(elk, 
                       ggplot2::aes(x = dttm, 
                                    y = animal_id,
                                    color = collar_id)
                       ) +
    ggplot2::geom_point() +
    ggplot2::scale_x_datetime(labels = scales::date_format("%b %y"),
                              date_breaks = "3 months") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none",
                   axis.title = element_blank(),
                   axis.ticks.x = element_line(color = "black"),
                   axis.text.x = element_text(size = 7, angle = 90))
  
  #ggsave("temp/elk_dotplot.png", p, width = 7, height = 9, bg = "#ffffff")
  
  return(p)
}


# N elk tracked through time (year-month)
elk_per_year_month_plot <- function(elk) {
  p <- elk |> 
    sf::st_drop_geometry() |>
    dplyr::group_by(year, month) |>
    dplyr::summarise(N = dplyr::n_distinct(animal_id)) |>
    dplyr::mutate(year_month = lubridate::ym(paste0(year, "-", month))) |>
    ggplot2::ggplot(ggplot2::aes(x = year_month, y = N)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::labs(y = "N elk tracked") +
    ggplot2::theme_minimal() +
    ggplot2::scale_x_date(date_breaks = "2 months", 
                          date_labels = "%b-%y", 
                          limits = c(lubridate::ym("2019-01"), NA),
                          expand = c(0, 20)) +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_line(color = "black"),
                   axis.text.x = ggplot2::element_text(size = 7, angle = 90))
  return(p)
}


# N elk tracked through time (month)
elk_per_month_plot <- function(elk) {
  p <- 
    elk |> 
    sf::st_drop_geometry() |>
    dplyr::group_by(month) |>
    dplyr::summarise(N = dplyr::n_distinct(animal_id)) |>
    ggplot2::ggplot(ggplot2::aes(x = month, y = N)) +
    ggplot2::geom_col() +
    ggplot2::geom_label(ggplot2::aes(label = N)) +
    ggplot2::labs(y = "N elk tracked",
                  caption = "Within each month, across all years") +
    ggplot2::theme_minimal() +
    ggplot2::scale_x_continuous(breaks = 1:12,
                                labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                                expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(0, 85),
                                expand = c(0, 0)) +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_line(color = "black"))
  return(p)
}

elk_dets_hist <- function(elk) {
  p <- elk |>
    sf::st_drop_geometry() |>
    dplyr::group_by(animal_id, collar_id) |>
    dplyr::summarise(N_dets = dplyr::n()) |>
    ggplot2::ggplot(ggplot2::aes(x = N_dets)) +
    ggplot2::geom_histogram() +
    ggplot2::labs(x = "Number of detections per elk deployment",
                  y = "Frequency") +
    ggplot2::theme_light()
  return(p)
}

elk_fix_hist <- function(fix_rate) {
  p <- fix_rate |>
    ggplot2::ggplot(ggplot2::aes(x = efficiency)) +
    ggplot2::geom_histogram() +
    ggplot2::labs(x = "Detection efficiency (fix success)",
                  y = "Frequency") +
    ggplot2::theme_light()
  return(p)
}

