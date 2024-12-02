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
    ggplot2::theme(legend.position = "none")
  
  return(p)
}