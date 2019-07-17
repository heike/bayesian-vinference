plot_names <- c("plain","color", "shape", "colorShape", "colorEllipse", "colorShapeEllipse", "trend", "trendError", "colorTrend", "colorEllipseTrendError")
fix_plot_names <- function(x) {
  x %>%
    str_replace("color", "Color + ") %>%
    str_replace("[sS]hape", "Shape + ") %>%
    str_replace("[tT]rend", "Trend + ") %>%
    str_replace("Ellipse", "Ellipse + ") %>%
    str_replace("Error", "Error + ") %>%
    str_replace("plain", "Plain") %>%
    str_replace("( \\+ )$", "") %>%
    str_replace_all(c("Color" = "C", "Shape" = "S", "Ellipse" = "L", "Trend" = "T", "Error" = "E")) %>%
    str_replace_all(" \\+ ", "+") %>%
    factor(levels = c("T", "T+E", "Plain", "S", "C", "C+S", "C+L", "C+S+L", "C+T", "C+L+T+E"), ordered = T)
}

source("code/GenerateLineups.R")
