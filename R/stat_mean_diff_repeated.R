#' This is the documentation for stat_catseye. remember to account for missing data
#' @param mapping asdfasd
#' @param data adfasd
#' @param geom asdfasd
#' @param position asdfasd
#' @param na.rm adsfasdf
#' @param show.legend asdfads
#' @param inherit.aes adfasd
#' @param group1 asdsf
#' @param group2 asdsf
#' @param width asdf
#' @param ... adfasdf
#' @export
stat_mean_diff_repeated <- function(mapping = NULL, data = NULL, geom = "point",
                           position = "identity", na.rm = FALSE, show.legend = NA,
                           inherit.aes = TRUE, group1 = NA, group2 = NA, alternative = "two.sided", text_size = 3, ...) {

  list(
    ggplot2::layer(
      stat = DiffScores, data = data, mapping = mapping, geom = "point",
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, group1 = group1, group2 = group2, alpha = .50, shape = 17,...)
    ),
    ggplot2::layer(
      stat = InsetPoints, data = data, mapping = mapping, geom = "point",
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, group1 = group1, group2 = group2, alpha = .50, ...)
    ),
    ggplot2::layer(
      stat = InsetLines, data = data, mapping = mapping, geom = "line",
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, group1 = group1, group2 = group2, alpha = .50, size = .3, ...)
    ),
    ggplot2::layer(
      stat = InternalWhiskers, data = data, mapping = mapping, geom = "errorbar",
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, width = .08, ...)
    ),
    ggplot2::layer(
      stat = MeanPoints, data = data, mapping = mapping, geom = "point",
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, width = .08, size = 3, ...)
    ),
    ggplot2::layer(
      stat = MeanLine, data = data, mapping = mapping, geom = "line",
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, size = 1.1, ...)
    ),
    ggplot2::layer(
      stat = MeanDiffLineGroup1, data = data, mapping = mapping, geom = "line",
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, linetype = 2, group1 = group1, group2 = group2, ...)
    ),
    ggplot2::layer(
      stat = MeanDiffLineGroup2, data = data, mapping = mapping, geom = "line",
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, linetype = 2, group1 = group1, group2 = group2, ...)
    ),
    ggplot2::layer(
      stat = DiffScale, data = data, mapping = mapping, geom = "segment",
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, group1 = group1, group2 = group2, alternative = alternative,...)
    ),
    ggplot2::layer(
      stat = DiffScaleLabels, data = data, mapping = mapping, geom = "text",
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, group1 = group1, group2 = group2, alternative = alternative, size = text_size, ...)
    ),
    ggplot2::layer(
      stat = ExtendXAxis, data = data, mapping = mapping, geom = "point",
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, group1 = group1, group2 = group2, alpha = 0,...)
    )
  )
}


inset_points  <- function(data) {
  is1 <- data$x == 1
  is2 <- data$x == 2

  data$x[is1] <- 1.3
  data$x[is2] <- 1.7

  data
}


create_diff_data <- function(data, scales, level) {
  # x has two levels
  # y has two levels

  id_cond1_value <- unique(data$x)[1]
  id_cond2_value <- unique(data$x)[2]

  id_for_cond1 <- data$x == id_cond1_value
  id_for_cond2 <- data$x == id_cond2_value

  condition1 <- data$y[id_for_cond1]
  condition2 <- data$y[id_for_cond2]
  diff <- condition1 - condition2

  mean_condition1 <- mean(condition1, na.rm = TRUE)
  mean_condition2 <- mean(condition2, na.rm = TRUE)

  min_mean <- min(mean_condition1, mean_condition2)

  diff_plus_mean <- diff + min_mean
  L <- length(diff)
  PANEL <- rep(data$PANEL[1], L)
  group = 1

  diff_x <-  rep(id_cond2_value+ .7, L)
  diff_x <- jitter(diff_x, 1)

  new_data <-data.frame(x = diff_x, y = diff_plus_mean, PANEL = PANEL, group = group)

  print("new data")
  print(new_data)
  return(new_data)
}


DiffScores<- ggplot2::ggproto("DiffScores", ggplot2::Stat,
                            required_aes = c("x", "y"),

                            compute_panel = function(data, scales, level) {

                              data  <- create_diff_data(data, scales, level)
                              print(data)
                              data
                            }
)



InsetLines<- ggplot2::ggproto("InsetLines", ggplot2::Stat,
                            required_aes = c("x", "y"),

                            compute_panel = function(data, scales, level) {
                              data  <- inset_points(data)
                              print(data)
                              data
                            }
)

InsetPoints<- ggplot2::ggproto("InsetPoints", ggplot2::Stat,
                              required_aes = c("x", "y"),

                              compute_panel = function(data, scales, level) {

                                data  <- inset_points(data)
                                data
                              }
)

InternalWhiskers<- ggplot2::ggproto("InternalWhiskers", ggplot2::Stat,
                               required_aes = c("x", "y"),

                               compute_panel = function(data, scales, level, group1, group2) {

                                 data  <- get_summary_df(data)
                                 print(data)
                                 data
                               }
)


MeanPoints<- ggplot2::ggproto("MeanPoints", ggplot2::Stat,
                                   required_aes = c("x", "y"),

                                   compute_panel = function(data, scales, level, group1, group2) {

                                     data  <- get_summary_df(data)
                                     data
                                   }
)

MeanLine<- ggplot2::ggproto("MeanLine", ggplot2::Stat,
                              required_aes = c("x", "y"),

                              compute_panel = function(data, scales, level, group1, group2) {

                                data  <- get_summary_df(data)
                                data
                              }
)

get_summary_df <-function(data) {
  print(data)
  data_group <- dplyr::group_by(data, .data$x)
  data_out <- dplyr::summarise(data_group,
                               ym = mean(.data$y, na.rm = TRUE),
                               ymin = stats::t.test(.data$y)$conf.int[1],
                               ymax = stats::t.test(.data$y)$conf.int[2])
  data_out <- dplyr::rename(data_out, y = .data$ym)
  data_out$group <- c(1,1)
  data_out$PANEL <- data$PANEL[1]
  data_out
}
