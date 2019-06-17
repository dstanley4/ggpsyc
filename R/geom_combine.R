#' Create a plot for independent groups t-test as per Introduction to the New Statistics
#' @export
geom_two_group_between <- function() {
  list(
    stat_two_group_ci(paired = FALSE),
    ggplot2::stat_summary(fun.data = mean_cl_normal,
                 geom = "errorbar",
                 width = .2),
    ggplot2::stat_summary(fun.data = mean_cl_normal,
                 geom = "point",
                 size = 3),
    stat_offset_points(position = ggplot2::position_jitter(width = .1))
  )
  # WARNING FIX ABOVE LINE no alcohol
  #ggplot2::geom_point(mapping = ggplot2::aes(as.numeric(alcohol) - 0.25),
  #                    position = ggplot2::position_jitter(width = .1))
}

#' Create a plot for paired groups t-test as per Introduction to the New Statistics
#' @export
geom_two_group_within <- function() {
  list(
    ggplot2::stat_summary(fun.data = mean_cl_normal,
                 geom = "errorbar",
                 width = .2,
                 group = 1),
    ggplot2::stat_summary(fun.data = mean_cl_normal,
                 geom = "point",
                 size = 3,
                 group = 1),
    ggplot2::stat_summary(fun.data = mean_cl_normal,
                 geom = "line",
                 size = 1,
                 group = 1),
    stat_paired_points(),
    stat_paired_difference_points(),
    stat_two_group_ci(paired = TRUE)

  )
}


