geom_two_group_between <- function() {
  list(
    stat_two_group_ci(paired = FALSE),
    stat_summary(fun.data = mean_cl_normal,
                 geom = "errorbar",
                 width = .2),
    stat_summary(fun.data = mean_cl_normal,
                 geom = "point",
                 size = 3),
    geom_point(mapping = aes(as.numeric(alcohol) - 0.25),
               position = position_jitter(width = .1))
  )
}

geom_two_group_within <- function() {
  list(
    stat_summary(fun.data = mean_cl_normal,
                 geom = "errorbar",
                 width = .2,
                 group = 1),
    stat_summary(fun.data = mean_cl_normal,
                 geom = "point",
                 size = 3,
                 group = 1),
    stat_summary(fun.data = mean_cl_normal,
                 geom = "line",
                 size = 1,
                 group = 1),
    stat_paired_points(),
    stat_paired_difference_points(),
    stat_two_group_ci(paired = TRUE)

  )
}


