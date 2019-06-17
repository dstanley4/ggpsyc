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
stat_paired_points <- function(mapping = NULL, data = NULL, geom = "point",
                           position = "identity", na.rm = FALSE, show.legend = NA,
                           inherit.aes = TRUE, group1 = NA, group2 = NA, alternative = "two.sided", text_size = 3, ...) {

  list(
    ggplot2::layer(
      stat = InsetPoints, data = data, mapping = mapping, geom = "point",
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, alpha = .50, ...)
    ),
    ggplot2::layer(
      stat = InsetLines, data = data, mapping = mapping, geom = "line",
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, alpha = .50, size = .3, ...)
    )
  )
}


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
stat_paired_difference_points <- function(mapping = NULL, data = NULL, geom = "point",
                               position = "identity", na.rm = FALSE, show.legend = NA,
                               inherit.aes = TRUE, group1 = NA, group2 = NA, alternative = "two.sided", text_size = 3, ...) {

  list(
    ggplot2::layer(
      stat = DiffScores, data = data, mapping = mapping, geom = "point",
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, alpha = .50, shape = 17,...)
    )
  )
}


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
stat_two_group_ci <- function(mapping = NULL, data = NULL, geom = "point",
                                          position = "identity", na.rm = FALSE, show.legend = NA,
                                          inherit.aes = TRUE, paired = FALSE, var.equal = TRUE,
                                          text_size = 3, level = .95, ...) {

  list(
    ggplot2::layer(
      stat = MeanDiffLineGroup1, data = data, mapping = mapping, geom = "line",
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, linetype = 2, ...)
    ),
    ggplot2::layer(
      stat = MeanDiffLineGroup2, data = data, mapping = mapping, geom = "line",
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, linetype = 2, ...)
    ),
    ggplot2::layer(
      stat = MeanDiffCI, data = data, mapping = mapping, geom = "errorbar",
      position = position, show.legend = FALSE, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, width = .2, paired = paired, var.equal = var.equal, level = level, ...)
    ),
    ggplot2::layer(
      stat = MeanDiffCIPoint, data = data, mapping = mapping, geom = "point",
      position = position, show.legend = FALSE, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, paired = paired, var.equal = var.equal,
                    level = level, shape = 17, size = 4, ...)
    ),
    ggplot2::layer(
      stat = DiffScale, data = data, mapping = mapping, geom = "segment",
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, var.equal = var.equal, paired = paired, level = level, ...)
    ),
    ggplot2::layer(
      stat = DiffScaleLabels, data = data, mapping = mapping, geom = "text",
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, var.equal = var.equal, paired = paired, level = level, size = text_size, ...)
    ),
    ggplot2::layer(
      stat = ExtendXAxis, data = data, mapping = mapping, geom = "point",
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, alpha = 0, ...)
    )
  )
}

