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
#' @param alternative asdsf
#' @param text_size asdsf
#' @param ... adfasdf
#' @export
stat_mean_diff <- function(mapping = NULL, data = NULL, geom = "line",
                          position = "identity", na.rm = FALSE, show.legend = NA,
                          inherit.aes = TRUE, group1 = NA, group2 = NA, alternative = "two.sided", text_size = 3, ...) {

  list(
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
      stat = MeanDiffPoints, data = data, mapping = mapping, geom = "point",
      position = position, show.legend = FALSE, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, size = 4, group1 = group1, group2 = group2, ...)
    ),
    ggplot2::layer(
      stat = MeanDiffCI, data = data, mapping = mapping, geom = "errorbar",
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, group1 = group1, group2 = group2, alternative = alternative, ...)
    ),
    # ggplot2::layer(
    #   stat = DiffLabel, data = data, mapping = mapping, geom = "text",
    #   position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    #   params = list(na.rm = na.rm, group1 = group1, group2 = group2, hjust = 1, ...)
    # ),
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

