#' This is the documentation for stat_catseye. remember to account for missing data
#' @inheritParams ggplot2::stat_identity
#' @param alpha Alpha transparency. Default .50 value.
#' @export
stat_paired_points <- function(mapping = NULL, data = NULL, geom = "point",
                           position = "identity", show.legend = NA,
                           inherit.aes = TRUE, alpha =.50, ...) {

  list(
    ggplot2::layer(
      stat = InsetPoints, data = data, mapping = mapping, geom = "point",
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(alpha = alpha, ...)
    ),
    ggplot2::layer(
      stat = InsetLines, data = data, mapping = mapping, geom = "line",
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(alpha = alpha, size = .3, ...)
    )
  )
}


#' This is the documentation for stat_catseye. remember to account for missing data
#' @inheritParams ggplot2::stat_identity
#' @param alpha Alpha transparency. Default .50 value.
#' @export
stat_paired_difference_points <- function(mapping = NULL, data = NULL, geom = "point",
                               position = "identity", show.legend = NA,
                               inherit.aes = TRUE, alpha = .50, ...) {

  list(
    ggplot2::layer(
      stat = DiffScores, data = data, mapping = mapping, geom = "point",
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(alpha = alpha, shape = 17, ...)
    )
  )
}


#' This is the documentation for stat_catseye. remember to account for missing data
#' @inheritParams ggplot2::stat_identity
#' @param alpha Alpha transparency. Default 1.0 value.
#' @param offset Offset for points
#' @export
stat_offset_points <- function(mapping = NULL, data = NULL, geom = "point",
                                          position = "identity", show.legend = NA,
                                          inherit.aes = TRUE, alpha = 1, offset = -.30, ...) {

  list(
    ggplot2::layer(
      stat = OffsetPoints, data = data, mapping = mapping, geom = "point",
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(alpha = alpha, offset = offset, ...)
    )
  )
}


#' This is the documentation for stat_catseye. remember to account for missing data
#' @inheritParams ggplot2::stat_identity
#' @param paired Indicate if paired data or not. TRUE/FALSE. Default is FALSE
#' @param var.equal Assume variance equal for confidence interval construction. TRUE/FALSE. Default is TRUE
#' @param level Confidence level for confidence interval. 0 to 1. Default .95
#' @param text_size Font size for right hand axis
#' @export
stat_two_group_ci <- function(mapping = NULL, data = NULL, geom = "point",
                                          position = "identity", show.legend = NA,
                                          inherit.aes = TRUE, paired = FALSE, var.equal = TRUE,
                                          text_size = 3, level = .95, ...) {

  list(
    ggplot2::layer(
      stat = MeanDiffLineGroup1, data = data, mapping = mapping, geom = "line",
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(linetype = 2, ...)
    ),
    ggplot2::layer(
      stat = MeanDiffLineGroup2, data = data, mapping = mapping, geom = "line",
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(linetype = 2, ...)
    ),
    ggplot2::layer(
      stat = MeanDiffCI, data = data, mapping = mapping, geom = "errorbar",
      position = position, show.legend = FALSE, inherit.aes = inherit.aes,
      params = list(width = .2, paired = paired, var.equal = var.equal, level = level, ...)
    ),
    ggplot2::layer(
      stat = MeanDiffCIPoint, data = data, mapping = mapping, geom = "point",
      position = position, show.legend = FALSE, inherit.aes = inherit.aes,
      params = list(paired = paired, var.equal = var.equal,
                    level = level, shape = 17, size = 4, ...)
    ),
    ggplot2::layer(
      stat = DiffScale, data = data, mapping = mapping, geom = "segment",
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(var.equal = var.equal, paired = paired, level = level, ...)
    ),
    ggplot2::layer(
      stat = DiffScaleLabels, data = data, mapping = mapping, geom = "text",
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(var.equal = var.equal, paired = paired, level = level, size = text_size, ...)
    ),
    ggplot2::layer(
      stat = ScaleTitle, data = data, mapping = mapping, geom = "text",
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(var.equal = var.equal, paired = paired, level = level, angle = 90,...)
    ),
    ggplot2::layer(
      stat = ExtendXAxis, data = data, mapping = mapping, geom = "point",
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(alpha = 0, ...)
    )
  )
}

