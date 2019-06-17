MeanDiffLineGroup1<- ggplot2::ggproto("MeanDiffLineGroup1", ggplot2::Stat,
                                      required_aes = c("x", "y"),

                                      compute_panel = function(data, scales, level, width) {

                                        data <- line_to_edge_data(data, scales, desired_group = 1)
                                        data

                                      }
)


MeanDiffLineGroup2<- ggplot2::ggproto("MeanDiffLineGroup2", ggplot2::Stat,
                                      required_aes = c("x", "y"),

                                      compute_panel = function(data, scales, width) {
                                        data <- line_to_edge_data(data, scales, desired_group = 2)
                                        data
                                      }
)

DiffScores<- ggplot2::ggproto("DiffScores", ggplot2::Stat,
                              required_aes = c("x", "y"),

                              compute_panel = function(data, scales, level) {

                                data  <- create_diff_data(data, scales, level)
                                #print(data)
                                data
                              }
)



InsetLines<- ggplot2::ggproto("InsetLines", ggplot2::Stat,
                              required_aes = c("x", "y"),

                              compute_panel = function(data, scales, level) {
                                data  <- inset_points(data)
                                #print(data)
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
                                      #print(data)
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


MeanDiffCI<- ggplot2::ggproto("MeanDiffCI", ggplot2::Stat,
                              required_aes = c("x", "y"),

                              compute_panel = function(data, scales, level, paired, var.equal, width, ymin, ymax) {

                                ci_data <- ci_diff(data = data,
                                                   scales = scales,
                                                   level = level,
                                                   paired = paired,
                                                   var.equal = var.equal)
                                ci_data
                              }
)


MeanDiffCIPoint<- ggplot2::ggproto("MeanDiffCIPoint", ggplot2::Stat,
                                   required_aes = c("x", "y"),

                                   compute_panel = function(data, scales, level, paired, var.equal, width, ymin, ymax) {

                                     ci_data <- ci_diff(data = data,
                                                        scales = scales,
                                                        level = level,
                                                        paired = paired,
                                                        var.equal = var.equal)
                                     ci_data$y[1] <- ci_data$maxmean[1]
                                     ci_data
                                   }
)


DiffLabel<- ggplot2::ggproto("DiffLabel", ggplot2::Stat,
                             required_aes = c("x", "y"),

                             compute_panel = function(data, scales, level, paired, var.equal, width) {

                               ci_data <- ci_diff(data = data,
                                                  scales = scales,
                                                  level = level,
                                                  paired = paired,
                                                  var.equal = var.equal)

                               yrangeinfo <- scales$y$range$range
                               ysize <- yrangeinfo[2] - yrangeinfo[1]
                               ybump <- .1 * ysize

                               data <- data.frame(x = ci_data$x[1], y = ci_data$ymax+ybump, label = "Difference", PANEL = ci_data$PANEL[1], group = ci_data$group[1])
                               data
                             }
)

DiffScale<- ggplot2::ggproto("DiffScale", ggplot2::Stat,
                             required_aes = c("x", "y"),

                             compute_panel = function(data, scales, level, width, paired, var.equal) {

                               scale_details <- get_scale_details(data = data,
                                                                  scales = scale,
                                                                  level = level,
                                                                  paired = paired,
                                                                  var.equal = var.equal)

                               scale_vertical <- scale_details$scale_vertical
                               scale_ticks <- scale_details$scale_ticks



                               data <- rbind(scale_vertical, scale_ticks)

                               data
                             }
)

DiffScaleLabels<- ggplot2::ggproto("DiffScale", ggplot2::Stat,
                                   required_aes = c("x", "y"),

                                   compute_panel = function(data, scales, level, paired, var.equal, width) {

                                     scale_details <- get_scale_details(data = data,
                                                                        scales = scales,
                                                                        level = level,
                                                                        var.equal = var.equal,
                                                                        paired = paired)

                                     scale_ticks <- scale_details$scale_ticks
                                     scale_labels <- scale_details$scale_labels

                                     x <- scale_ticks$xend + .15
                                     y <- scale_ticks$y
                                     label <- scale_labels
                                     PANEL <- scale_ticks$PANEL
                                     group <- scale_ticks$group

                                     data <- data.frame(x, y, label, PANEL, group)

                                     data
                                   }
)

ExtendXAxis<- ggplot2::ggproto("ExtendXAxis", ggplot2::Stat,
                               required_aes = c("x", "y"),

                               compute_panel = function(data, scales, level, width) {

                                 xnew <- max(data$x) + 2
                                 ynew <- max(data$y)

                                 data <- data[1, ]
                                 data$x[1] <- xnew
                                 data$y[1] <- ynew

                                 data
                               }
)


OffsetPoints<- ggplot2::ggproto("OffsetPoints", ggplot2::Stat,
                               required_aes = c("x", "y"),

                               compute_panel = function(data, scales, level, width, offset) {
                                 data$x <- data$x + offset
                                 data
                               }
)

