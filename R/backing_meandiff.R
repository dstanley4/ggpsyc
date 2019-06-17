


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

MeanDiffPoints<- ggplot2::ggproto("MeanDiffPoints", ggplot2::Stat,
                                  required_aes = c("x", "y"),

                                  compute_panel = function(data, scales, level, width) {
                                    maxgroup <- max(data$group)
                                    data1 <- edge_point_data(data, scales, desired_group = 1)
                                    data2 <- edge_point_data(data, scales, desired_group = 2)

                                    if (data1$y > data2$y) {
                                      data <- data1
                                    } else {
                                      data <- data2
                                    }
                                    #data <- rbind(data1, data2)
                                    #data$x <- data$x - .05
                                    data$group <- maxgroup + 1
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
                                                  width = width,
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




line_to_edge_data <- function(data, scales, desired_group) {

  group1 <- 1
  group2 <- 2
  x_end <- max(data$x) + 1.3

  id_desired_group <- data$x == desired_group
  data_subset <- data[id_desired_group, ]
  #
  #
  # data_subset <- get_group_data(data, scales, desired_group )

  dodge_adjust <- 0

  # if (desired_group == 1) {
  #   if (any("dodge_adjust" %in% names(group1))) {
  #     dodge_adjust <- group1$dodge_adjust
  #   }
  # } else {
  #   if (any("dodge_adjust" %in% names(group2))) {
  #     dodge_adjust <- group2$dodge_adjust
  #   }
  # }


  # print(data_subset)
  y <- mean(data_subset$y)
  #print(y)
  x_start <- data_subset$x[1] + dodge_adjust
  #print(data_subset$x[1])
  #print(x_start)
  group <- data_subset$group[1]
  PANEL <- data_subset$PANEL[1]


  data <- data.frame(x=c(x_start, x_end), y=c(y,y), PANEL = PANEL, group = group)

  data
}

get_group_data <- function(data, scales, desired_group) {
  #print("get_group_data")
  #print(data)
  # group1 <- make_group_info_a_list(group1)
  # group2 <- make_group_info_a_list(group2)
  group1 <- 1
  group2 <- 2

  fac_levels <- scales$x$range$range
  data$levx <- factor(data$x)
  levels(data$levx) <- fac_levels


  if (desired_group == 1) {
    x_level <- group1$x
    group1_attributes <- names(group1)
    if (any("shape" %in% group1_attributes)) {
      desired_level <- group1$shape
      data_subset <- dplyr::filter(data, .data$levx == !! x_level, .data$shape == !! desired_level)
    } else if(any("colour" %in% group1_attributes)) {
      desired_level <- group1$colour
      data_subset <- dplyr::filter(data, .data$levx == !! x_level, .data$colour == !! desired_level)
    } else if(any("fill" %in% group1_attributes)) {
      desired_level <- group1$fill
      data_subset <- dplyr::filter(data, .data$levx == !! x_level, .data$fill == !! desired_level)
    } else {
      data_subset <- dplyr::filter(data, .data$levx == !! x_level)
    }
  } else {
    x_level <- group2$x
    group2_attributes <- names(group2)
    if (any("shape" %in% group2_attributes)) {
      desired_level <- group2$shape
      #print("in iff")
      #print(data)
      data_subset <- dplyr::filter(data, .data$levx == !! x_level, .data$shape == !! desired_level)
    } else if(any("colour" %in% group2_attributes)) {
      desired_level <- group2$colour
      data_subset <- dplyr::filter(data, .data$levx == !! x_level, .data$colour == !! desired_level)
    } else if(any("fill" %in% group2_attributes)) {
      desired_level <- group2$fill
      data_subset <- dplyr::filter(data, .data$levx == !! x_level, .data$fill == !! desired_level)
    } else {
      data_subset <- dplyr::filter(data, .data$levx == !! x_level)
    }


  }

  data_subset
}


edge_point_data <- function(data, scales, desired_group) {
  # group1 <- make_group_info_a_list(group1)
  # group2 <- make_group_info_a_list(group2)
  group1 <- 1
  group2 <- 2
  x <- max(data$x) + 1

  data_subset <- get_group_data(data, scales, desired_group )
  y <- mean(data_subset$y)

  group <- data_subset$group[1]
  PANEL <- data_subset$PANEL[1]

  data <- data.frame(x = x, y = y, shape = "Difference", PANEL = PANEL, group = group)
  data
}

ci_diff <- function(data, scales, level, paired, var.equal) {
  group1 <- 1
  group2 <- 2

  id1 <- data$x == 1
  id2 <- data$x == 2

  print(data)

  g1data <- data$y[id1]
  g2data <- data$y[id2]

  g1mean <- mean(g1data)
  g2mean <- mean(g2data)
  min_mean <- min(g1mean, g2mean)
  max_mean <- max(g1mean, g2mean)

  #alternative = "two.sided"
  alternative = "two.sided"
  if (paired == FALSE) {
    tresult <- stats::t.test(x = g1data,
                             y = g2data,
                             alternative = alternative,
                             paired = paired,
                             var.equal = var.equal,
                             conf.level = level)

  } else {
    tresult <- stats::t.test(x = g1data,
                             y = g2data,
                             alternative = alternative,
                             paired = paired,
                             conf.level = level)
  }


  x_end <- max(data$x) + 1
  ymin <- tresult$conf.int[1] + min_mean
  ymax <- tresult$conf.int[2] + min_mean
  cimin <- tresult$conf.int[1]
  cimax <- tresult$conf.int[2]

  group <- max(data$group)+1
  PANEL <- data$PANEL[1]




  dataout <- data.frame(x = x_end,
                     y = min_mean,
                     ymin = ymin,
                     ymax = ymax,
                     PANEL = PANEL,
                     group = group,
                     cimin = cimin,
                     cimax = cimax,
                     maxmean = max_mean)

  print(dataout)
  dataout
}


get_scale_details <- function(data, scales, level, paired, var.equal) {

  PANEL <- data$PANEL[1]
  group <- data$group[1]

  ci_data <- ci_diff(data = data,
                     scales = scales,
                     level = level,
                     paired = paired,
                     var.equal = var.equal)

  scale_x <- max(data$x) + 1.3
  scale_y <- ci_data$y[1] #min mean value

  LL <- ci_data$cimin[1]
  UL <- ci_data$cimax[1]

  min_mean <- ci_data$min_mean[1]

  ci_length <- UL - LL
  ci_length_extended <- ci_length * .5


  lowest_value <- min((0 - ci_length_extended*1.2), (LL - ci_length_extended*1.2))
  highest_value <- UL + ci_length_extended

  ci_ticks <- grDevices::axisTicks(usr=c(lowest_value, highest_value),log=FALSE, nint = 8)
  scale_ticks <- ci_ticks + scale_y
  scale_ymin <- scale_ticks[1]
  scale_ymax <- scale_ticks[length(scale_ticks)]

  scale_vertical <- data.frame(x = scale_x, y = scale_ymin, xend = scale_x, yend =scale_ymax,PANEL = PANEL, group = group)
  scale_ticks <- data.frame(x = scale_x, y = scale_ticks, xend = scale_x +.03, yend = scale_ticks, PANEL = PANEL, group = group)
  scale_labels <- as.character(ci_ticks)

  output <- list(scale_vertical = scale_vertical,
                 scale_ticks = scale_ticks,
                 scale_labels = scale_labels)
  output
}


make_group_info_a_list <- function(group) {
  cur_class <- class(group)

  if (cur_class == "character") {
    group <- list(x = group)
  }
  return(group)
}



