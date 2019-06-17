line_to_edge_data <- function(data, scales, desired_group) {

  group1 <- 1
  group2 <- 2
  x_end <- max(data$x) + 1.3

  id_desired_group <- data$x == desired_group
  data_subset <- data[id_desired_group, ]

  dodge_adjust <- 0



  y <- mean(data_subset$y)
  x_start <- data_subset$x[1] + dodge_adjust
  group <- data_subset$group[1]
  PANEL <- data_subset$PANEL[1]


  data <- data.frame(x=c(x_start, x_end), y=c(y,y), PANEL = PANEL, group = group)

  data
}

get_group_data <- function(data, scales, desired_group) {
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


  g1data <- data$y[id1]
  g2data <- data$y[id2]

  g1mean <- mean(g1data)
  g2mean <- mean(g2data)
  min_mean <- min(g1mean, g2mean)
  max_mean <- max(g1mean, g2mean)

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



inset_points  <- function(data) {
  is1 <- data$x == 1
  is2 <- data$x == 2

  data$x[is1] <- 1.3
  data$x[is2] <- 1.7

  data
}


create_diff_data <- function(data, scales, level) {

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

  return(new_data)
}


get_summary_df <-function(data) {
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




