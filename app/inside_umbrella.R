#### CREATE UMBRELLA CHECK FOR SD
#####
inside_umbrella_fun <- function(data_row, data_set, scale_min, scale_max, parameter = "SD"){


  ## select x and y of the row
  x <- data_set[data_row,]$x
  y <- data_set[data_row,]$y

  if(x >= scale_min & x <= scale_max){

    ## calculate scale max if the scale starts at 0
    scale_max_0 <- scale_max - scale_min

    ## create means of the scale from 0 to scale_max_0
    rating_scale_means <- seq(from = 0, to = scale_max_0, by = scale_max_0/(200*scale_max_0))

    ## calculate Bernoulli means for upper limit
    ## we assume all values are
    ## x = {0;scale_max_0}
    p_upper <- rating_scale_means/scale_max_0

    ## calculate Var according to Bernoulli distribution, rescale and take the sqrt
    SD_upper_limit <- sqrt(scale_max_0^2 * p_upper * (1-p_upper))

    ## calculate Bernoulli means for lower limit
    ## we assume all values are
    ## x = { ceiling(rating_scale_means); floor(rating_scale_means) }
    p_lower <- rating_scale_means - floor(rating_scale_means)

    ## calculate Var according to Bernoulli distribution, rescale the variance and take the sqrt
    SD_lower_limit <- sqrt((p_lower * (1-p_lower)))

    if (parameter == "SD") {

      ## create plot data
      umbrella_data <- data.frame(
        means_x = rating_scale_means,
        upper_y = SD_upper_limit,
        lower_y = SD_lower_limit
      )
    }

    ### check if SD is outside umbrella

    ## Insert the target into the vector and sort
    combined <- sort(c(umbrella_data$means_x, x))

    ## remove duplicates (this ensures that the following lines work and the logic stays intact)
    combined <- unique(combined)

    ## Find the index of the target
    mean_index <- which(combined == x)

    ## Get the two neighbors (if they exist)
    lower_neighbor <- if (mean_index > 1) combined[mean_index - 1] else NA
    upper_neighbor <- if (mean_index < length(combined)) combined[mean_index + 1] else NA

    if(x == scale_min) {
      lower_neighbor <- x
    } else if(x == scale_max){
      upper_neighbor <- x
    }

    ## is the data point below the umbrella? (TRUE/FALSE)
    below_umbrella <-
      (subset(umbrella_data, means_x == upper_neighbor)$lower_y < y) &
      (subset(umbrella_data, means_x == lower_neighbor)$lower_y < y)
    ## is the data point above the umbrella? (TRUE/FALSE)
    above_umbrella <-
      (subset(umbrella_data, means_x == upper_neighbor)$upper_y > y) &
      (subset(umbrella_data, means_x == lower_neighbor)$upper_y > y)

    ## is the data point inside the umbrella? (TRUE/FALSE)
    below_umbrella & above_umbrella

  } else {

    ## the mean is outside of the scale, therefore
    FALSE

  }

}

#####
