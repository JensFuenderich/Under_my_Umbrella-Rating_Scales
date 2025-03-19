### custom function for plots in the manuscript 
### check "Analyses_Manuscrip.qmd" for examples 

### functionality

## Umbrella only 
## you only need to define scale_min and scale_max (and step_size, if the step size is not 1)
## specify parameter = "Var" to return the plot for variances rather than standard deviations on the y-axis

## Umbrella with data (sample variances or sample standard deviations, n in the denominator) 
## you need to define all input parameters other than C_N and T_N
## CM, TM, C_SD and T_SD need to be of the same length
## specify parameter = "Var" to return the plot for variances rather than standard deviations on the y-axis

## Umbrella with data (estimates for population variances or population standard deviations, n-1 in the denominator) 
## you need to define all input parameters
## CM, TM, C_SD, T_SD, C_N and T_N need to be of the same length 
## specify parameter = "Var" to return the plot for variances rather than standard deviations on the y-axis

### required packages 
## c("ggplot2")

umbrella_fun <- function(scale_min, scale_max, step_size = 1, 
                         CM = NA, TM = NA, 
                         C_SD = NA, T_SD = NA, 
                         C_N = NA, T_N = NA,
                         parameter = "SD", 
                         point_alpha = 1, 
                         line_alpha = 0.2, 
                         x_lab = NA, 
                         y_lab = NA){
  
  ### prepare data input 
  
  ## change (n-1) to (n) in calculation of SD 
  if (sum(c(is.na(C_N), is.na(T_N))) == 0) {
    
    C_SD <- sqrt(C_SD^2 * (C_N - 1) / C_N )
    T_SD <- sqrt(T_SD^2 * (T_N - 1) / T_N )
    
  }
  
  ## create df for the plot 
  input_data <- data.frame(
    ID = c(seq(from = 1, to = length(CM), by = 1), seq(from = 1, to = length(TM), by = 1)),
    means = c(CM, TM) - scale_min, 
    SD = c(C_SD, T_SD), 
    group_color = rep(c(0.2, 0.8), times = c(length(C_SD), length(T_SD)))
  )
  
  ### create data for the umbrella 
  
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
      upper_x = rating_scale_means,
      upper_y = SD_upper_limit,
      lower_x = rating_scale_means,
      lower_y = SD_lower_limit
    )
    
    if ( sum(c(is.na(CM), is.na(TM), is.na(C_SD), is.na(T_SD))) == 0 ) {
      
      ## plot the umbrella
      ggplot(data = umbrella_data) +
        geom_line(aes(x = upper_x,
                      y = upper_y)) +
        geom_line(aes(x = lower_x,
                      y = lower_y)) +
        geom_point(data = input_data, 
                   aes(x = means, y = SD, color = group_color), alpha = point_alpha) +
        geom_line(data = input_data, 
                  aes(x = means, 
                      y = SD, 
                      group = ID), 
                  alpha = line_alpha) + 
        theme_minimal() +
        scale_x_continuous(name = ifelse(is.na(x_lab), "mean", x_lab), 
                           breaks = seq(from = 0, to = scale_max - scale_min, by = step_size), 
                           labels = seq(from = scale_min, to = scale_max, by = step_size)) + 
        ylab(ifelse(is.na(y_lab), "standard deviation", y_lab)) + 
        theme(legend.position = "none")
      
    } else {
      
      ## plot the umbrella
      ggplot(data = umbrella_data) +
        geom_line(aes(x = upper_x,
                      y = upper_y)) +
        geom_line(aes(x = lower_x,
                      y = lower_y)) +
        theme_minimal() +
        scale_x_continuous(name = "mean", breaks = seq(from = 0, to = scale_max - scale_min, by = step_size), labels = seq(from = scale_min, to = scale_max, by = step_size)) + 
        ylab(ifelse(is.na(y_lab), "standard deviation", y_lab))
      
    }
    
  } else if (parameter == "Var") {
    
    ## create plot data
    umbrella_data <- data.frame(
      upper_x = rating_scale_means,
      upper_y = SD_upper_limit^2,
      lower_x = rating_scale_means,
      lower_y = SD_lower_limit^2
    )
    
    if ( sum(c(is.na(CM), is.na(TM), is.na(C_SD), is.na(T_SD))) == 0 ) {
      
      ## plot the umbrella
      ggplot(data = umbrella_data) +
        geom_line(aes(x = upper_x,
                      y = upper_y)) +
        geom_line(aes(x = lower_x,
                      y = lower_y)) +
        geom_point(data = input_data, 
                   aes(x = means, y = SD^2, color = group_color), alpha = point_alpha) +
        geom_line(data = input_data, 
                  aes(x = means, 
                      y = SD^2, 
                      group = ID), 
                  alpha = line_alpha) + 
        theme_minimal() +
        scale_x_continuous(name = ifelse(is.na(x_lab), "mean", x_lab), 
                           breaks = seq(from = 0, to = scale_max - scale_min, by = step_size),
                           labels = seq(from = scale_min, to = scale_max, by = step_size)) + 
        ylab(ifelse(is.na(y_lab), "variance", y_lab)) + 
        theme(legend.position = "none")
      
    } else {
      
      ## plot the umbrella
      ggplot(data = umbrella_data) +
        geom_line(aes(x = upper_x,
                      y = upper_y)) +
        geom_line(aes(x = lower_x,
                      y = lower_y)) +
        theme_minimal() +
        scale_x_continuous(name = "mean", 
                           breaks = seq(from = 0, to = scale_max - scale_min, by = step_size),
                           labels = seq(from = scale_min, to = scale_max, by = step_size)) + 
        ylab(ifelse(is.na(y_lab), "variance", y_lab))
      
    }
    
  } else {
    
    print("your input does not fit the function")
    
  }
}
