### SERVER
#####

server <- function(input, output, session){


  ## Create shiny modal dialog window for terms of use

  terms_of_service_modal <- shiny::modalDialog(
    title = "Terms of Service",
    shiny::p("This service is intended for academic researchers and students and may not be used for commercial purposes. No personally identifiable data may be uploaded. The user ensures that the upload of the data does not infringe the rights of third parties."),
    shiny::p("LMU does not guarantee the accuracy of the analyses and visualizations. Likewise, LMU does not guarantee that the provided server applications meet the specific requirements of the user or that the system runs without errors or interruption. LMU cannot guarantee the integrity (in terms of destruction, manipulation) and confidentiality of the data processed by it. LMU may change the server applications at any time or discontinue the service."),
    shiny::p("LMU is not liable for any damages of any kind arising from the use of the server applications; except for intentional misconduct and gross negligence of LMU employees or the persons they use to fulfill their duties."),
    shiny::checkboxGroupInput("check_terms", "",
                              choiceNames =
                                list("I have read and accept the terms of service",
                                     "I will only upload fully anonymized data sets, not personally identifiable data.",
                                     "I will not upload any data that infringes on the rights of third parties."),
                              choiceValues =
                                list("calendar", "bed", "cog")),
    easyClose = F,
    footer = tagList(
      shiny::p("Make sure to agree to the terms of service and you may explore the Shiny app."),
      shiny::actionButton("start_app", "Start exploring the app!")
    )
  )

  # Show the model on start up
  shiny::showModal(terms_of_service_modal)

  shiny::observeEvent(input$start_app, {

    if ("calendar" %in% input$check_terms &
        "bed" %in% input$check_terms &
        "cog" %in% input$check_terms){
      shiny::removeModal()
    }

  })

  #### DISCRETE UMBRELLA

  ## create the umbrella plot
  output_plot_discrete <- shiny::eventReactive(input$update_plot_discrete,{

    scale_min <- base::as.numeric(input$LL_scale_discrete)
    scale_max <- base::as.numeric(input$UL_scale_discrete)
    n <- base::as.numeric(input$n_discrete)

    ## generate are possible response patterns (IPD) for:
    ## n = 12
    ## scale: 0 - 6
    IPD_combinations <- gtools::combinations(
      n = length(scale_min:scale_max), # number of integers of the scale
      r = n, # n
      v = scale_min:scale_max, # the units of the scale
      repeats.allowed = TRUE)

    ## create list with mean and sd results per possibel sample result (row)
    data_list <- base::lapply(
      1:base::nrow(IPD_combinations),
      function(row){
        subset_possible_combinations <- IPD_combinations[row,]
        base::data.frame(
          mean = mean(subset_possible_combinations),
          sd = base::sqrt(
            mean((subset_possible_combinations - mean(subset_possible_combinations))^2)
          )
        )
      })

    ## store as df
    df <- base::do.call(base::rbind, data_list)

    ## create plot, conditional on SD/Var choice
    if(input$variability_est_discrete == "SD"){

      ## create plot
      ggplot2::ggplot(data = df,
                      ggplot2::aes(x = mean, y = sd)) +
        ggplot2::geom_point(color = "#CCBBAA") +
        ggplot2::scale_x_continuous(breaks = base::seq(from = scale_min, to = scale_max, by = 1)) +
        ggplot2::ylab(label = "standard deviation") +
        ggplot2::theme_minimal()

    } else if(input$variability_est_discrete == "VAR"){

      ## create plot
      ggplot2::ggplot(data = df,
                      ggplot2::aes(x = mean, y = sd^2)) +
        ggplot2::geom_point(color = "#CCBBAA") +
        ggplot2::scale_x_continuous(breaks = base::seq(from = scale_min, to = scale_max, by = 1)) +
        ggplot2::ylab(label = "variance") +
        ggplot2::theme_minimal()

    }

  })

  ## render the umbrella plot
  output$umbrella_plot_discrete <- shiny::renderPlot({

    output_plot_discrete()

  })

  #### ERROR CHECKING

  ## import csv file & add info columns
  csv_import <- shiny::eventReactive(input$import_csv, {

    ## store file specifications
    input_specifications <- base::as.data.frame(input$select_csv)

    ## import csv
    data <- readr::read_csv(file = input_specifications$datapath)

    ## adjust SD according to input
    if(input$SD_type == "estimated"){
      n <- base::as.numeric(input$sample_size)
      data$y <- data$y * base::sqrt(n/(n-1))
    }

    ## check if data points are in the umbrella & add column
    data$inside_umbrella <- base::unlist(base::lapply(1:base::nrow(data), inside_umbrella_fun, data_set = data, scale_min = 0, scale_max = 6))

    data

  })

  sim_data <- shiny::reactive({

    scale_min <- base::as.numeric(input$LL_scale_error)
    scale_max <- base::as.numeric(input$UL_scale_error)
    scale_max_0 <- scale_max - scale_min

    data <- base::data.frame(
      ## ID
      ID = 1:10,
      ## mean
      x = base::sample(stats::runif(n = 1e3,
                       min = scale_min,
                       max = scale_max),
                 size = 10,
                 replace = TRUE),
      ## sd
      y = base::sample(stats::runif(n = 1e3,
                       min = scale_min,
                       max = base::sqrt(0.5^2 * scale_max_0^2)),
                 size = 10,
                 replace = TRUE)
    )

    ## check if data points are in the umbrella & add column
    data$inside_umbrella <- base::unlist(base::lapply(1:10, inside_umbrella_fun, data_set = data, scale_min = scale_min, scale_max = scale_max))

    data

  })

  ## create/update plot according to UI input
  output_plot <- shiny::eventReactive(input$update_plot_error, {


    if(input$data_input == "no_data"){

      umbrella_fun(scale_min = base::as.numeric(input$LL_scale_error),
                   scale_max = base::as.numeric(input$UL_scale_error),
                   step_size = base::as.numeric(input$step_size_scale_error))

    } else if(input$data_input == "sim_data"){

      ## store reactive object as data frame
      sim_data <- base::as.data.frame(sim_data())

      umbrella_fun(scale_min = base::as.numeric(input$LL_scale_error),
                   scale_max = base::as.numeric(input$UL_scale_error),
                   step_size = base::as.numeric(input$step_size_scale_error),
                   CM = base::as.numeric(sim_data$x),
                   TM = base::as.numeric(sim_data$x),
                   C_SD = base::as.numeric(sim_data$y),
                   T_SD = base::as.numeric(sim_data$y))

    } else if(input$data_input == "upload"){

      ## store reactive object as data frame
      csv_import <- base::as.data.frame(csv_import())

      umbrella_fun(scale_min = base::as.numeric(input$LL_scale_error),
                   scale_max = base::as.numeric(input$UL_scale_error),
                   step_size = base::as.numeric(input$step_size_scale_error),
                   CM = base::as.numeric(csv_import$x),
                   TM = base::as.numeric(csv_import$x),
                   C_SD = base::as.numeric(csv_import$y),
                   T_SD = base::as.numeric(csv_import$y))

    } else if(input$data_input == "manual"){

      umbrella_fun(scale_min = base::as.numeric(input$LL_scale_error),
                   scale_max = base::as.numeric(input$UL_scale_error),
                   step_size = base::as.numeric(input$step_size_scale_error),
                   CM = base::as.numeric(input$man_x),
                   TM = base::as.numeric(input$man_x),
                   C_SD = base::as.numeric(input$man_y),
                   T_SD = base::as.numeric(input$man_y))

    }

  })

  ## render umbrella plot
  output$umbrella_plot <- shiny::renderPlot({

    output_plot()

  })

  ## collect clicks
  # selected_points <- shiny::reactiveVal(base::data.frame(x = numeric(0), y = numeric(0)))
  selected_points <- shiny::reactiveVal(base::data.frame())


  ## create data table from clicks
  shiny::observeEvent(input$plot_click, {

    ## check if necessary info has been provided
    shiny::req(input$data_input == "sim_data", input$plot_click)

    ## turn reactive into data frame
    df <- base::as.data.frame(sim_data())

    if (!all(c("x", "y") %in% names(df))) {
      showNotification("CSV must have 'x' and 'y' columns", type = "error")
      return()
    }

    ## save click coordinates
    x_click <- input$plot_click$x
    y_click <- input$plot_click$y

    ## define click tolerance
    x_UL <- base::as.numeric(input$UL_scale_error) - base::as.numeric(input$LL_scale_error)
    tol_x <- x_UL / (x_UL * 20)
    y_UL <- base::sqrt(x_UL^2 * ( x_UL / 2)/x_UL * (1-( x_UL / 2)/x_UL))
    tol_y <- y_UL / (y_UL * 20)

    ## store clicked point(s)
    clicked_point <- subset(df,
                            x > (x_click - tol_x) & x < (x_click + tol_x) &
                              y > (y_click - tol_y) & y < (y_click + tol_y))

    if (base::nrow(clicked_point) > 0) {
      current <- selected_points()

      # Check if point is already selected
      match_idx <- base::which(base::abs(current$x - clicked_point$x[1]) < 1e-8 &
                           base::abs(current$y - clicked_point$y[1]) < 1e-8)

      if (length(match_idx) > 0) {
        updated <- current[-match_idx, , drop = FALSE]  # remove point
      } else {
        updated <- base::rbind(current, clicked_point[1, , drop = FALSE])  # add point
      }

      selected_points(updated)
    }


  })


  ## create data table from clicks
  shiny::observeEvent(input$plot_click, {

    ## check if necessary info has been provided
    shiny::req(csv_import(), input$plot_click)

    ## turn reactive into data frame
    df <- base::as.data.frame(csv_import())

    if (!all(c("x", "y") %in% names(df))) {
      showNotification("CSV must have 'x' and 'y' columns", type = "error")
      return()
    }

    ## save click coordinates
    x_click <- input$plot_click$x
    y_click <- input$plot_click$y

    ## define click tolerance
    x_UL <- base::as.numeric(input$UL_scale_error) - base::as.numeric(input$LL_scale_error)
    tol_x <- x_UL / (x_UL * 20)
    y_UL <- base::sqrt(x_UL^2 * ( x_UL / 2)/x_UL * (1-( x_UL / 2)/x_UL))
    tol_y <- y_UL / (y_UL * 20)

    ## store clicked point(s)
    clicked_point <- subset(df,
                            x > (x_click - tol_x) & x < (x_click + tol_x) &
                              y > (y_click - tol_y) & y < (y_click + tol_y))

    if (base::nrow(clicked_point) > 0) {
      current <- selected_points()

      # Check if point is already selected
      match_idx <- base::which(base::abs(current$x - clicked_point$x[1]) < 1e-8 &
                                 base::abs(current$y - clicked_point$y[1]) < 1e-8)

      if (length(match_idx) > 0) {
        updated <- current[-match_idx, , drop = FALSE]  # remove point
      } else {
        updated <- base::rbind(current, clicked_point[1, , drop = FALSE])  # add point
      }

      selected_points(updated)
    }
  })

  ## render data table
  output$data_table <- DT::renderDT({
    selected_points()
  },
  ## create download button to store selected data points
  extensions = 'Buttons',
  options = list(
    paging = TRUE,
    searching = TRUE,
    fixedColumns = TRUE,
    autoWidth = TRUE,
    ordering = TRUE,
    dom = 'tB',
    buttons = c('copy', 'csv')
  ),
  class = "display"
  )

  #### POWER

  ## create/update plot according to UI input
  data_umbrella_power <- shiny::eventReactive(input$update_plot_power, {

    ## use values from input
    scale_max <- base::as.numeric(input$UL_scale_power)
    scale_min <- base::as.numeric(input$LL_scale_power)
    step_size <- base::as.numeric(input$step_size_power)
    MD <- base::as.numeric(input$set_MD_power)

    ## calculate scale max if the scale starts at 0
    scale_max_0 <- scale_max - scale_min

    ## create means of the scale from 0 to scale_max_0
    rating_scale_means <- base::seq(from = 0, to = scale_max_0, by = scale_max_0/(1e3*scale_max_0))

    ## calculate Bernoulli means for upper limit
    ## we assume all values are
    ## x = {0;scale_max_0}
    p_upper <- rating_scale_means/scale_max_0

    ## calculate Var according to Bernoulli distribution, rescale and take the sqrt
    SD_upper_limit <- base::sqrt(scale_max_0^2 * p_upper * (1-p_upper))

    scale_center <- scale_max/2

    ## all possible group means for the MD
    C_M <- rating_scale_means[rating_scale_means <= (scale_max_0 - MD)]
    T_M <- C_M + MD

    pooled_SD_list <- base::lapply(1:length(C_M), function(x){

      CM <- C_M[x]
      TM <- T_M[x]

      ## identify the index of the mean (to later identify the respective SD)
      ## use near for floating point issues
      C_M_ID <- base::which(dplyr::near(rating_scale_means, CM))
      T_M_ID <- base::which(dplyr::near(rating_scale_means, TM))
      # C_M_ID <- base::which(base::abs(rating_scale_means-CM) == min(base::abs(rating_scale_means-CM)))
      # T_M_ID <- base::which(base::abs(rating_scale_means-TM) == min(base::abs(rating_scale_means-TM)))

      ## identify the respective SD
      C_SD <- base::unique(SD_upper_limit[C_M_ID])
      T_SD <- base::unique(SD_upper_limit[T_M_ID])

      base::data.frame(
        C_SD = C_SD,
        T_SD = T_SD,
        pooled_SD = base::sqrt((C_SD^2 + T_SD^2) / 2)
      )


    })

    pooled_SD_df <- base::do.call(base::rbind, pooled_SD_list)

    largest_pooled_SD <- pooled_SD_df[base::which(pooled_SD_df$pooled_SD == max(pooled_SD_df$pooled_SD)),]

    largest_pooled_SD

    ## corresponding CM
    final_CM <- C_M[base::which(pooled_SD_df$pooled_SD == max(pooled_SD_df$pooled_SD))]

    ## corresponding TM
    final_TM <- final_CM + MD

    base::data.frame(
      CM = final_CM + scale_min,
      TM = final_TM + scale_min,
      MD = (final_TM + scale_min) - (final_CM + scale_min),
      C_SD = largest_pooled_SD$C_SD,
      T_SD = largest_pooled_SD$T_SD,
      pooled_SD = largest_pooled_SD$pooled_SD
    )


  })

  output_umbrella_power <- shiny::eventReactive(input$update_plot_power,{

    ## use values from input
    scale_max <- base::as.numeric(input$UL_scale_power)
    scale_min <- base::as.numeric(input$LL_scale_power)
    step_size <- base::as.numeric(input$step_size_power)

    ## use the data of the previous chunk
    df <- base::as.data.frame(data_umbrella_power())

    umbrella_fun(scale_min = scale_min,
                 scale_max = scale_max,
                 step_size = step_size,
                 CM = df$CM,
                 TM = df$TM,
                 C_SD = df$C_SD,
                 T_SD = df$T_SD,
                 line_alpha = 0.75) +
      ggplot2::ggtitle(base::paste("maximum pooled SD:", base::round(df$pooled_SD, digits = 3)))

  })

  ## render umbrella plot
  output$umbrella_plot_power <- shiny::renderPlot({

    output_umbrella_power()

  })

  ## create/update plot according to UI input
  output_power_plot <- shiny::reactive({

    ### save UI inputs as numeric
    scale_min <- base::as.numeric(input$LL_scale_discrete)
    scale_max <- base::as.numeric(input$UL_scale_discrete)
    MD <- base::as.numeric(input$set_MD_power)

    ## use the data of the previous chunk
    df <- base::as.data.frame(data_umbrella_power())

    ### power calculations and plot

    ## set a vector with a range of n (per group)
    n_group_vec <- base::seq(from = 12, to = 360, by = 12)

    ## calculate power per n
    data <- base::lapply(
      n_group_vec,
      function(n_group){

        ## calculate power
        Welch_results <- MKpower::power.welch.t.test(
          n = n_group,
          delta = df$MD,
          sd1 = df$pooled_SD,
          sd2 = df$pooled_SD,
          sig.level = 0.05,
          alternative = "two.sided")

        ## export results as df
        base::data.frame(n = n_group*2,
                         power_Welch = Welch_results$power
        )

      }
    )

    ## store data in a single df
    df <- base::do.call(base::rbind, data)

    ## create plot
    ggplot2::ggplot(data = df) +
      ggplot2::geom_point(ggplot2::aes(x = n, y = power_Welch),
                          color = "#DF536B") +
      ## theme, etc.
      ggplot2::ylab(label = "Power") +
      ggplot2::theme_minimal() +
      ggplot2::ggtitle(label = "")

  })

  ## render power plot
  output$power_plot <- shiny::renderPlot({

    output_power_plot()

  })

  #### Meta-Analysis

  ## import csv file & add info columns
  csv_import_ES <- shiny::eventReactive(input$import_csv_ES, {

    ## store file specifications
    input_specifications <- base::as.data.frame(input$select_csv_ES)

    ## import csv
    data <- readr::read_csv(file = input_specifications$datapath)

    ## adjust SD according to input

    #### STILL MISSING

    data

  })

  ## import csv file & add info columns
  sim_data_ES <- shiny::reactive({
    base::data.frame(
      CM = rnorm(n = 15, mean = base::as.numeric(input$UL_scale_ES)/2.5, sd = 1/base::as.numeric(input$UL_scale_ES)*0.5),
      TM = rnorm(n = 15, mean = base::as.numeric(input$UL_scale_ES)/1.5, sd = 1/base::as.numeric(input$UL_scale_ES)**0.5),
      C_SD = base::sqrt(0.5^2 * base::as.numeric(input$UL_scale_ES)^2)/2+rnorm(15,0,1/base::as.numeric(input$UL_scale_ES)),
      T_SD = base::sqrt(0.5^2 * base::as.numeric(input$UL_scale_ES)^2)/2+rnorm(15,0,1/base::as.numeric(input$UL_scale_ES)),
      C_N = base::rep(40, times = 15),
      T_N = base::rep(40, times = 15))
  })

  ## create/update plot according to UI input
  output_plot_ES <- shiny::eventReactive(input$update_plot_ES, {

    scale_min <- base::as.numeric(input$LL_scale_ES)
    scale_max <- base::as.numeric(input$UL_scale_ES)
    step_size <- base::as.numeric(input$step_size_scale_ES)

    if(input$data_input_ES == "upload"){

      data <- base::as.data.frame(csv_import_ES())

      if(input$SD_type_ES == "estimated"){

        umbrella_fun(scale_min = scale_min,
                     scale_max = scale_max,
                     step_size = step_size,
                     CM = data$CM,
                     TM = data$TM,
                     C_SD = data$C_SD,
                     T_SD = data$T_SD,
                     C_N = data$C_N,
                     T_N = data$T_N,
                     parameter = "SD"
        )

      } else if (input$SD_type_ES == "observed"){

        umbrella_fun(scale_min = scale_min,
                     scale_max = scale_max,
                     step_size = step_size,
                     CM = data$CM,
                     TM = data$TM,
                     C_SD = data$C_SD,
                     T_SD = data$T_SD,
                     parameter = "SD"
        )

      }

    } else if(input$data_input_ES == "sim_data"){

      data <- base::as.data.frame(sim_data_ES())

      umbrella_fun(scale_min = scale_min,
                   scale_max = scale_max,
                   step_size = step_size,
                   CM = data$CM,
                   TM = data$TM,
                   C_SD = data$C_SD,
                   T_SD = data$T_SD,
                   parameter = "SD"
      )

    }


  })

  ## render umbrella plot
  output$umbrella_plot_ES <- shiny::renderPlot({

    output_plot_ES()

  })

  ## create meta-analytic output
  output_table_ES <- shiny::eventReactive(input$update_plot_ES,{

    if(input$data_input_ES == "upload"){

      data <- base::as.data.frame(csv_import_ES())

      MD_escalc_list <- base::lapply(1:base::nrow(data), function(row){metafor::escalc(measure = "MD", m1i = TM, m2i = CM, sd1i = T_SD, sd2i = C_SD, n1i = T_N, n2i = C_N, data = data[row,])})

      MD_escalc_df <- base::do.call(base::rbind, MD_escalc_list)

      MD_metafor_out <- metafor::rma(yi = MD_escalc_df$yi, vi = MD_escalc_df$vi)

      d_escalc_list <- base::lapply(1:base::nrow(data), function(row){metafor::escalc(measure = "SMD", m1i = TM, m2i = CM, sd1i = T_SD, sd2i = C_SD, n1i = T_N, n2i = C_N, data = data[row,])})

      d_escalc_df <- base::do.call(base::rbind, d_escalc_list)

      d_metafor_out <- metafor::rma(yi = d_escalc_df$yi, vi = d_escalc_df$vi)

      base::round(
        base::data.frame(
          MD_H2 = MD_metafor_out$H2,
          d_H2 = d_metafor_out$H2,
          MD_CV = base::sqrt(MD_metafor_out$tau2) / MD_metafor_out$b,
          d_CV = base::sqrt(d_metafor_out$tau2) / d_metafor_out$b
        ),
        digits = 2
      )

    } else if(input$data_input_ES == "sim_data"){

      data <- base::as.data.frame(sim_data_ES())

      MD_escalc_list <- base::lapply(1:base::nrow(data), function(row){metafor::escalc(measure = "MD", m1i = TM, m2i = CM, sd1i = T_SD, sd2i = C_SD, n1i = T_N, n2i = C_N, data = data[row,])})

      MD_escalc_df <- base::do.call(base::rbind, MD_escalc_list)

      MD_metafor_out <- metafor::rma(yi = MD_escalc_df$yi, vi = MD_escalc_df$vi)

      d_escalc_list <- base::lapply(1:base::nrow(data), function(row){metafor::escalc(measure = "SMD", m1i = TM, m2i = CM, sd1i = T_SD, sd2i = C_SD, n1i = T_N, n2i = C_N, data = data[row,])})

      d_escalc_df <- base::do.call(base::rbind, d_escalc_list)

      d_metafor_out <- metafor::rma(yi = d_escalc_df$yi, vi = d_escalc_df$vi)

      base::round(
        base::data.frame(
          MD_H2 = MD_metafor_out$H2,
          d_H2 = d_metafor_out$H2,
          MD_CV = base::sqrt(MD_metafor_out$tau2) / MD_metafor_out$b,
          d_CV = base::sqrt(d_metafor_out$tau2) / d_metafor_out$b
        ),
        digits = 2
      )


    }

  })

  ## render data table
  output$MA_table_ES <- DT::renderDT({
    output_table_ES()
  })


}

#####
