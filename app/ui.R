### UI
#####

ui <- shiny::navbarPage(

  title = "Shiny Umbrellas",
  #### LANDING PAGE

  shiny::tabPanel(
    title = "General Info",

    tags$img(src = "https://raw.githubusercontent.com/JensFuenderich/Under_my_Umbrella-Rating_Scales/refs/heads/main/myhex_logo.svg",
             height = "200px"),

    shiny::mainPanel(
      shiny::h3("Overview"),
      shiny::p("Shiny Umbrellas contains four interfaces to umbrella plots that demonstrate the association between means and standard deviations of rating scale data.
      The 'Discrete Umbrella' describes this association in its dependency on the sample size, and the other three tabs are use cases of a formalization that generalizes across sample sizes."),
      shiny::h4("Discrete Umbrella"),
      shiny::p("Heathers et al. (2018) visualize the possible combinations of means and standard deviations for a five point scale with ten participants.
        This tab provides a (for computational reasons) very limited interface to simulate this illustration.
        The algorithm identifies individual participant data sets using the gtools package (Warnes et al., 2023)."),
      shiny::h4("Error Checking"),
      shiny::p("In 'DEBIT: A simple consistency test for binary data', Heathers & Brown (2019) use a Bernoulli-formalization to detect inconsistencies in reports of means an standard deviations for binary data.
        In 'Under my Umbrella: Rating Scales Obscure Statistical Power and Effect Size Heterogeneity' (Jens H. Fünderich, Lukas J. Beinhauer, Frank Renkewitz, 2025), we use a similar Bernoulli-formalization to identify the umbrella from Heathers et al. (2018) that is independent of the sample size.
        This provides a computationally quick, but - especially in smaller samples - less precise method than SPRITE for checking the congruence between information on a scale, mean and standard deviation."),
      shiny::h4("Power"),
      shiny::p("A priori power analyses require us to make assumptions around both, the unstandardized effect and the expected variability, the standard deviations.
        The restrictions of the scale allow us to identify the largest possible standard deviations for an assumed mean difference.
        This restrictions applies to a single rating scale and aggregations of any number of identical rating scales (a measure with five 7-point rating scales, for example).
        Considering such restrictions in a priori power calculations can contribute to an appropriate allocation of resources."),
      shiny::h4("Meta-Analysis"),
      shiny::p("The pooled standard deviation is a weighted average of the standard deviations of both groups.
        Thus, in the case of data from a rating scale, we standardize with a pooled_SD that is affected by the restrictions described by the umbrella.
        This can lead to deviations between the distributions of d and MD, making the identified heterogeneity dependent on the choice to standardize the effects or not. "),
      shiny::h3("Ressources"),
      tags$a(href = "https://osf.io/preprints/osf/7au4q_v3",
             "Under my Umbrella: Rating Scales Obscure Statistical Power and Effect Size Heterogeneity (Fünderich et al., 2025)"),
      shiny::br(),
      tags$a(href = "https://osf.io/5vb3u",
             "Heathers & Brown (2019)"),
      shiny::br(),
      tags$a(href = "https://doi.org/10.7287/peerj.preprints.26968v1",
             "Heathers et al. (2018)"),
      shiny::br(),
      shiny::h3("About"),
      p("This Shiny App was developed by Jens H. Fünderich."),
      tags$a(href = "https://orcid.org/0000-0002-7185-9248",
             "ORCID"),
      p("Version: 0.2.0"),
      tags$a(href = "https://github.com/JensFuenderich/Under_my_Umbrella-Rating_Scales",
             "GitHub Repository with the code for this Shiny App.")
    )

  ),

  #### DISCRETE UMBRELLA

  shiny::tabPanel(
    title = "Discrete Umbrella",

    shiny::sidebarLayout(

      shiny::sidebarPanel(

        shiny::h3("Discrete Umbrella"),

        shiny::br(),

        ### set scale parameters

        shiny::h4("1. set scale parameters"),

        ## set lower scale limit
        shiny::textInput(
          inputId = "LL_scale_discrete",
          label = "set lower scale limit",
          value = "0"
        ),

        ## set upper scale limit
        shiny::textInput(
          inputId = "UL_scale_discrete",
          label = "set upper scale limit",
          value = "6"
        ),

        ## set sample size
        shiny::sliderInput(
          inputId = "n_discrete",
          label = "set the sample size",
          min = 2,
          max = 20,
          value = 10
        ),

        ##  standard deviation / variance
        shiny::radioButtons(
          inputId = "variability_est_discrete",
          label = "select the variability estimate",
          choices = c(
            "standard deviation" = "SD",
            "variance" = "VAR")
        ),

        ### create/update the plot
        shiny::actionButton(
          inputId = "update_plot_discrete",
          label = "create/update plot"
        )

      ),

      shiny::mainPanel(

        shiny::plotOutput("umbrella_plot_discrete")

      )

    )
  ),

  #### ERROR CHECKING

  shiny::tabPanel(
    title = "Error Checking",

    shiny::sidebarLayout(

      shiny::sidebarPanel(

        shiny::h3("Check for Inconsistencies"),

        shiny::br(),

        ### set scale parameters

        shiny::h4("1. set scale parameters"),

        ## set lower scale limit
        shiny::textInput(
          inputId = "LL_scale_error",
          label = "set lower scale limit",
          value = "0"
        ),

        ## set upper scale limit
        shiny::textInput(
          inputId = "UL_scale_error",
          label = "set upper scale limit",
          value = "6"
        ),

        ## set step-size
        shiny::textInput(
          inputId = "step_size_scale_error",
          label = "set scale step size ",
          value = "1"
        ),

        shiny::br(),

        ### add csv data to the plot

        shiny::h4("2. insert data"),


        ## sample standard deviation
        shiny::radioButtons(
          inputId = "data_input",
          label = "select data to inspect",
          choices = c(
            "display umbrella without data" = "no_data",
            "display simulated example" = "sim_data",
            "data from a csv file" = "upload",
            "manual data" = "manual")
        ),

        shiny::conditionalPanel(
          condition = "input.data_input == 'upload'",

          shiny::h5(shiny::strong("import csv file")),

          shiny::helpText("Make sure to handle sensitive data properly. The csv needs to contain the columns 'ID', 'x' (for the mean), and 'y' (for the standard deviation)."),

          ## select csv data to import
          shiny::fileInput(
            inputId = "select_csv",
            label = "2.1 select csv file",
            multiple = FALSE,
            accept = ".csv"
          ),

          ## sample standard deviation
          shiny::radioButtons(
            inputId = "SD_type",
            label = "2.2 select standard deviation type",
            choices = c("observed SD" = "observed",
                        "estimated SD (n-1)" = "estimated")
          ),

          shiny::helpText("The umbrella depicts observed SD. Set the sample size to correct for (n-1). The implementation assumes n to be consistent across data points in the file."),

          ## set sample size for correction of SD
          shiny::textInput(
            inputId = "sample_size",
            label = "2.3 set sample size"
          ),

          ## import file
          shiny::actionButton(
            inputId = "import_csv",
            label = "import/update csv"
          )

        ),

        shiny::conditionalPanel(
          condition = "input.data_input == 'manual'",

          ### insert a data point

          shiny::h5(shiny::strong("manually insert data point")),

          ## set lower scale limit
          shiny::textInput(
            inputId = "man_x",
            label = "2.1 set x-value",
            value = "3"
          ),

          ## set upper scale limit
          shiny::textInput(
            inputId = "man_y",
            label = "2.2 set y-value",
            value = "1.5"
          )

        ),

        shiny::br(),

        shiny::h4("3. create the umbrella"),

        ### create/update the plot
        shiny::actionButton(
          inputId = "update_plot_error",
          label = "create/update plot"
        )

      ),

      shiny::mainPanel(

        shiny::plotOutput("umbrella_plot",
                          click = "plot_click"),

        DT::DTOutput(
          outputId = "data_table",

        )


      )


    )


  ),

  #### POWER

  shiny::tabPanel(
    title = "Power",

    shiny::sidebarLayout(

      shiny::sidebarPanel(

        shiny::h3("Maximum SD for Power Analysis"),

        shiny::br(),

        ### set scale parameters

        shiny::h4("1. set scale parameters"),

        ## set lower scale limit
        shiny::textInput(
          inputId = "LL_scale_power",
          label = "set lower scale limit",
          value = "0"
        ),

        ## set upper scale limit
        shiny::textInput(
          inputId = "UL_scale_power",
          label = "set upper scale limit",
          value = "6"
        ),

        ## set step-size
        shiny::textInput(
          inputId = "step_size_power",
          label = "set scale step size",
          value = "1"
        ),

        shiny::br(),

        ### set MD

        shiny::h4("2. set minimally interesting MD"),

        shiny::textInput(
          inputId = "set_MD_power",
          label = "set MD",
          value = "2"
        ),

        shiny::br(),

        shiny::h4("3. identify the pooled SD for scale & MD"),

        ## create/update the plot
        shiny::actionButton(
          inputId = "update_plot_power",
          label = "Identify max(SD)"
        )

      ),

      shiny::mainPanel(

        shiny::plotOutput("umbrella_plot_power"),
        shiny::plotOutput("power_plot")

      )

    )

  ),

  #### Meta-Analysis

  shiny::tabPanel(
    title = "Meta-Analysis",

    shiny::sidebarLayout(

      shiny::sidebarPanel(

        shiny::h3("Visualize Meta-Analytic Data"),

        shiny::br(),

        ### set scale parameters

        shiny::h4("1. set scale parameters"),

        ## set lower scale limit
        shiny::textInput(
          inputId = "LL_scale_ES",
          label = "set lower scale limit",
          value = "0"
        ),

        ## set upper scale limit
        shiny::textInput(
          inputId = "UL_scale_ES",
          label = "set upper scale limit",
          value = "6"
        ),

        ## set step-size
        shiny::textInput(
          inputId = "step_size_scale_ES",
          label = "set scale step size ",
          value = "1"
        ),

        shiny::br(),

        shiny::h4("2. select data"),

        ## select data to be displayed
        shiny::radioButtons(
          inputId = "data_input_ES",
          label = "select data to inspect",
          choices = c(
            "display simulated example" = "sim_data",
            "data from a csv file" = "upload")
        ),

        ## only display for data upload
        shiny::conditionalPanel(

          condition = "input.data_input_ES == 'upload'",

          shiny::helpText("Make sure to handle sensitive data properly.
        The data set needs to contain the columns CM, TM, C_SD, T_SD.
        If it contains observed SD, it should also contain the columns C_N and T_N."),

          ## select csv data to import
          shiny::fileInput(
            inputId = "select_csv_ES",
            label = "select csv file",
            multiple = FALSE,
            accept = ".csv"
          ),

          ## sample standard deviation
          shiny::radioButtons(
            inputId = "SD_type_ES",
            label = "select variability measure",
            choices = c("observed SD" = "observed",
                        "estimated SD (n-1)" = "estimated")
          ),

          ## import file
          shiny::actionButton(
            inputId = "import_csv_ES",
            label = "import/update csv"
          ),


        ),

        shiny::br(),
        shiny::br(),

        shiny::h4("3. display data within the umbrella"),

        ### create/update the plot
        shiny::actionButton(
          inputId = "update_plot_ES",
          label = "create/update plot"
        )


      ),

      shiny::mainPanel(

        shiny::plotOutput("umbrella_plot_ES"),
        DT::DTOutput("MA_table_ES")

      )


    )

  )

)

#####
