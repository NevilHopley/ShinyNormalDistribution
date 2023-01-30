library(shiny)
library(ggplot2)
library(dplyr)
library(colourpicker)

# global variables
y_min = -0.08
y_max = 0.45

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # CSS code for black horizontal lines
  tags$head(
    tags$style(HTML("hr {border-top: 1px solid #000000;}")),
    tags$style("
      .checkbox {
        margin-bottom: -5px;
        margin-top: 0px;
      }"),
    #tags$head(tags$style(".tab-content {overflow: visible;}")),
    tags$style(HTML('.shiny-split-layout>div {overflow: hidden;}')) # to prevent horizontal scroll bars
  ),
  
  # Application title
  titlePanel("Normal Distribution Graph Generator"),
  ## Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "rv_letter",
                  label = "Random Variable letter",
                  selected = "Z",
                  choices = LETTERS[1:26],
                  width = "50%"
      ),
      fluidRow(splitLayout(#cellWidths = c("30%", "30%"),
        verticalLayout(
          checkboxInput(
            inputId = "show_rv_letter",
            label = "Show variable letter",
            value = TRUE
          ),
          checkboxInput(
            inputId = "show_rv_parameters",
            label = "Show parameters",
            value = TRUE
          )),
        verticalLayout(
          checkboxInput(
            inputId = "show_axis_values",
            label = "Show axis values",
            value = TRUE
          ),
          checkboxInput(
            inputId = "show_axis_mean",
            label = "Show mean on axis",
            value = TRUE
          )),
      )),
      fluidRow(splitLayout(
        div(style = "white-space: nowrap;", 
            h5("μ =  ",style="display:inline-block"),
            div(style="display: inline-block; width: 80%;",
                numericInput(
                  inputId = "rv_mu",
                  label = NULL,
                  value = 0,
                  width = "80%"
                )
            )
        ),
        actionButton(inputId = "rv_spread",
                     label = "st. deviation, σ = ",
                     class = "btn-primary"
        ),
        numericInput(
          inputId = "rv_sigma",
          label = NULL,
          value = 1,
          min = 0,
          width = "70%"
        )
      )),
      
      hr(), # horizontal line
      
      fluidRow(splitLayout(
        numericInput(
          inputId = "x_lower_value",
          label = "Lower value",
          value = -1.68,
          step = 0.01,
          width = "70%"
        ),
        numericInput(
          inputId = "x_upper_value",
          label = "Upper value",
          value = 1.68,
          step = 0.01,
          width = "70%"
        )
      )),
      fluidRow(splitLayout(
        checkboxInput(
          inputId = "show_x_lower_value",
          label = "Show lower value",
          value = TRUE
        ),
        checkboxInput(
          inputId = "show_x_upper_value",
          label = "Show upper value",
          value = TRUE
        )
      )),
      hr(), # horizontal line
      fluidRow(
        column(width = 12, 
               actionButton(inputId = "value_prob",
                            label = "Updating Probabilities changes Values",
                            icon = icon("circle-up"),
                            class = "btn-primary"
               )
        ),
        align = "center" # centres this button in the left sidepanel
      ),
      
      hr(), # horizontal line
      
      fluidRow(splitLayout(
        numericInput(
          inputId = "p_lower_value",
          label = "Lower probability",
          value = pnorm(-1.68, 0, 1, lower.tail = TRUE),
          min = 0,
          max = 1,
          step = 0.01,
          width = "70%"
        ),
        numericInput(
          inputId = "p_upper_value",
          label = "Upper probability",
          value = pnorm(1.68, 0, 1, lower.tail = FALSE),
          min = 0,
          max = 1,
          step = 0.01,
          width = "70%"
        )
      )),
      
      fluidRow(splitLayout(
        checkboxInput(
          inputId = "show_p_lower_shade",
          label = "Shade lower",
          value = TRUE
        ),
        checkboxInput(
          inputId = "show_p_middle_shade",
          label = "Shade middle",
          value = FALSE
        ),
        checkboxInput(
          inputId = "show_p_upper_shade",
          label = "Shade upper",
          value = TRUE
        )
      )),
      fluidRow(
        splitLayout(
          column(width=1,
                 fluidRow(style = "height:1800px;",
                          colourInput(
                            inputId = "p_lower_colour",
                            label = "",
                            value = "",
                            showColour = "background",
                            palette = "limited",
                            closeOnClick = TRUE
                          ))
          ),
          column(width=1,
                 fluidRow(style = "height:1800px;",
                          colourInput(
                            inputId = "p_middle_colour",
                            label = "",
                            value = "",
                            showColour = "background",
                            palette = "limited",
                            closeOnClick = TRUE
                          ))
          ),
          column(width=1,
                 fluidRow(style = "height:180px;",
                          colourInput(
                            inputId = "p_upper_colour",
                            label = "",
                            value = "",
                            showColour = "background",
                            palette = "limited",
                            closeOnClick = TRUE
                          ))
          )
        )
      )
    ), # end of side bar panel function
    
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("plot"),
      helpText("Cick on the 'st. deviation' button to enter either standard deviation or variance."),
      helpText("Cick on the 'Updating Probabilities changes Values' button to change to 'Updating Values changes Probabilities."),
      helpText("Be careful when updating values or probabilities to avoid creating an image where the information is inconsistent."),
      helpText("When ready, right click on the diagram and choose an option to export the image."),
      helpText("To reset, reload this web page, using CTRL+R"),
    )
  )
) # end fluidpage function

# Define server logic required to draw scatterplot with regression line
server <- function(input, output, session) {
  
  # define reactive values 
  x = reactiveValues(axis = NULL,
                     lim_min = NULL,
                     lim_max = NULL)
  
  # definte reactive values for spread of rv
  rv = reactiveValues(sigma = NULL)
  
  # input$value_prob %% 2 = 1 means values change probabilities
  # input$value_prob %% 2 = 0 means probabilities change values
  observeEvent(input$value_prob,
               {if(input$value_prob %% 2 == 0) {
                 updateActionButton(inputId = "value_prob",
                                    label = "Updating Probabilities changes Values",
                                    icon = icon("circle-up")
                 )
               } else {
                 updateActionButton(inputId = "value_prob",
                                    label = "Updating Values changes Probabilities",
                                    icon = icon("circle-down")
                 )
               }
               })
  
  
  observeEvent(input$rv_spread | input$rv_sigma,
               {if(input$rv_spread %% 2 == 0) {
                 updateActionButton(inputId = "rv_spread",
                                    label = "st. deviation, σ =")
                 rv$sigma = input$rv_sigma
               } else {
                 updateActionButton(inputId = "rv_spread",
                                    label = "variance, σ² = ")
                 rv$sigma = sqrt(input$rv_sigma)
               }
               })
  
  observeEvent(input$show_rv_letter,
               {
                 updateCheckboxInput(session,
                                     inputId = "show_rv_parameters",
                                     value = input$show_rv_letter)
               }
  )
  
  observeEvent(input$rv_mu | input$rv_sigma | input$rv_spread,
               {
                 x_lims = 4
                 x$lim_min = input$rv_mu - x_lims * rv$sigma
                 x$lim_max = input$rv_mu + x_lims * rv$sigma
                 z_axis = c(-3, -2, -1, 1, 2, 3)
                 x$axis = round(input$rv_mu + z_axis * rv$sigma, 2)
               }
  )
  
  observeEvent(input$x_lower_value | input$rv_mu | input$rv_sigma | input$rv_spread,
               {if (input$value_prob %% 2 == 1) {updateNumericInput(session,
                                                                    inputId = "p_lower_value",
                                                                    value = pnorm(input$x_lower_value,
                                                                                  mean = input$rv_mu,
                                                                                  sd = rv$sigma,
                                                                                  lower.tail = TRUE)
               )
               } 
               }
  )
  
  observeEvent(input$p_lower_value | input$rv_mu | input$rv_sigma | input$rv_spread,
               {if (input$value_prob %% 2 == 0) {updateNumericInput(session,
                                                                    inputId = "x_lower_value",
                                                                    value = qnorm(input$p_lower_value,
                                                                                  mean = input$rv_mu,
                                                                                  sd = rv$sigma,
                                                                                  lower.tail = TRUE)
               )
               } 
               }
  )
  
  observeEvent(input$x_upper_value | input$rv_mu | input$rv_sigma | input$rv_spread,
               {if (input$value_prob %% 2 == 1) {updateNumericInput(session,
                                                                    inputId = "p_upper_value",
                                                                    value = pnorm(input$x_upper_value,
                                                                                  mean = input$rv_mu,
                                                                                  sd = rv$sigma,
                                                                                  lower.tail = FALSE)
               )
               } 
               }
  )
  
  observeEvent(input$p_upper_value | input$rv_mu | input$rv_sigma | input$rv_spread,
               {if (input$value_prob %% 2 == 0) {updateNumericInput(session,
                                                                    inputId = "x_upper_value",
                                                                    value = qnorm(input$p_upper_value,
                                                                                  mean = input$rv_mu,
                                                                                  sd = rv$sigma,
                                                                                  lower.tail = FALSE)
               )
               } 
               }
  )
  
  # detect any significant mis-match between values and probabilties
  # observeEvent(input$p_upper_value | input$p_lower_value | input$x_upper_value | input$x_lower_value,
  #              {if (abs(input$x_upper_value - qnorm(input$p_upper_value,
  #                                                mean = input$rv_mu,
  #                                                sd = rv$sigma,
  #                                                lower.tail = FALSE)) > 0.01 |
  #                   abs(input$x_lower_value - qnorm(input$p_lower_value,
  #                                                mean = input$rv_mu,
  #                                                sd = rv$sigma,
  #                                                lower.tail = TRUE)) > 0.01)
  #              {showNotification("To keep consistency, first press the 'Updated...' button",
  #                                type = "error")
  #              }
  #                }
  #              )
  
  output$plot = renderPlot({
    
    x_values = seq(from = x$lim_min,
                   to = x$lim_max,
                   by = (x$lim_max - x$lim_min)/640)
    
    df = data.frame(x = x_values,
                    y = dnorm(x = x_values,
                              mean = input$rv_mu,
                              sd = rv$sigma
                    )
    )
    
    y_max = 1.2 * dnorm(x = input$rv_mu,
                        mean = input$rv_mu,
                        sd = rv$sigma)
    y_min = y_max / -6
    
    x_axis = x$axis
    x_mean = input$rv_mu
    text_size = 8
    
    g = ggplot() +
      geom_line(data = df,
                mapping = aes(x = x,
                              y = y)) +
      theme(panel.background = element_blank(),
            panel.grid.major = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank()) +
      geom_segment(mapping = aes(x = x$lim_min, y = 0,
                                 xend = x$lim_max, yend = 0)) +
      geom_segment(mapping = aes(x = input$rv_mu, y = y_min / 4,
                                 xend = input$rv_mu, yend = y_max)) +
      geom_segment(mapping = aes(x = input$rv_mu, y = y_min / 4,
                                 xend = input$rv_mu, yend = y_min),
                   fill = "white",
                   colour = "white")
    
    if (input$show_axis_values) {
      g = g + geom_segment(data = data.frame(x$axis),
                           mapping = aes(x = x$axis, y = 0,
                                         xend = x$axis, yend = y_min / 4)) +
        #scale_x_continuous(breaks = seq(-3,3)) +
        geom_text(data = data.frame(x = x_axis, y = 0),
                  mapping = aes(x = x_axis,
                                y = 0,
                                label = x_axis,
                                vjust = 2
                  ),
                  size = text_size * 0.8)
    }
    
    if (input$show_axis_mean) {
      g = g + geom_text(data = data.frame(x = x_mean, y = 0),
                        mapping = aes(x = x_mean,
                                      y = 0,
                                      label = x_mean,
                                      vjust = 2
                        ),
                        size = text_size * 0.8)
    }
    
    if (input$show_rv_letter & input$show_rv_parameters & input$rv_spread %% 2 == 0) {
      g = g + annotate(geom = "text",
                       x = input$rv_mu + 2 * rv$sigma,
                       y = 0.9 * y_max,
                       size = text_size,
                       label = paste0(toupper(input$rv_letter)," * '~' * N(", input$rv_mu, ",", rv$sigma, "^2)"),
                       parse = TRUE)
    }
    
    if (input$show_rv_letter & input$show_rv_parameters & input$rv_spread %% 2 != 0) {
      g = g + annotate(geom = "text",
                       x = input$rv_mu + 2 * rv$sigma,
                       y = 0.9 * y_max,
                       size = text_size,
                       label = paste0(toupper(input$rv_letter)," * '~' * N(", input$rv_mu, ",", input$rv_sigma, ")"),
                       parse = TRUE)
    }
    
    if (input$show_rv_letter & !input$show_rv_parameters)  {
      g = g + annotate(geom = "text",
                       x = input$rv_mu + 2 * rv$sigma,
                       y = 0.9 * y_max,
                       size = text_size,
                       label = paste0(toupper(input$rv_letter)),
                       parse = TRUE)
    }
    
    # upper x- value on horizontal axis
    if (input$show_x_upper_value) {
      g = g + annotate(geom = "text",
                       x = input$x_upper_value, y = 0,
                       size = text_size,
                       label = paste(tolower(input$rv_letter), "=", round(input$x_upper_value,2)),
                       vjust = 3.5,
                       hjust = 0.3) +
        geom_segment(mapping = aes(x = input$x_upper_value, y = y_min / 2,
                                   xend = input$x_upper_value, yend = 0))
    }
    
    # shaded prob_upper
    if (input$show_p_upper_shade) {
      g = g + geom_area(data = subset(df, x >= input$x_upper_value),
                        mapping = aes(x = x,
                                      y = y),
                        fill = tolower(input$p_upper_colour)) +
        # shaded prob_upper label
        annotate(geom = "text",
                 x = input$rv_mu + 3 * rv$sigma,
                 y = dnorm(input$rv_mu + 3 * rv$sigma),
                 size = text_size,
                 label = paste0(round(input$p_upper_value, 4)*100, "%"),
                 vjust = -2,
                 hjust = 0) +
        annotate(geom = "segment",
                 x = input$rv_mu + 3 * rv$sigma,
                 xend = input$x_upper_value,
                 y = 0.1 * y_max,
                 yend = 0,
                 colour = tolower(input$p_upper_colour))
    }
    # lower x- value on horizontal axis
    if (input$show_x_lower_value) {
      g = g + annotate(geom = "text",
                       x = input$x_lower_value, y = 0,
                       size = text_size,
                       label = paste(tolower(input$rv_letter), "=", round(input$x_lower_value,2)),
                       vjust = 3.5,
                       hjust = 0.3) +
        geom_segment(mapping = aes(x = input$x_lower_value, y = y_min / 2,
                                   xend = input$x_lower_value, yend = 0))
    }
    
    # shaded prob_middle
    if (input$show_p_middle_shade) {
      g = g + geom_area(data = subset(df, x >= input$x_lower_value & x<=input$x_upper_value),
                        mapping = aes(x = x,
                                      y = y),
                        fill = tolower(input$p_middle_colour)) + 
        #shaded prob_middle label
        annotate(geom = "text",
                 x = (input$x_lower_value + input$x_upper_value) / 2,
                 y = dnorm((input$x_lower_value + input$x_upper_value) / 2),
                 size = text_size,
                 label = paste0(round(1 - input$p_lower_value - input$p_upper_value, 4)*100, "%"),
                 vjust = -2,
                 hjust = 0.5)
    }
    
    # shaded prob_lower
    if (input$show_p_lower_shade) {
      g = g + geom_area(data = subset(df, x <= input$x_lower_value),
                        mapping = aes(x = x,
                                      y = y),
                        fill = tolower(input$p_lower_colour)) + 
        #shaded prob_lower label
        annotate(geom = "text",
                 x = input$rv_mu - 3 * rv$sigma,
                 y = dnorm(input$rv_mu - 3 * rv$sigma),
                 size = text_size,
                 label = paste0(round(input$p_lower_value, 4)*100, "%"),
                 vjust = -2,
                 hjust = 1) +
        annotate(geom = "segment",
                 x = input$rv_mu - 3 * rv$sigma,
                 xend = input$x_lower_value,
                 y = 0.1 * y_max,
                 yend = 0,
                 colour = tolower(input$p_lower_colour))
    }
    g
  }) # end renderPlot
  
} # end server function

# Run the application 
shinyApp(ui = ui, server = server)