# Simple Shiny app for Palmer Penguins data
# author: Emma Kroell
# References:
# Paul C. Bauer, Applied Data Visualization (with R), Chapter 11: Interactive
# data visualization: Shiny, https://bookdown.org/paul/applied-data-visualization
# /example-a-simple-regression-app.html, 2021.
# and
# Hadley Wickham, Mastering Shiny, https://mastering-shiny.org/index.html, 2020.

# libraries
library(shiny)
library(tidyverse)
library(palmerpenguins)

ui <- fluidPage(
  titlePanel("Simple Linear Regression with Palmer Penguins"),
  sidebarLayout(
    sidebarPanel(
      selectInput("outcome_var", label = h3("Response variable"),
                  choices = c("bill_length_mm", "bill_depth_mm",
                              "flipper_length_mm", "body_mass_g"), selected = 1),
      selectInput("explanatory_var", label = h3("Explanatory variable"),
                  choices = c("bill_depth_mm","bill_length_mm",
                              "flipper_length_mm", "body_mass_g"), selected = 1),
      checkboxInput("linear_fit", "Add linear regression fit"),
      conditionalPanel("input.linear_fit",
                       selectInput(inputId = "covariate",
                                   label = h3("Optional: group regression by"),
                                   choices = c("none","species","island","sex"),
                                   selected = 1)),
      hr(),
      HTML("This app allows you to fit a linear regression to the"),
      tags$a(href = "https://allisonhorst.github.io/palmerpenguins/","Palmer Penguins data set"),
      HTML(". You can specify the reponse and explanatory variables. 
           You can also add a categorical variable such as sex or species as a covariate.")
    ),
    mainPanel(plotOutput("scatterplot"))
    )
  )



# SERVER
server <- function(input, output) {

  # paste(names(penguins)[1], "~", paste(names(penguins)[-1], collapse=" + "))
  # scatterplot with optional linear (and other?) fits
  output$scatterplot <- renderPlot({
    penguins_mod <- drop_na(penguins)
    
    # fit linear model
    fit <- lm(paste(input$explanatory_var, "~",input$outcome_var),
              data = penguins_mod)
    
    if (input$explanatory_var == input$outcome_var){
      ggplot(penguins_mod, aes_string(x=input$explanatory_var, y=input$outcome_var)) +
        geom_point() + theme_bw(base_size = 16) +
        ggtitle("Same outcome and explanatory variable!")
    } else{
      if (input$linear_fit == "TRUE") {
        if (input$covariate != "none"){
          ggplot(penguins_mod, aes_string(x=input$explanatory_var,
                                          y=input$outcome_var,
                                          colour=input$covariate,
                                          fill=input$covariate)) +
            geom_smooth(method='lm', formula= y~x) +
            geom_point() + theme_bw(base_size = 16) +
            ggpubr::stat_regline_equation(aes(label = ..eq.label..),
                                          size = 8,
                                          show.legend = FALSE)
          
        } else{
          ggplot(penguins_mod, aes_string(x=input$explanatory_var,
                                          y=input$outcome_var)) +
            geom_smooth(method='lm', formula= y~x, colour = "black") +
            geom_point() + theme_bw(base_size = 16) +
            ggpubr::stat_regline_equation(aes(label = ..eq.label..),
                                          size = 8)
        }} else {
        ggplot(penguins_mod, aes_string(x=input$explanatory_var, y=input$outcome_var)) +
          geom_point() + theme_bw(base_size = 16)
      }}
  }, height=400)

}

shinyApp(ui = ui, server = server)
