# Simple Shiny example
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
      selectInput("outcome_var", label = h3("Outcome variable"),
                  choices = c("bill_length_mm", "bill_depth_mm",
                              "flipper_length_mm", "body_mass_g"), selected = 1),
      selectInput("explanatory_var", label = h3("Explanatory variable"),
                  choices = colnames(penguins), selected = 1),
      checkboxInput("linear_fit", "Add linear regression fit")
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

    if (input$linear_fit == "TRUE") {
      if (input$explanatory_var == input$outcome_var){
        ggplot(penguins, aes_string(x=input$explanatory_var, y=input$outcome_var)) +
          geom_smooth(method='lm', formula= y~x, colour="black") +
          geom_point() + theme_bw(base_size = 16) +
          ggtitle("Perfect linear relationship: same outcome and explanatory variable!")
      } else {
        ggplot(penguins, aes_string(x=input$explanatory_var, y=input$outcome_var)) +
          geom_smooth(method='lm', formula= y~x, colour="black") +
          geom_point() + theme_bw(base_size = 16) +
          ggtitle(paste0("Linear model: y = ",round(coef(fit)[2],4),
                         "x + ", round(coef(fit)[1],4)))
      }
    } else {
      ggplot(penguins, aes_string(x=input$explanatory_var, y=input$outcome_var)) +
        geom_point() + theme_bw(base_size = 16)
    }
  }, height=400)

}

shinyApp(ui = ui, server = server)
