#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            uiOutput("UI_Nuis_slider"),
            bookmarkButton("save snapshot..."),
            actionBttn(
                inputId = "UI_updateInputSet",
                label = "update the input set",
                style = "unite",
                color = "success",
                size = "lg",
                block = TRUE,
                no_outline = TRUE
            ),
            verbatimTextOutput("debugout")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            uiOutput("UI_name_and_type"),
            uiOutput("UI_input_set"),
            verbatimTextOutput("global_test")
        )
    )
))
