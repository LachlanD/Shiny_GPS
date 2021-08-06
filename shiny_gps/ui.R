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
    titlePanel("Victoria Geology and Vegetation information for GPS tracks"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            fileInput("gpx",
                      "Upload gpx file",
                      multiple = FALSE,
                      accept = c(".gpx")
                      ),
            sliderInput("range", 
                        label = "Range of interest:",
                        min = 1, max = 100, value = c(1, 100)
            ),
            radioButtons(
                "choice",
                label = "Display",
                choices = c("None", "Geology", "Vegetation"),
                selected = "None"
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distMap")
        )
        
        
    ),
        
    mainPanel(
        plotOutput("distPlot")
    )
    
    
    
    
    
    
))
