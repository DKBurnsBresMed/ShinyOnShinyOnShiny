# app.R
# 
# 
# 



library(shiny)

ui <- fluidPage(
  source("./ui.R")$value
)

server <- function(input, output, session) {
  source("./server.R")$value
}

# options(Shiny.fullstacktrace = TRUE)

shinyAppDir(
  appDir = "./",
  options = list(display.mode = "showcase"),
  enableBookmarking = "server",
)
