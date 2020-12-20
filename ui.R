librariesList <- c("shiny", "data.table", "magrittr", "utils", "plotly", "bit64",
                   "leaflet", "lubridate", "shinydashboard", "shinyWidgets")
for(i in librariesList) {
  if(!i %in% rownames(installed.packages())){
    install.packages(i)
  }
  library(i, character.only = T)
}

# Define UI for application that draws a histogram
shinyUI(dashboardPage(
  dashboardHeader(disable = T),
  dashboardSidebar(disable = T),
  dashboardBody(
    tabItem(
      tabName = "Bio data analysis",
      uiOutput("mainPage")
    )
  )
))
