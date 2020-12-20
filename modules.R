# dropdown with header
selectorsServerUI <- function(id, header) {
  ns <- NS(id)
  tagList(
    tags$h4(header),
    uiOutput(ns("selectors"))
  )
}

# dropDown module server
# input:
#  - choices   - character   - character vector with values for dropdown
#
# output:
#  selected dropdown value wrapped in reactive(...)
selectorsServer<- function(id, patientsDT) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      output$selectors <- renderUI({
        
        req(all(c("AGE", "SEX", "RACE") %chin% names(patientsDT)))
        
        tagList(
          column(
            width = 4,
            sliderInput(
              inputId = ns("ageSlider"),
              label = "Age",
              step = 1,
              min = min(patientsDT$AGE),
              max = max(patientsDT$AGE),
              value = c(min(patientsDT$AGE), max(patientsDT$AGE)),
              animate = T
            )
          ),
          column(
            width = 2,
            pickerInput(
              inputId = ns("sexSlct"),
              label = "Sex",
              choices = unique(patientsDT$SEX),
              selected = unique(patientsDT$SEX),
              multiple = T,
              options = pickerOptions(
                actionsBox = TRUE,
                size = 8
              )
            )
          ),
          column(
            width = 3,
            pickerInput(
              inputId = ns("raceSclt"),
              label = "Race",
              choices = unique(patientsDT$RACE),
              selected = unique(patientsDT$RACE),
              multiple = T,
              options = pickerOptions(
                actionsBox = TRUE,
                size = 8
              )
            )
          ),
          column(
            width = 3,
            pickerInput(
              inputId = ns("ACTARMSclt"),
              label = "ACTARM",
              choices = unique(patientsDT$ACTARM),
              selected = unique(patientsDT$ACTARM),
              multiple = T,
              options = pickerOptions(
                actionsBox = TRUE,
                size = 8
              )
            )
          )
        )
      })
      
      return(input)
    })
}