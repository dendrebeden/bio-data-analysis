source("modules.R")
# source("functions.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    globalList <- reactiveValues()
    globalList$dataAnalysis <- reactiveValues()
    globalList$loadedData <- reactiveValues()
    # Nevertheless PATIENTS and LABS table names are hardcoded in data analysis,
    # they are added to globalList$dataLoader$tablesNames to make changes in
    # data loader section easier.
    globalList$dataLoader <- reactiveValues(tablesNames = c("PATIENTS", "LABS"))
    
    observeEvent(globalList$dataLoader$tablesNames, {
        sapply(globalList$dataLoader$tablesNames, function(x) {
            globalList$loadedData[[x]] <- NULL
        })
    })
    
    output$mainPage <- renderUI({
        tagList(
            fluidRow(
                column(
                    width = 12,
                    uiOutput("dataLoadBox")
                )
            ),
            fluidRow(
                column(
                    width = 12,
                    uiOutput("dataAnalysisBox")
                )
            )
        )
    })
    
    ### DATA LOAD ###
    output$dataLoadBox <- renderUI({
        box(
            title = "Data loader box",
            collapsible = T,
            solidHeader = T,
            width = NULL,
            tagList(
                fluidRow(
                    column(
                        width = 4,
                        fileInput(
                            inputId = "dataLoaderBrowseFileBtn",
                            label = "Load bio data in .tsv format",
                            accept = ".tsv",
                            buttonLabel = "Browse..."
                        )
                    ),
                    column(
                        width = 1,
                        actionButton(
                            inputId = "dataLoaderLoadFileBtn",
                            label = "Load",
                            icon = icon("download"),
                            style = "margin-top: 25px;",
                            width = "100%"
                        )
                    ),
                    column(
                        width = 4,
                        selectInput(
                            inputId = "dataLoaderTableNameSclt",
                            label = "Set type of the table",
                            choices = globalList$dataLoader$tablesNames
                        )
                    )
                ),
                fluidRow(
                    column(
                        width = 12,
                        tags$h4("Table preview"),
                        DT::dataTableOutput("dataLoaderLoadedTablePreview"),
                        br()
                    )
                )
            )
        )
    })
    
    observeEvent(input$dataLoaderLoadFileBtn, {
        req(!is.null(input$dataLoaderLoadFileBtn),
            !is.null(input$dataLoaderBrowseFileBtn))
        
        dataTable <- tryCatch({
            fread(
                input = input$dataLoaderBrowseFileBtn$datapath,
                data.table = T
            )
        }, error = function(e) {
            # TO. DO: create alert
            return()
        })
        
        globalList$loadedData[[input$dataLoaderTableNameSclt]] <- dataTable
    })
    
    output$dataLoaderLoadedTablePreview <- DT::renderDataTable({
        
        previewTable <- globalList$loadedData[[input$dataLoaderTableNameSclt]]
        
        shiny::validate(need(!is.null(previewTable),
                        paste0("Table ", input$dataLoaderTableNameSclt, " has not been loaded."),
                        input$dataLoaderTableNameSclt))

        DT::datatable(
            data = as.data.frame(previewTable),
            rownames = F,
            colnames = names(previewTable),
            height = "400px"
        )
    })
    
    ### DATA ANALYSIS ###
    output$dataAnalysisBox <- renderUI({
        ui <- box(
            title = "Data analysis box",
            collapsible = T,
            solidHeader = T,
            width = NULL,
            if(is.null(globalList$loadedData$PATIENTS) ||
               is.null(globalList$loadedData$LABS)) {
                paste0("Load PATIENTS and LABS tables.")
            } else {
                tagList(
                    fluidRow(
                        column(
                            width = 12,
                            selectorsServerUI("main", "Main segmentation")
                        )
                    ),
                    fluidRow(
                        column(
                            width = 12,
                            selectorsServerUI("comparison", "Comparison segmentation")
                        )
                    ),
                    fluidRow(
                        column(
                            width = 12,
                            uiOutput("dataAnalysisPlotsTabBox")
                        )
                    )
                )
                
            }
        )
    })
    
    mainSelectorsChoices <- selectorsServer("main", globalList$loadedData$PATIENTS)
    comparisonSelectorsChoices <- selectorsServer("comparison", globalList$loadedData$PATIENTS)
    
    output$dataAnalysisPlotsTabBox <- renderUI({
        tagList(
            tabBox(
                width = 12,
                tabPanel(
                    "BMRKR1 distribution",
                    plotlyOutput("dataAnalysisPlotsBRMKR1Histogram")
                ),
                tabPanel(
                    "BMRKR2 division",
                    plotlyOutput("dataAnalysisPlotsBRMKR2PieChart")
                ),
                tabPanel(
                    "LBTEST average measurements",
                    plotlyOutput("dataAnalysisPlotsLBTESTAverageMeasurements")
                )
            )
        )
    })
    
    mainPlotsData <- reactive({
        globalList$loadedData$PATIENTS[
            AGE >= mainSelectorsChoices$ageSlider[[1]] &
                AGE <= mainSelectorsChoices$ageSlider[[2]] &
                SEX %in% mainSelectorsChoices$sexSlct &
                RACE %in% mainSelectorsChoices$raceSclt &
                ACTARM %in% mainSelectorsChoices$ACTARMSclt
        ]
    })
    
    comparisonPlotsData <- reactive({
        globalList$loadedData$PATIENTS[
            AGE >= comparisonSelectorsChoices$ageSlider[[1]] &
                AGE <= comparisonSelectorsChoices$ageSlider[[2]] &
                SEX %chin% comparisonSelectorsChoices$sexSlct &
                RACE %chin% comparisonSelectorsChoices$raceSclt &
                ACTARM %chin% comparisonSelectorsChoices$ACTARMSclt
        ]
    })
    
    mainBRMKRPlotsData <- reactive({
        merge(
            mainPlotsData(),
            unique(globalList$loadedData$LABS[
                ,c("STUDYID", "USUBJID", "BMRKR1", "BMRKR2")]),
            by = c("STUDYID", "USUBJID")
        )
    })
    
    comparisonBRMKRPlotsData <- reactive({
        merge(
            comparisonPlotsData(),
            unique(globalList$loadedData$LABS[
                ,c("STUDYID", "USUBJID", "BMRKR1", "BMRKR2")]),
            by = c("STUDYID", "USUBJID")
        )
    })
    
    mainLBTESTPlotsData <- reactive({
        globalList$loadedData$LABS[
            STUDYID %chin% unique(mainPlotsData()$STUDYID) &
                USUBJID %chin% unique(mainPlotsData()$USUBJID)
        ][, .(AVAL_AVERAGE = mean(AVAL)), by = .(AVISIT, LBTEST, AVALU)]
    })
    
    comparisonLBTESTPlotsData <- reactive({
        globalList$loadedData$LABS[
            STUDYID %chin% unique(comparisonPlotsData()$STUDYID) &
                USUBJID %chin% unique(comparisonPlotsData()$USUBJID)
        ][, .(AVAL_AVERAGE = mean(AVAL)), by = .(AVISIT, LBTEST, AVALU)]
    })
    
    output$dataAnalysisPlotsBRMKR1Histogram <- renderPlotly({
        mainPlot <- if(nrow(mainBRMKRPlotsData()) > 0) {
            plot_ly(
                type = "histogram",
                x = mainBRMKRPlotsData()$BMRKR1,
                name = "main",
                color = I("light green")
            )
        } else {
            plot_ly()
        }
        comparisonPlot <- if(nrow(comparisonBRMKRPlotsData()) > 0) {
            plot_ly(
                type = "histogram",
                x = comparisonBRMKRPlotsData()$BMRKR1,
                name = "comparison",
                color = I("light blue")
            )
        } else {
            plot_ly()
        }
        plot <- subplot(mainPlot, comparisonPlot, shareY = T) %>%
            layout(bargap=0.1,
                   annotations = list(
                       list(x = 0.2 ,
                            y = 1.05,
                            text = "main segmentation",
                            showarrow = F,
                            xref='paper',
                            yref='paper'),
                       list(x = 0.8,
                            y = 1.05,
                            text = "comparison segmentation",
                            showarrow = F,
                            xref='paper',
                            yref='paper')
                   )
            )
        
        plot
    })
    
    output$dataAnalysisPlotsBRMKR2PieChart <- renderPlotly({
        mainData <- mainBRMKRPlotsData() %>%
            .[order(rank(BMRKR2)), .N, by = .(BMRKR2)]
        comparisonData <- comparisonBRMKRPlotsData() %>%
            .[order(rank(BMRKR2)), .N, by = .(BMRKR2)]
        
        colors <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)')
        
        plot <- plot_ly()
        if(nrow(mainData) > 0) {
            plot %<>% add_pie(
                labels = mainData$BMRKR2,
                values = mainData$N,
                textinfo='label+percent',
                title = "main",
                domain = list(x = c(0, 0.45), y = c(0, 1)),
                marker = list(colors = colors,
                              line = list(color = '#FFFFFF', width = 1)))
        }
        if(nrow(comparisonData) > 0) {
            plot %<>% add_pie(
                labels = comparisonData$BMRKR2,
                values = comparisonData$N,
                textinfo='label+percent',
                title = "comparison",
                domain = list(x = c(0.55, 1), y = c(0, 1)),
                marker = list(colors = colors,
                              line = list(color = '#FFFFFF', width = 1)))
        }
        
        plot
    })
    
    output$dataAnalysisPlotsLBTESTAverageMeasurements <- renderPlotly({
        mainPlotData <- mainLBTESTPlotsData()
        comparisonPlotData <- comparisonLBTESTPlotsData()
        
        plotsList <- list()
        if(nrow(mainPlotData) > 0 || nrow(comparisonPlotData) > 0 ) {
            xlab = list(categoryorder = "array",
                        categoryarray = union(unique(mainPlotData$AVISIT),
                                              unique(comparisonPlotData$AVISIT)))
            LBTESTValues <- union(unique(mainPlotData$LBTEST),
                                  unique(comparisonPlotData$LBTEST))
            
            for(i in LBTESTValues) {
                
                if(i %in% unique(mainPlotData$LBTEST)) {
                    plot <- plot_ly(
                        type = 'scatter',
                        data = mainPlotData[LBTEST == i],
                        x = ~AVISIT,
                        y = ~AVAL_AVERAGE,
                        color = ~paste0(LBTEST, " main"),
                        colors = "Paired",
                        mode = "markers+lines")
                    
                    if(i %in% unique(comparisonPlotData$LBTEST)) {
                        plot %<>% add_trace(
                            type = 'scatter',
                            data = comparisonPlotData[LBTEST == i],
                            x = ~AVISIT,
                            y = ~AVAL_AVERAGE,
                            color = ~paste0(LBTEST, " comparison"),
                            colors = "Dark2",
                            mode = "markers+lines"
                        )
                    }
                    
                } else {
                    plot <- plot_ly(
                        type = 'scatter',
                        data = comparisonPlotData[LBTEST == i],
                        x = ~AVISIT,
                        y = ~AVAL_AVERAGE,
                        TITLE = i,
                        color = ~paste0(LBTEST, " comparison"),
                        colors = "Paired",
                        mode = "markers+lines")
                }
                
                plotsList[[i]] <- plotly_build(
                    plot %>% layout(
                        xaxis = xlab,
                        yaxis = list(
                            title = unique(
                                c(mainPlotData[LBTEST == i, AVALU],
                                  comparisonPlotData[LBTEST == i, AVALU]))
                        ),
                        title = i,
                        annotations = list(
                            list(x = 0.5,
                                 y = 1.05,
                                 text = i,
                                 showarrow = F,
                                 xref='paper',
                                 yref='paper'))))
                
            }
            
            return(subplot(plotsList,
                           nrows = length(LBTESTValues),
                           shareX = T))
        } else {
            plot_ly()
        }
    })
})