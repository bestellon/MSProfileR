reportingUI <- function(id) {
  ns <- NS(id)
  tagList(
    box(title="Reporting (.pdf file)", width = 12, solidHeader = TRUE, status = "primary",
        downloadButton(ns("report"), "Download report")
    ),
    box(title="Parameters (.json file)", width = 12, solidHeader = TRUE, status = "primary",
        downloadButton(ns("parameters"), "Download parameters")
    ),
    box(title="Intensity matrix (.csv file)", width = 12, solidHeader = TRUE, status = "primary",
        uiOutput(ns("intensityMatrixUI"))
    ),
    box(title="Figures (.svg files)", width = 12, solidHeader = TRUE, status = "primary",
        downloadButton(ns("figures"), "Download figures")
    ),
    box(title="HDF5 file (.h5 file)", width = 12, solidHeader = TRUE, status = "primary",
        checkboxInput(ns('h5WithParameters'), label='include parameters', value=TRUE),
        uiOutput(ns("h5WithSpectraUI")),
        uiOutput(ns("h5WithIntensityMatrixUI")),
        uiOutput(ns("h5WithAnnotationsUI")),
        downloadButton(ns("h5File"), "Download HDF5 file")
    ),
    box(title="All files", width = 12, solidHeader = TRUE, status = "primary",
        downloadButton(ns("allFiles"), "Download all files")
    )
  )
}

reportingServer <- function(id, values) {
  moduleServer(
    id,
    function(input, output, session) {
      output$report <- downloadHandler(filename = "report.pdf", 
                                       content = function(file) { writeReport(file, values) })
      
      output$parameters <- downloadHandler(filename = "parameters.json", 
                                           content = function(file) { writeParameters(file, values) })
      
      output$intensityMatrixUI <- renderUI({
        if (is.null(values$intensityMatrix)) { return() }
        ns <- session$ns
        downloadButton(ns("intensityMatrix"), "Download intensity matrix")
      })
      
      output$intensityMatrix <- downloadHandler(filename = "intensity_matrix.csv", 
                                                content = function(file) { writeIntensityMatrixCSVFile(file, values) })
      
      output$figures <- downloadHandler(filename = "figures.zip", 
                                        content = function(file) { writeFiguresZip(file, values) })
      
      output$h5WithSpectraUI <- renderUI({
        if (is.null(values$mergedSpectra)) { return() }
        ns <- session$ns
        checkboxInput(ns('h5WithSpectra'), label='include averaged spectra', value=TRUE)
      })
      
      output$h5WithIntensityMatrixUI <- renderUI({
        if (is.null(values$intensityMatrix)) { return() }
        ns <- session$ns
        checkboxInput(ns('h5WithIntensityMatrix'), label='include intensity matrix', value=TRUE)
      })
      
      output$h5WithAnnotationsUI <- renderUI({
        if (is.null(values$annotations)) { return() }
        ns <- session$ns
        checkboxInput(ns('h5WithAnnotations'), label='include annotations', value=TRUE)
      })
      
      output$h5File <- downloadHandler(filename = "data.h5", 
                                       content = function(file) { 
                                         withParameters <- input$h5WithParameters
                                         withSpectra <- !is.null(values$mergedSpectra) && input$h5WithSpectra
                                         withIntensityMatrix <- !is.null(values$intensityMatrix) && input$h5WithIntensityMatrix
                                         withAnnotations <- !is.null(values$annotations) && input$h5WithAnnotations
                                         writeH5File(file, values, withParameters, withSpectra, withIntensityMatrix, withAnnotations) 
                                       })
      
      output$allFiles <- downloadHandler(filename = "all.zip", 
                                         content = function(file) { 
                                           withParameters <- input$h5WithParameters
                                           withSpectra <- !is.null(values$mergedSpectra) && input$h5WithSpectra
                                           withIntensityMatrix <- !is.null(values$intensityMatrix) && input$h5WithIntensityMatrix
                                           withAnnotations <- !is.null(values$annotations) && input$h5WithAnnotations
                                           writeAllFilesZip(file, values, withParameters, withSpectra, withIntensityMatrix, withAnnotations) 
                                         })
    }
    
  )
}
