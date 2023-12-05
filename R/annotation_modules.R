annotationUI <- function(id) {
  ns <- NS(id)
  tagList(
    box(title="Empty template for annotation table", width = 12, solidHeader = TRUE, status = "primary",
        uiOutput(ns("downloadAnnotationFileButton"))
    ),
    box(title="Upload annotation table", width = 12, solidHeader = TRUE, status = "primary",
        fileInput(ns('uploadAnnotationFile'),                   
                  label = tags$span("Upload an annotation table", 
                                    tags$i(
                                      class = "glyphicon glyphicon-info-sign", 
                                      style = "margin-left:10px;color:#0072B2;",
                                      title = "Upload an annotation table to add additional information for each spectrum"
                                    )),
                  accept = c('.csv','.xls','.xlsx')),
        uiOutput(ns("removeAnnotationsButton")),
        br(),
        DT::dataTableOutput(ns("raw_annotations"))
    ),
    box(title="Annotation processing", width = 12, solidHeader = TRUE, status = "primary",
        uiOutput(ns("annotationError")),
        DT::dataTableOutput(ns("annotationStatistics"))
    )
  )
}

annotationServer <- function(id, values) {
  moduleServer(
    id,
    function(input, output, session) {
      output$downloadAnnotationFileButton <- renderUI({
        if (is.null(values$spectra)) { return() }
        ns <- session$ns
        downloadButton(ns('downloadAnnotationFile'), "Download an empty annotation table")
      })
      
      output$downloadAnnotationFile <- downloadHandler(
        filename = "annotation.xlsx", 
        content = function(file) {
          writeAnnotationFile(file, values)
        }
      )
      
      output$removeAnnotationsButton <- renderUI({
        if (is.null(values$rawAnnotations)) { return() }
        ns <- session$ns
        actionButton(ns('removeAnnotations'), 'Remove annotations')
      })
      
      observeEvent(input$removeAnnotations, {
        if (input$removeAnnotations==0) { return() }
        values$rawAnnotations = NULL
        processAnnotations(values)
      })
      
      observeEvent(input$uploadAnnotationFile, {
        file <- input$uploadAnnotationFile
        ext <- tools::file_ext(file$datapath)
        req(file)
        validate(need(ext == "csv" || ext == "xls" || ext == "xlsx", "Please upload a csv/xls/xlsx file"))
        readAnnotationFile(file$datapath, values)
        processAnnotations(values)
      })
      
      output$raw_annotations <- DT::renderDataTable(values$rawAnnotations)
      
      output$annotationError <- renderUI({
        if (is.null(values$annotationError)) { return() }
        ns <- session$ns
        p(values$annotationError)
      })
      
      output$annotationStatistics <- DT::renderDataTable(values$annotationStatistics)
      
    }
  )
}