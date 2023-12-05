dataLoadingUI <- function(id) {
  ns <- NS(id)
  tagList(
    box(title="Data loading", width = 12, solidHeader = TRUE, status = "primary",

        br(),
        numericInput(
          inputId = ns("directoryLevel"),
          label = tags$span("Level of directories from sample name:",
                            tags$i(
                              class = "glyphicon glyphicon-info-sign",
                              style = "margin-left:10px;color:#0072B2;",
                              title = "Select the path of the folder containing the spectra files to analyse which is the number of folders generated between the sampleName and the fid or acqu files of Bruker"
                            )),
          value = 4
        ),
        shinyDirButton(ns("directory"),
                       "Choose spectra directory",
                       "Choose spectra directory"),
        icon("info-circle",class = "glyphicon glyphicon-info-sign", id=ns("icon_help"), style="margin-left:10px;color:#0072B2;"),
        bsTooltip(id = ns("icon_help"), title = "Select the mass spectrum directory to analyze from your computer",
                  placement = "right", trigger = "hover"),
        br(),
        br(),
        infoBoxOutput(ns("infoBox"), width=12),
        DT::dataTableOutput(ns("metadata"))
    ),



    box(title="Parameters", width = 12, solidHeader = TRUE, status = "primary",
        fileInput(ns('uploadParameters'),
                  label = tags$span("Upload parameters",
                                    tags$i(
                                      class = "glyphicon glyphicon-info-sign",
                                      style = "margin-left:10px;color:#0072B2;",
                                      title = "Loading previous parameters (optional)"
                                    )),
                  accept = ".json")
    ),
  )

}

dataLoadingServer <- function(id, values) {
  moduleServer(
    id,
    function(input, output, session) {

      volumes <- getVolumes()
      shinyDirChoose(input, 'directory', roots = volumes())

      directory <- reactive({
        directory <- parseDirPath(volumes(), input$directory)
        if (length(directory) == 0) NULL else directory[[1]]
      })

      message <- reactive({
        directory_ <- directory()
        spectra <- values$spectra
        if (is.null(directory_) || is.null(spectra)) { return('No spectra loaded...') }
        return(paste(length(values$spectra),
                     ' spectra loaded (', directory_,')', sep=''))
      })

      observeEvent(directory(), {
        directory_ <- directory()
        if (is.null(directory_)) { return() }
        if (is.null(input$directoryLevel)) { return() }
        values$spectra <- NULL
        values$spectra <- loadSpectra(directory_, input$directoryLevel)
        processAnnotations(values)
      })

      output$infoBox <- renderInfoBox({
        infoBox("Spectra", message(), icon = icon('upload'),
                color = 'purple', fill = TRUE)
      })

      output$metadata <- DT::renderDataTable(extractSpectraMetadata(values$spectra)[,c('sampleName','replicateName','acquisitionDate')])

      observeEvent(input$uploadParameters, {
        file <- input$uploadParameters
        ext <- tools::file_ext(file$datapath)
        req(file)
        validate(need(ext == "json", "Please upload a json file"))
        values$updateParameters <- fromJSON(file=file$datapath)
        values$updatePreprocessing <- TRUE
      })


    }
  )
}

