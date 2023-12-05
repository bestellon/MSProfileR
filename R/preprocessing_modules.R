
preprocessingUI <- function(id) {
  ns <- NS(id)
  tagList(
    # Trimming module
    box(title="Trimming", width = 12, solidHeader = TRUE, status = "primary",
        numericRangeInput(
          inputId = ns("spectraTrimmingRange"),
          label = tags$span("Range (m/z):",
                            tags$i(
                              class = "glyphicon glyphicon-info-sign",
                              style = "margin-left:10px;color:#0072B2;",
                              title = "Trim: Selection of the beginnings and ends of MS spectra range; Conformity test: automatic exclusion of empty, irregular and non-conform length of spectra"
                            )),
          value = c(2, 20)
        ),
        uiOutput(ns("trimActionButtons")),
        plotOutput(ns("massBoundsPlot"), height="120px", width="100%"),
        uiOutput(ns("downloadMassBoundsPlotButton"))
    ),
    # Conformity test module
    box(title="Conformity tests", width = 12, solidHeader = TRUE, status = "primary",
        DT::dataTableOutput(ns("conformityTests")),
        br(),
        checkboxInput(ns('excludeEmptySpectra'), label='Exclude empty spectra', value=TRUE),
        checkboxInput(ns('excludeIrregularSpectra'), label='Exclude irregular spectra', value=TRUE),
        uiOutput(ns("conformityTestActionButtons"))
    ),
    # cleaning module
    box(title="Cleaning", width = 12, solidHeader = TRUE, status = "primary",
        # Intensity transformation step
        radioButtons(ns("transformIntensityMethod"),label = tags$span("Method used to transform intensity:",
                                                                      tags$i(
                                                                        class = "glyphicon glyphicon-info-sign",
                                                                        style = "margin-left:10px;color:#0072B2;",
                                                                        title = "Variance stabilization : improve the graphical visualization of spectra by reducing the flattening effect exerted by the high-intensity peaks on the low-intensity peaks. Select one of the methods below"
                                                                      )), inline = TRUE,
                     choices = list("Sqrt" = "sqrt", "Log" = "log" ,"Log2" = "log2", "Log10" = "log10"),
                     selected = "sqrt"),

        # Smooth intensity step
        radioButtons(ns("smoothIntensityMethod"),
                     label = tags$span("Method used to smooth intensity:",
                                       tags$i(
                                         class = "glyphicon glyphicon-info-sign",
                                         style = "margin-left:10px;color:#0072B2;",
                                         title ="Reveal data partially obscured by noise : improve the precision of peak m/z values and peak detection. Select one of the methods below"
                                       )),

                     inline = TRUE,
                     choices = list("Savitsky-Golay" = "SavitzkyGolay", "Moving average" = "MovingAverage"),
                     selected = "SavitzkyGolay"),
        numericInput(ns("halfWindowSize"), "Half window size:", 10, min = 1, max = 100),
        # baseline correction step
        radioButtons(ns("removeBaselineMethod"),
                     label = tags$span("Method used to remove baseline:",
                                       tags$i(
                                         class = "glyphicon glyphicon-info-sign",
                                         style = "margin-left:10px;color:#0072B2;",
                                         title ="Correction of background noise altering the base level of mass spectra. Select one of the methods below"
                                       )),
                     inline = TRUE,
                     choices = list("SNIP" = "SNIP", "TopHat" = "TopHat", "Convex hull" = "ConvexHull", "Median" = "median"),
                     selected = "SNIP"),
        numericInput(ns("removeBaselineIterations"), "Number of iterations (only for SNIP):", 100, min = 1, max = 100),
        # Normalisation step
        radioButtons(ns("calibrateIntensityMethod"),
                     label = tags$span("Method used to normalize intensity:",
                                       tags$i(
                                         class = "glyphicon glyphicon-info-sign",
                                         style = "margin-left:10px;color:#0072B2;",
                                         title ="Normalization to achieve comparisons of intensities from each m/z values value between spectra. Select one of the methods below"
                                       )),

                     inline = TRUE,
                     choices = list("TIC" = "TIC", "PQN" = "PQN", "Median" = "median"),
                     selected = "TIC"),
        uiOutput(ns("cleanActionButtons")),
        br(),
        br(),
        uiOutput(ns("spectrumSelector")),
        plotOutput(ns("spectraSamplePlot")),
        uiOutput(ns("downloadspectraSamplePlotButton")),
    ),
    # the screening module (quality control)
    box(title="Quality Control", width = 12, solidHeader = TRUE, status = "primary",
        radioButtons(ns("scaleEstimator"),
                     label = tags$span( "Scale estimator:",
                                        tags$i(
                                          class = "glyphicon glyphicon-info-sign",
                                          style = "margin-left:10px;color:#0072B2;",
                                          title ="Detection of outliers spectra with a programmatic quality score : visualisation of Atypical spectra upper the threshold. The quality control is carried out with one of the five methods bellow. Outside the threshold indicated in red. Select one of the methods below"
                                        )),
                     inline = TRUE,
                     choices = list("Q" = "Q", "Median Absolute Deviation (MAD)" = "MAD"),
                     selected = "Q"),
        radioButtons(ns("atypicalIdentificationMethod"), label = "Method used for the identification of atypical spectra:", inline = TRUE,
                     choices = list("RC" = "RC", "Hampel" = "Hampel" , "ESD" = "ESD", "Boxplot" = "boxplot", "Adj.boxplot"= "adj.boxplot"),
                     selected = "RC"),
        numericInput(ns("threshold"), "Threshold:", 3, min = 0.1, max = 10, step=0.1),
        checkboxInput(ns("includeLowerSpectra"), "Include spectra below the lower threshold", value=TRUE),
        uiOutput(ns("screenActionButtons")),
        br(), br(),
        tags$b(textOutput(ns("typicalSpectraCount"))),
        plotOutput(ns("screeningPlot")),
        uiOutput(ns("downloadScreeningPlotButton"))
    ),
    # Possibility to select and kept the outliers or/and to remove typical spectra from the spectra list
    box(title="Selection", width = 12, solidHeader = TRUE, status = "primary",
        fluidRow(
          column(5, selectInput(ns('atypicalSpectraSelect'),
                                label = tags$span('Atypical spectra: (automatically removed)',
                                                  tags$i(
                                                    class = "glyphicon glyphicon-info-sign",
                                                    style = "margin-left:10px;color:#0072B2;",
                                                    title = " Possibility to recover the excluded spectra"
                                                  )),

                                choices = list(),
                                size = 10, selectize = FALSE)),
          column(2, br(), br(), br(), br(), actionButton(ns("moveSpectraButton"), label='>>>')),
          column(5, selectInput(ns('selectedSpectraSelect'),
                                label = tags$span('Selected spectra:',
                                                  tags$i(
                                                    class = "glyphicon glyphicon-info-sign",
                                                    style = "margin-left:10px;color:#0072B2;",
                                                    title = " Possibility to exclude the selected typical spectra"
                                                  )),
                                choices = list(),
                                size = 10, selectize = FALSE))
        ),
        plotOutput(ns("selectedSpectrumPlot")),
        textOutput(ns("addedSpectra")),
        textOutput(ns("removedSpectra")),
        br(),
        # add the download button here
        downloadButton(ns("downloadPlot"), "Download Selected")

    ),



    ###

    # Spectra averaging
    box(title="Averaging", width = 12, solidHeader = TRUE, status = "primary",
        radioButtons(ns("averageMassSpectraMethod"),
                     label = tags$span("Averaging method:",
                                       tags$i(
                                         class = "glyphicon glyphicon-info-sign",
                                         style = "margin-left:10px;color:#0072B2;",
                                         title = "Averaging of MS spectra replicates: one representative spectra per sample of the replicates is the future analysis. Select one of the methods below"
                                       )),

                     inline = TRUE,
                     choices = list("Mean" = "mean", "Median" = "median" ,"Sum" = "sum"),
                     selected = "mean"),

        uiOutput(ns("mergeActionButtons")),
        br(), br(),
        tags$b(textOutput(ns("mergedSpectraCount"))),
        br(), br(),
        DT::dataTableOutput(ns("mergedSpectraMetadata"))
    )
  )
}

# Parameters

updatePreprocessingParameters <- function(session, input, values) {
  updateTrimParams(session, input, values$updateParameters$trimParams, values)
  updateTrim(values)
  updateConformityParams(session, input, values$updateParameters$conformityParams, values)
  updateConformity(values)
  updateCleanParams(session, input, values$updateParameters$cleanParams, values)
  updateClean(values)
  updateScreenParams(session, input, values$updateParameters$screenParams, values)
  updateScreen(values)
  updateMergeParams(session, input, values$updateParameters$mergeParams, values)
  updateMerge(values)
}

# TrimSpectra

updateTrim <- function(values) {
  enableOrDisableParams(c('spectraTrimmingRange'), values$trimParams)
  values$trimmedSpectra <- trimSpectra(values$spectra, values$trimParams)
  removeConformityParams(values)
}

extractTrimParams <- function(input, values) {
  values$trimParams <- list(
    range=input$spectraTrimmingRange
  )
  updateTrim(values)
}

removeTrimParams <- function(values) {
  values$trimParams <- FALSE
  updateTrim(values)
}

updateTrimParams <- function(session, input, params, values) {
  values$trimParams <- params
  if (isFALSE(params)) { return() }
  updateNumericRangeInput(session,
                          'spectraTrimmingRange',
                          value=params$range)
}

# ConformitySpectra

updateConformity <- function(values) {
  enableOrDisableParams(c('excludeEmptySpectra', 'excludeIrregularSpectra'), values$conformityParams)
  values$conformitySpectra <- conformitySpectra(values$trimmedSpectra, values$conformityParams)
  removeCleanParams(values)
}

extractConformityParams <- function(input, values) {
  values$conformityParams <- list(
    excludeEmptySpectra=input$excludeEmptySpectra,
    excludeIrregularSpectra=input$excludeIrregularSpectra
  )
  updateConformity(values)
}

removeConformityParams <- function(values) {
  values$conformityParams <- FALSE
  updateConformity(values)
}

updateConformityParams <- function(session, input, params, values) {
  values$conformityParams <- params
  if (isFALSE(params)) { return() }
  updateCheckboxInput(session,
                      'excludeEmptySpectra',
                      value=params$excludeEmptySpectra)
  updateCheckboxInput(session,
                      'excludeIrregularSpectra',
                      value=params$excludeIrregularSpectra)
}

# CleanSpectra

updateClean <- function(values) {
  enableOrDisableParams(c('transformIntensityMethod',
                          'smoothIntensityMethod',
                          'halfWindowSize',
                          'removeBaselineMethod',
                          'removeBaselineIterations',
                          'calibrateIntensityMethod'),
                        values$cleanParams)
  values$cleanedSpectra <- cleanSpectra(values$conformitySpectra, values$cleanParams)
  removeScreenParams(values)
}


extractCleanParams <- function(input, values) {
  values$cleanParams <- list(
    transformIntensityMethod=input$transformIntensityMethod,
    smoothIntensityMethod=input$smoothIntensityMethod,
    halfWindowSize=input$halfWindowSize,
    removeBaselineMethod=input$removeBaselineMethod,
    removeBaselineIterations=input$removeBaselineIterations,
    calibrateIntensityMethod=input$calibrateIntensityMethod
  )
  updateClean(values)
}

removeCleanParams <- function(values) {
  values$cleanParams <- FALSE
  updateClean(values)
}

updateCleanParams <- function(session, input, params, values) {
  values$cleanParams <- params
  if (isFALSE(params)) { return() }
  updateRadioButtons(session,'transformIntensityMethod',
                     selected=params$transformIntensityMethod)
  updateRadioButtons(session,'smoothIntensityMethod',
                     selected=params$smoothIntensityMethod)
  updateNumericInput(session,'halfWindowSize',
                     value=params$halfWindowSize)
  updateRadioButtons(session,'removeBaselineMethod',
                     selected=params$removeBaselineMethod)
  updateNumericInput(session,'removeBaselineIterations',
                     value=params$removeBaselineIterations)
  updateRadioButtons(session,'calibrateIntensityMethod',
                     selected=params$calibrateIntensityMethod)
}

# ScreenSpectra

updateScreen <- function(values) {
  enableOrDisableParams(c('scaleEstimator',
                          'atypicalIdentificationMethod',
                          'threshold',
                          'includeLowerSpectra'), values$screenParams)
  screenResults <- screenSpectra_(values$cleanedSpectra, values$screenParams)
  values$typicalSpectra <- screenResults$typicalSpectra
  values$atypicalSpectra <- screenResults$atypicalSpectra
  values$screeningPlot <- screenResults$screeningPlot
  values$screeningEstTable <- screenResults$screeningEstTable
  values$selectedSpectra <- values$typicalSpectra
  values$atypicalSpectraAfterSelection <- values$atypicalSpectra
  removeMergeParams(values)
}

extractScreenParams <- function(input, values) {
  values$screenParams <- list(
    scaleEstimator=input$scaleEstimator,
    atypicalIdentificationMethod=input$atypicalIdentificationMethod,
    threshold=input$threshold,
    includeLowerSpectra=input$includeLowerSpectra
  )
  updateScreen(values)
}

removeScreenParams <- function(values) {
  values$screenParams <- FALSE
  updateScreen(values)
}

updateScreenParams <- function(session, input, params, values) {
  values$screenParams <- params
  if (isFALSE(params)) { return() }
  updateRadioButtons(session,'scaleEstimator',
                     selected=params$scaleEstimator)
  updateRadioButtons(session,'atypicalIdentificationMethod',
                     selected=params$atypicalIdentificationMethod)
  updateNumericInput(session,'threshold',
                     value=params$threshold)
  updateCheckboxInput(session,'includeLowerSpectra',
                      value=params$includeLowerSpectra)
}

# MergeSpectra

updateMerge <- function(values) {
  enableOrDisableParams(c('averageMassSpectraMethod'), values$mergeParams)
  values$mergedSpectra <- mergeSpectra(values$selectedSpectra,
                                       values$mergeParams)
  values$clearProcessing <- TRUE
}

extractMergeParams <- function(input, values) {
  values$mergeParams <- list(
    averageMassSpectraMethod=input$averageMassSpectraMethod
  )
  updateMerge(values)
}

removeMergeParams <- function(values) {
  values$mergeParams <- FALSE
  updateMerge(values)
}

updateMergeParams <- function(session, input, params, values) {
  values$mergeParams <- params
  if (isFALSE(params)) { return() }
  updateRadioButtons(session,'averageMassSpectraMethod',
                     selected=params$averageMassSpectraMethod)
}

# Server

preprocessingServer <- function(id, values) {
  initializePreprocessingParams(values)

  moduleServer(
    id,
    function(input, output, session) {

      # Parameters

      observeEvent(values$updatePreprocessing, {
        if (isFALSE(values$updatePreprocessing)) { return() }
        updatePreprocessingParameters(session, input, values)
        values$updatePreprocessing <- FALSE
        values$updateProcessing <- TRUE
      })

      observeEvent(values$spectra, {
        removeTrimParams(values)
      })

      # Trim

      observeEvent(input$resetTrimButton, {
        if (input$resetTrimButton==0) { return() }
        removeTrimParams(values)
      })

      observeEvent(input$trimButton, {
        if (input$trimButton==0) { return() }
        extractTrimParams(input, values)
      })

      output$trimActionButtons <- renderUI({
        ns <- session$ns
        if (is.null(values$spectra)) { return() }
        createActionButton(isFALSE(values$trimParams),
                           ns("trimButton"), "Trim",
                           ns("resetTrimButton"), "Reset")
      })

      output$massBoundsPlot <- renderPlot({ computeMassBoundsPlot(values$trimmedSpectra) })

      output$downloadMassBoundsPlotButton <- renderUI({
        ns <- session$ns
        if (is.null(values$trimmedSpectra)) { return() }
        downloadButton(ns('downloadMassBoundsPlot'), 'Download figure')
      })

      output$downloadMassBoundsPlot <- downloadHandler(
        filename = 'mass_bounds.svg',
        content = function(file) { writeMassBoundsPlot(file, values) }
      )

      # Conformity test

      output$conformityTests <- renderDataTable({
        if (is.null(values$trimmedSpectra)) { return() }
        results <- conformityTests(values$trimmedSpectra)
        datatable({results}, options = list(dom = 't'), selection = list(mode = "none")) %>% formatStyle(
          "Results",
          target = "row",
          backgroundColor = styleEqual(c(0, "No"), c("#33cc33", "#33cc33"))
        )
      })

      output$conformityTestActionButtons <- renderUI({
        ns <- session$ns
        if (is.null(values$trimmedSpectra)) { return() }
        createActionButton(isFALSE(values$conformityParams),
                           ns("conformityButton"), "Exclude spectra",
                           ns("resetConformityButton"), "Reset")
      })

      observeEvent(input$conformityButton, {
        if (input$conformityButton==0) { return() }
        extractConformityParams(input, values)
      })

      observeEvent(input$resetConformityButton, {
        if (input$resetConformityButton==0) { return() }
        removeConformityParams(values)
      })

      # Clean

      observeEvent(input$resetCleanButton, {
        if (input$resetCleanButton==0) { return() }
        removeCleanParams(values)
      })

      observeEvent(input$cleanButton, {
        if (input$cleanButton==0) { return() }
        extractCleanParams(input, values)
      })

      output$cleanActionButtons <- renderUI({
        ns <- session$ns
        if (is.null(values$trimmedSpectra)) { return() }
        createActionButton(isFALSE(values$cleanParams),
                           ns("cleanButton"), "Clean",
                           ns("resetCleanButton"), "Reset")
      })

      output$spectrumSelector <- renderUI({
        spectra <- values$cleanedSpectra
        if (is.null(spectra)) { return () }
        ns <- session$ns
        numericInput(ns("spectrumIndex"), "Spectrum index:", value = 1, min = 1, max = length(spectra), step = 1)
      })

      output$spectraSamplePlot <- renderPlot({
        index <- input$spectrumIndex
        spectra <- values$cleanedSpectra
        if (is.null(index) || is.null(spectra) || index <= 0 || index > length(spectra)) { return() }
        MALDIquant::plot(spectra[[index]])
      })

      output$downloadspectraSamplePlotButton <- renderUI({
        ns <- session$ns
        index <- input$spectrumIndex
        spectra <- values$cleanedSpectra
        if (is.null(index) || is.null(spectra) || index <= 0 || index > length(spectra)) { return() }
        downloadButton(ns('downloadSpectraSamplePlot'), 'Download figure')
      })

      output$downloadSpectraSamplePlot <- downloadHandler(
        filename = 'spectrum_after_cleaning.svg',
        content = function(file) {
          index <- input$spectrumIndex
          spectra <- values$cleanedSpectra
          svg(file=file, width = 8, height = 4)
          MALDIquant::plot(spectra[[index]])
          dev.off()
        }
      )

      # Screen

      observeEvent(input$resetScreenButton, {
        if (input$resetScreenButton==0) { return() }
        removeScreenParams(values)
      })

      observeEvent(input$screenButton, {
        if (input$screenButton==0) { return() }
        extractScreenParams(input, values)
      })

      output$screenActionButtons <- renderUI({
        ns <- session$ns
        if (is.null(values$cleanedSpectra)) { return() }
        createActionButton(isFALSE(values$screenParams),
                           ns("screenButton"), "Screen",
                           ns("resetScreenButton"), "Reset")
      })

      output$typicalSpectraCount <- renderText({
        typicalSpectra <- values$typicalSpectra
        cleanedSpectra <- values$cleanedSpectra
        if (is.null(cleanedSpectra) || is.null(typicalSpectra)) {
          return ('Number of typical spectra: no spectra loaded.')
        }
        return(paste('Number of typical spectra: ',
                     length(typicalSpectra), '/',
                     length(cleanedSpectra),
                     sep=''))
      })

      output$screeningPlot <- renderPlot(values$screeningPlot)

      output$downloadScreeningPlotButton <- renderUI({
        ns <- session$ns
        if (is.null(values$screeningPlot)) { return() }
        downloadButton(ns('downloadScreeningPlot'), 'Download figure')
      })

      output$downloadScreeningPlot <- downloadHandler(
        filename = 'screening.svg',
        content = function(file) { writeScreeningPlot(file, values) }
      )

      # Select # to beeeeeee changed
      output$downloadPlot <- downloadHandler(
        filename = "selected_plot.svg",
        content = function(file) {
          writeSelectedPlot(file, values)
        }
      )
      # Select
      observeEvent(c(values$atypicalSpectraAfterSelection, values$selectedSpectra), {
        updateSelectInput(session,
                          'atypicalSpectraSelect',
                          choices = computeListForSelection(
                            values$atypicalSpectraAfterSelection,
                            values$screeningEstTable,
                            FALSE))
        updateSelectInput(session,
                          'selectedSpectraSelect',
                          choices = computeListForSelection(
                            values$selectedSpectra,
                            values$screeningEstTable,
                            TRUE))
        disable('moveSpectraButton')
      })

      observeEvent(input$atypicalSpectraSelect, {
        if (is.null(input$atypicalSpectraSelect)) { return() }
        values$selectedSpectrum <- input$atypicalSpectraSelect
        values$selectedSpectrumState <- 'atypical'
        updateActionButton(session, 'moveSpectraButton', label='>>>')
        enable('moveSpectraButton')
      })

      observeEvent(input$selectedSpectraSelect, {
        if (is.null(input$selectedSpectraSelect)) { return() }
        values$selectedSpectrum <- input$selectedSpectraSelect
        values$selectedSpectrumState <- 'selected'
        updateActionButton(session, 'moveSpectraButton', label='<<<')
        enable('moveSpectraButton')
      })

      output$selectedSpectrumPlot <- renderPlot({
        if (is.null(values$selectedSpectrum)) { return () }
        if (values$selectedSpectrumState == 'atypical') {
          MALDIquant::plot(values$atypicalSpectraAfterSelection[[values$selectedSpectrum]])
        } else {
          MALDIquant::plot(values$selectedSpectra[[values$selectedSpectrum]])
        }
      })

      observeEvent(input$moveSpectraButton, {
        if (is.null(values$selectedSpectra)) { return() }
        if (values$selectedSpectrumState == 'atypical') {
          values$selectedSpectra[[values$selectedSpectrum]] <- values$atypicalSpectraAfterSelection[[values$selectedSpectrum]]
          values$atypicalSpectraAfterSelection[[values$selectedSpectrum]] <- NULL
        } else {
          values$atypicalSpectraAfterSelection[[values$selectedSpectrum]] <- values$selectedSpectra[[values$selectedSpectrum]]
          values$selectedSpectra[[values$selectedSpectrum]] <- NULL
        }
        values$selectedSpectrum <- NULL
        values$selectedSpectrumState < NULL
        removeMergeParams(values)
      })

      output$addedSpectra <- renderText({
        ids <- computeAddedSpectraIds(values$atypicalSpectra, values$atypicalSpectraAfterSelection)
        str <- if (length(ids) == 0) "no spectra added" else paste(ids, collapse=', ')
        return(paste('Restore outlier spectra to continue analysis:', str))
      })

      output$removedSpectra <- renderText({
        ids <- computeRemovedSpectraIds(values$atypicalSpectra, values$atypicalSpectraAfterSelection)
        str <- if (length(ids) == 0) "no spectra removed" else paste(ids, collapse=', ')
        return(paste('Remove typical spectra from the analysis:', str))
      })

      # Merge

      observeEvent(input$resetMergeButton, {
        if (input$resetMergeButton==0) { return() }
        removeMergeParams(values)
      })

      observeEvent(input$mergeButton, {
        if (input$mergeButton==0) { return() }
        extractMergeParams(input, values)
      })

      output$mergeActionButtons <- renderUI({
        ns <- session$ns
        if (is.null(values$selectedSpectra)) { return() }
        createActionButton(isFALSE(values$mergeParams),
                           ns("mergeButton"), "Merge",
                           ns("resetMergeButton"), "Reset")
      })

      output$mergedSpectraCount <- renderText({
        mergedSpectra <- values$mergedSpectra
        return (
          if (is.null(mergedSpectra)) 'Number of spectra after averaging: no spectra loaded.'
          else paste('Number of spectra after merging: ', length(mergedSpectra), sep='')
        )
      })

      output$mergedSpectraMetadata <- DT::renderDataTable(extractSpectraMetadata(values$mergedSpectra)[,c('sampleName','replicateName','acquisitionDate')])
    })

}
