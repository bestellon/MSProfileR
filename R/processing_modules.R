processingUI <- function(id) {
  ns <- NS(id)
  tagList(
    # Peak detection
    box(title="Peak detection", width = 12, solidHeader = TRUE, status = "primary",
        radioButtons(ns("noiseEstimatorMethod"),
                     label = tags$span("Noise estimator method:",
                                       tags$i(
                                         class = "glyphicon glyphicon-info-sign",
                                         style = "margin-left:10px;color:#0072B2;",
                                         title = "A local maximum is detected as a peak whether its intensity is greater than the noise level"
                                       )),

                     inline = TRUE,
                     choices = list("MAD" = "MAD", "Super smoother" = "SuperSmoother"),
                     selected = "MAD"),
        numericInput(ns("peakDetectionHalfWindowSize"), "Half window size:", 20, min = 1, max = 100),
        uiOutput(ns("computePeaksBySNRActionButtons")),
        plotOutput(ns("peakCountsBySNRPlot")),
        uiOutput(ns("downloadPeakCountsBySNRPlotButton")),
        br(),
        sliderTextInput(
          inputId = ns("SNR"),
          label = "Choose SNR Value:",
          choices = seq(from = 2, to = 7, by = 1),
          grid = TRUE),
        textOutput(ns("SNRStatistics")),
        br(), br(),
        uiOutput(ns("detectPeaksActionButtons")),
        br(), br(),
        uiOutput(ns("spectrumWithPeaksSelector")),
        textOutput(ns("detectPeaksCount")),
        plotOutput(ns("spectrumWithPeaksPlot")),
        uiOutput(ns("downloadSpectrumWithPeaksPlotButton"))
    ),
    # Spectrum alignment
    box(title="Spectrum alignment", width = 12, solidHeader = TRUE, status = "primary",
        radioButtons(ns("referencePeaksMethod"),
                     label = tags$span("Method for reference peaks:",
                                       tags$i(
                                         class = "glyphicon glyphicon-info-sign",
                                         style = "margin-left:10px;color:#0072B2;",
                                         title = "Creation of reference MassPeaks objec from a list of MassPeaks object & align MassPeaks object with the selected references"
                                       )),
                     inline = TRUE,
                     choices = list("Strict" = "strict", "Relaxed" = "relaxed"),
                     selected = "strict"),
        numericInput(ns("minFrequency"), "Minimum frenquency of reference peaks:", 0.9, min = 0.1, max = 1.0, step=0.1),
        numericInput(ns("referencePeaksTolerance"), "Tolerance for reference peaks:", 0.002, min = 0.0001, max = 0.5, step=0.0001),
        uiOutput(ns("computeReferencePeaksActionButtons")),
        br(),
        textOutput(ns("referencePeaksCount")),
        plotOutput(ns("referencePeaksPlot")),
        uiOutput(ns("downloadReferencePeaksPlotButton")),
        br(),
        radioButtons(ns("alignmentWarpingMethod"), label = "Warping method for alignment:", inline = TRUE,
                     choices = list("Lowess" = "lowess", "Linear" = "linear", "Quadratic" = "quadratic", "Cubic" = "cubic"),
                     selected = "lowess"),
        uiOutput(ns("alignSpectraActionButtons")),
        plotOutput(ns("alignedSpectraPlot")),
        uiOutput(ns("downloadAlignedSpectraPlotButton"))
    ),
    # Binning peaks
    box(title="Peak binning", width = 12, solidHeader = TRUE, status = "primary",
        radioButtons(ns("binPeaksMethod"),
                     label = tags$span("Method for binning:",
                                       tags$i(
                                         class = "glyphicon glyphicon-info-sign",
                                         style = "margin-left:10px;color:#0072B2;",
                                         title = "Looking for similar peaks across MassPeaks objects and equalizes their mass"
                                       )),
                     inline = TRUE,
                     choices = list("strict" = "strict", "relaxed" = "relaxed"),
                     selected = "strict"),
        numericInput(ns("binPeaksTolerance"), "Tolerance for binning:", 0.002, min = 0.0001, max = 0.5, step=0.0001),
        uiOutput(ns("binPeaksActionButtons")),
        br(),
        textOutput(ns("binnedPeaksCount")),
        plotOutput(ns("binnedPeaksPlot")),
        uiOutput(ns("downloadBinnedPeaksPlotButton"))
    ),
    # Filtering peaks
    box(title="Peak filtering", width = 12, solidHeader = TRUE, status = "primary",
        numericInput(ns("filterPeaksMinFrequency"),
                     label = tags$span("Minimum frenquency:",
                                       tags$i(
                                         class = "glyphicon glyphicon-info-sign",
                                         style = "margin-left:10px;color:#0072B2;",
                                         title =  "Removing of infrequently occuring peaks in a list of MassPeaks objects"
                                       )),


                     0.2, min = 0.1, max = 1.0, step=0.1),
        uiOutput(ns("filterPeaksActionButtons")),
        br(),
        textOutput(ns("filteredPeaksCount")),
        plotOutput(ns("filteredPeaksPlot")),
        uiOutput(ns("downloadFilteredPeaksPlotButton"))
    ),
    # Classification of specimen by hierarchical clustering
    box(title="Clustering", width = 12, solidHeader = TRUE, status = "primary",
        checkboxInput(ns("fillMissingValues"),

                      label = tags$span("Fill missing intensities using the aligned spectra",
                                        tags$i(
                                          class = "glyphicon glyphicon-info-sign",
                                          style = "margin-left:10px;color:#0072B2;",
                                          title =  "Converting a list of MassPeaks objects into a graphical matrix"
                                        )), value=TRUE),
        uiOutput(ns("computeIntensityMatrixActionButtons")),
        plotOutput(ns("intensityMatrixPlot")),
        uiOutput(ns("downloadIntensityMatrixPlotButtons"))
    )
  )
}

# Parameters

updateProcessingParameters <- function(session, input, values) {
  updatePeakDetectionSpectra(values)
  updateComputePeaksBySNRParams(session, input, values$updateParameters$computePeaksBySNRParams, values)
  updateComputePeaksBySNR(values)
  updateDetectPeaksParams(session, input, values$updateParameters$detectPeaksParams, values)
  updateDetectPeaks(values)
  updateComputeReferencePeaksParams(session, input, values$updateParameters$computeReferencePeaksParams, values)
  updateComputeReferencePeaks(values)
  updateAlignSpectraParams(session, input, values$updateParameters$alignSpectraParams, values)
  updateAlignSpectra(values)
  updateBinPeaksParams(session, input, values$updateParameters$binPeaksParams, values)
  updateBinPeaks(values)
  updateFilterPeaksParams(session, input, values$updateParameters$filterPeaksParams, values)
  updateFilterPeaks(values)
  updateComputeIntensityMatrixParams(session, input, values$updateParameters$computeIntensityMatrixParams, values)
  updateComputeIntensityMatrix(values)
}

runProcessing <- function(values) {
  updatePeakDetectionSpectra(values)
  updateComputePeaksBySNR(values)
  updateDetectPeaks(values)
  updateComputeReferencePeaks(values)
  updateAlignSpectra(values)
  updateBinPeaks(values)
  updateFilterPeaks(values)
  updateComputeIntensityMatrix(values)
}

# PeakDetectionSpectra

updatePeakDetectionSpectra <- function(values) {
  values$peakDetectionSpectra <- values$mergedSpectra
  removeComputePeaksBySNRParams(values)
  removeDetectPeaksParams(values)
}

# ComputePeaksBySNR

updateComputePeaksBySNR <- function(values) {
  enableOrDisableParams(c('noiseEstimatorMethod',
                          'peakDetectionHalfWindowSize'), values$computePeaksBySNRParams)
  values$peakCountsBySNR <- computePeakCountsBySNR(values$peakDetectionSpectra, values$computePeaksBySNRParams)
}

extractComputePeaksBySNRParams <- function(input, values) {
  values$computePeaksBySNRParams <- list(
    noiseEstimatorMethod=input$noiseEstimatorMethod,
    peakDetectionHalfWindowSize=input$peakDetectionHalfWindowSize
  )
  updateComputePeaksBySNR(values)
}

removeComputePeaksBySNRParams <- function(values) {
  values$computePeaksBySNRParams <- FALSE
  updateComputePeaksBySNR(values)
}

updateComputePeaksBySNRParams <- function(session, input, params, values) {
  values$computePeaksBySNRParams <- params
  if (isFALSE(params)) { return() }
  updateRadioButtons(session,'noiseEstimatorMethod',
                     selected=params$noiseEstimatorMethod)
  updateNumericInput(session,'peakDetectionhalfWindowSize',
                     value=params$peakDetectionhalfWindowSize)
}

# DetectPeaks

updateDetectPeaks <- function(values) {
  enableOrDisableParams(c('SNR'), values$detectPeaksParams)
  enableOrDisableParams(c('noiseEstimatorMethod', 'peakDetectionHalfWindowSize'),
                        !(isFALSE(values$detectPeaksParams) && isFALSE(values$computePeaksBySNRParams)))
  values$peaks <- detectPeaks_(values$peakDetectionSpectra, values$detectPeaksParams)
  removeComputeReferencePeaksParams(values)
  removeBinPeaksParams(values)
}

extractDetectPeaksParams <- function(input, values) {
  values$detectPeaksParams <- list(
    noiseEstimatorMethod=input$noiseEstimatorMethod,
    peakDetectionHalfWindowSize=input$peakDetectionHalfWindowSize,
    SNR=input$SNR
  )
  updateDetectPeaks(values)
}

removeDetectPeaksParams <- function(values) {
  values$detectPeaksParams <- FALSE
  updateDetectPeaks(values)
}

updateDetectPeaksParams <- function(session, input, params, values) {
  values$detectPeaksParams <- params
  if (isFALSE(params)) { return() }
  updateRadioButtons(session,'noiseEstimatorMethod',
                     selected=params$noiseEstimatorMethod)
  updateNumericInput(session,'peakDetectionhalfWindowSize',
                     value=params$peakDetectionhalfWindowSize)
  updateSliderTextInput(session, 'SNR', selected=params$SNR)
}

# ComputeReferencePeaks

updateComputeReferencePeaks <- function(values) {
  enableOrDisableParams(c('referencePeaksMethod',
                          'minFrequency',
                          'referencePeaksTolerance'), values$computeReferencePeaksParams)
  values$referencePeaks <- computeReferencePeaks(values$peaks, values$computeReferencePeaksParams)
  removeAlignSpectraParams(values)
}

extractComputeReferencePeaksParams <- function(input, values) {
  values$computeReferencePeaksParams <- list(
    referencePeaksMethod=input$referencePeaksMethod,
    minFrequency=input$minFrequency,
    referencePeaksTolerance=input$referencePeaksTolerance
  )
  updateComputeReferencePeaks(values)
}

removeComputeReferencePeaksParams <- function(values) {
  values$computeReferencePeaksParams <- FALSE
  updateComputeReferencePeaks(values)
}

updateComputeReferencePeaksParams <- function(session, input, params, values) {
  values$computeReferencePeaksParams <- params
  if (isFALSE(params)) { return() }
  updateRadioButtons(session,'referencePeaksMethod',
                     selected=params$referencePeaksMethod)
  updateNumericInput(session,'minFrequency',
                     value=params$minFrequency)
  updateNumericInput(session,'referencePeaksTolerance',
                     value=params$referencePeaksTolerance)
}

# AlignSpectra

updateAlignSpectra <- function(values) {
  enableOrDisableParams(c('alignmentWarpingMethod'), values$alignSpectraParams)
  values$alignedSpectra <- alignSpectra_(values$peakDetectionSpectra,
                                         values$peaks,
                                         values$referencePeaks,
                                         values$alignSpectraParams)
  removeComputeIntensityMatrixParams(values)
}

extractAlignSpectraParams <- function(input, values) {
  values$alignSpectraParams <- list(
    referencePeaksTolerance=input$referencePeaksTolerance,
    alignmentWarpingMethod=input$alignmentWarpingMethod
  )
  updateAlignSpectra(values)
}

removeAlignSpectraParams <- function(values) {
  values$alignSpectraParams <- FALSE
  updateAlignSpectra(values)
}

updateAlignSpectraParams <- function(session, input, params, values) {
  values$alignSpectraParams <- params
  if (isFALSE(params)) { return() }
  updateRadioButtons(session,'alignmentWarpingMethod',
                     selected=params$alignmentWarpingMethod)
}

# BinPeaks

updateBinPeaks <- function(values) {
  enableOrDisableParams(c('binPeaksMethod', 'binPeaksTolerance'), values$binPeaksParams)
  values$binnedPeaks <- binPeaks_(values$peaks, values$binPeaksParams)
  removeFilterPeaksParams(values)
}

extractBinPeaksParams <- function(input, values) {
  values$binPeaksParams <- list(
    binPeaksMethod=input$binPeaksMethod,
    binPeaksTolerance=input$binPeaksTolerance
  )
  updateBinPeaks(values)
}

removeBinPeaksParams <- function(values) {
  values$binPeaksParams <- FALSE
  updateBinPeaks(values)
}

updateBinPeaksParams <- function(session, input, params, values) {
  values$binPeaksParams <- params
  if (isFALSE(params)) { return() }
  updateRadioButtons(session,'binPeaksMethod',
                     selected=params$binPeaksMethod)
  updateNumericInput(session,'binPeaksTolerance',
                     value=params$binPeaksTolerance)
}

# FilterPeaks

updateFilterPeaks <- function(values) {
  enableOrDisableParams(c('filterPeaksMinFrequency'), values$filterPeaksParams)
  values$filteredPeaks <- filterPeaks_(values$binnedPeaks, values$filterPeaksParams)
  removeComputeIntensityMatrixParams(values)
}

extractFilterPeaksParams <- function(input, values) {
  values$filterPeaksParams <- list(
    filterPeaksMinFrequency=input$filterPeaksMinFrequency
  )
  updateFilterPeaks(values)
}

removeFilterPeaksParams <- function(values) {
  values$filterPeaksParams <- FALSE
  updateFilterPeaks(values)
}

updateFilterPeaksParams <- function(session, input, params, values) {
  values$filterPeaksParams <- params
  if (isFALSE(params)) { return() }
  updateNumericInput(session,'filterPeaksMinFrequency',
                     value=params$filterPeaksMinFrequency)
}

# ComputeIntensityMatrix

updateComputeIntensityMatrix <- function(values) {
  enableOrDisableParams(c('fillMissingValues'), values$computeIntensityMatrixParams)
  values$intensityMatrix <- computeIntensityMatrix(values$filteredPeaks,
                                                   values$alignedSpectra,
                                                   values$computeIntensityMatrixParams)
}

extractComputeIntensityMatrixParams <- function(input, values) {
  values$computeIntensityMatrixParams <- list(
    fillMissingValues=input$fillMissingValues
  )
  updateComputeIntensityMatrix(values)
}

removeComputeIntensityMatrixParams <- function(values) {
  values$computeIntensityMatrixParams <- FALSE
  updateComputeIntensityMatrix(values)
}

updateComputeIntensityMatrixParams <- function(session, input, params, values) {
  values$computeIntensityMatrixParams <- params
  if (isFALSE(params)) { return() }
  updateCheckboxInput(session,'fillMissingValues',
                      value=params$fillMissingValues)
}

processingServer <- function(id, values) {
  initializeProcessingParams(values)

  moduleServer(
    id,
    function(input, output, session) {

      # Parameters

      observeEvent(values$updateProcessing, {
        if (isFALSE(values$updateProcessing)) { return() }
        updateProcessingParameters(session, input, values)
        values$updateProcessing <- FALSE
      })

      observeEvent(values$clearProcessing, {
        if (isFALSE(values$clearProcessing)) { return() }
        updatePeakDetectionSpectra(values)
        values$clearProcessing <- FALSE
      })

      # ComputePeaksBySNR

      observeEvent(input$resetComputePeaksBySNRButton, {
        if (input$resetComputePeaksBySNRButton==0) { return() }
        removeComputePeaksBySNRParams(values)
      })

      observeEvent(input$computePeaksBySNRButton, {
        if (input$computePeaksBySNRButton==0) { return() }
        extractComputePeaksBySNRParams(input, values)
      })

      output$computePeaksBySNRActionButtons <- renderUI({
        ns <- session$ns
        if (is.null(values$peakDetectionSpectra)) { return() }
        createActionButton(isFALSE(values$computePeaksBySNRParams),
                           ns("computePeaksBySNRButton"), "Compute number of peaks by Signal-to-Noise-Ratio (SNR)",
                           ns("resetComputePeaksBySNRButton"), "Reset")
      })

      output$peakCountsBySNRPlot <- renderPlot({
        if (!is.null(values$peakCountsBySNR)) {
          boxplot(values$peakCountsBySNR, xlab = "Signal-to-Noise Ratio (SNR)", ylab = "Number of Peaks")
        }
      })

      output$SNRStatistics <- renderText({
        if (is.null(values$peakCountsBySNR)) { return ("") }
        peakCounts <- values$peakCountsBySNR[[paste('SNR', input$SNR, sep=' ')]]
        peakCountMean <- mean(peakCounts)
        peakCountSd <- sd(peakCounts)
        return (paste('Average number of peaks for SNR ', input$SNR, ': ', round(peakCountMean, 2), ' (sd: ', round(peakCountSd, 2), ')', sep=''))
      })

      output$downloadPeakCountsBySNRPlotButton <- renderUI({
        ns <- session$ns
        if (is.null(values$peakCountsBySNR)) { return() }
        downloadButton(ns('downloadPeakCountsBySNRPlot'), 'Download figure')
      })

      output$downloadPeakCountsBySNRPlot <- downloadHandler(
        filename = 'peak_counts_by_SNR.svg',
        content = function(file) { writePeaksCountsBySNRPlot(file, values) }
      )

      # DetectPeaks

      observeEvent(input$resetDetectPeaksButton, {
        if (input$resetDetectPeaksButton==0) { return() }
        removeDetectPeaksParams(values)
      })

      observeEvent(input$detectPeaksButton, {
        if (input$detectPeaksButton==0) { return() }
        extractDetectPeaksParams(input, values)
      })

      output$detectPeaksActionButtons <- renderUI({
        ns <- session$ns
        if (is.null(values$peakDetectionSpectra)) { return() }
        createActionButton(isFALSE(values$detectPeaksParams),
                           ns("detectPeaksButton"), "Detect peaks",
                           ns("resetDetectPeaksButton"), "Reset")
      })

      output$spectrumWithPeaksSelector <- renderUI({
        spectra <- values$peakDetectionSpectra
        if (is.null(spectra)) { return () }
        ns <- session$ns
        numericInput(ns("spectrumWithPeaksIndex"), "Spectrum index:",
                     value = 1, min = 1, max = length(spectra), step = 1)
      })

      output$detectPeaksCount <- renderText({
        index <- input$spectrumWithPeaksIndex
        spectra <- values$peakDetectionSpectra
        peaks <- values$peaks
        if (is.null(index) || is.null(spectra) || is.null(peaks)
            || index <= 0 || index > length(spectra)) { return("") }
        return (paste("Number of detected peaks: ", length(peaks[[index]]@mass), sep = ""))
      })

      output$spectrumWithPeaksPlot <- renderPlot({
        index <- input$spectrumWithPeaksIndex
        spectra <- values$peakDetectionSpectra
        peaks <- values$peaks
        if (is.null(index) || is.null(spectra) || is.null(peaks)
            || index <= 0 || index > length(spectra)) { return() }
        plotSpectrumWithPeaks(spectra[[index]], peaks[[index]])
      })

      output$downloadSpectrumWithPeaksPlotButton <- renderUI({
        ns <- session$ns
        index <- input$spectrumWithPeaksIndex
        spectra <- values$peakDetectionSpectra
        peaks <- values$peaks
        if (is.null(index) || is.null(spectra) || is.null(peaks)
            || index <= 0 || index > length(spectra)) { return() }
        downloadButton(ns('downloadSpectrumWithPeaksPlot'), 'Download figure')
      })

      output$downloadSpectrumWithPeaksPlot <- downloadHandler(
        filename = 'spectrum_with_peaks.svg',
        content = function(file) {
          svg(file=file, width = 8, height = 4)
          index <- input$spectrumWithPeaksIndex
          spectra <- values$peakDetectionSpectra
          peaks <- values$peaks
          plotSpectrumWithPeaks(spectra[[index]], peaks[[index]])
          dev.off()
        }
      )

      # ComputeReferencePeaks

      observeEvent(input$resetComputeReferencePeaksButton, {
        if (input$resetComputeReferencePeaksButton==0) { return() }
        removeComputeReferencePeaksParams(values)
      })

      observeEvent(input$computeReferencePeaksButton, {
        if (input$computeReferencePeaksButton==0) { return() }
        extractComputeReferencePeaksParams(input, values)
      })

      output$computeReferencePeaksActionButtons <- renderUI({
        ns <- session$ns
        if (is.null(values$peaks)) { return() }
        createActionButton(isFALSE(values$computeReferencePeaksParams),
                           ns("computeReferencePeaksButton"), "Compute reference peaks",
                           ns("resetComputeReferencePeaksButton"), "Reset")
      })

      output$referencePeaksCount <- renderText({
        if (is.null(values$referencePeaks)) { return ('') }
        paste('Number of reference peaks: ', length(values$referencePeaks))
      })

      output$referencePeaksPlot <- renderPlot({
        if (!is.null(values$referencePeaks)) { MALDIquant::plot(values$referencePeaks) }
      })

      output$downloadReferencePeaksPlotButton <- renderUI({
        ns <- session$ns
        if (is.null(values$referencePeaks)) { return () }
        downloadButton(ns('downloadReferencePeaksPlot'), 'Download figure')
      })

      output$downloadReferencePeaksPlot <- downloadHandler(
        filename = 'reference_peaks.svg',
        content = function(file) { writeReferencePeaksPlot(file, values) }
      )

      # AlignSpectra

      observeEvent(input$resetAlignSpectraButton, {
        if (input$resetAlignSpectraButton==0) { return() }
        removeAlignSpectraParams(values)
      })

      observeEvent(input$alignSpectraButton, {
        if (input$alignSpectraButton==0) { return() }
        extractAlignSpectraParams(input, values)
      })

      output$alignSpectraActionButtons <- renderUI({
        ns <- session$ns
        if (is.null(values$referencePeaks) || is.null(values$peaks) || is.null(values$peakDetectionSpectra)) { return() }
        if (length(values$referencePeaks)==0) { return() }
        createActionButton(isFALSE(values$alignSpectraParams),
                           ns("alignSpectraButton"), "Align Spectra",
                           ns("resetAlignSpectraButton"), "Reset")
      })

      output$alignedSpectraPlot <- renderPlot({ computeSpectraPheatmap(values$alignedSpectra) })

      output$downloadAlignedSpectraPlotButton <- renderUI({
        ns <- session$ns
        if (is.null(values$alignedSpectra)) { return () }
        downloadButton(ns('downloadAlignedSpectraPlot'), 'Download figure')
      })

      output$downloadAlignedSpectraPlot <- downloadHandler(
        filename = 'spectra_aligned.svg',
        content = function(file) { writeAlignedSpectraPlot(file, values) }
      )

      # BinPeaks

      observeEvent(input$resetBinPeaksButton, {
        if (input$resetBinPeaksButton==0) { return() }
        removeBinPeaksParams(values)
      })

      observeEvent(input$binPeaksButton, {
        if (input$binPeaksButton==0) { return() }
        extractBinPeaksParams(input, values)
      })

      output$binPeaksActionButtons <- renderUI({
        ns <- session$ns
        if (is.null(values$peaks)) { return() }
        createActionButton(isFALSE(values$binPeaksParams),
                           ns("binPeaksButton"), "Bin peaks",
                           ns("resetBinPeaksButton"), "Reset")
      })

      output$binnedPeaksCount <- renderText({
        paste('Number of peaks (number of m/z values): ', computePeakCount(values$binnedPeaks))
      })

      output$binnedPeaksPlot <- renderPlot({ computeSpectraPheatmap(values$binnedPeaks) })

      output$downloadBinnedPeaksPlotButton <- renderUI({
        ns <- session$ns
        if (is.null(values$binnedPeaks)) { return () }
        downloadButton(ns('downloadBinnedPeaksPlot'), 'Download figure')
      })

      output$downloadBinnedPeaksPlot <- downloadHandler(
        filename = 'peaks_binned.svg',
        content = function(file) { writeBinnedPeaksPlot(file, values) }
      )

      # FilterPeaks

      observeEvent(input$resetFilterPeaksButton, {
        if (input$resetFilterPeaksButton==0) { return() }
        removeFilterPeaksParams(values)
      })

      observeEvent(input$filterPeaksButton, {
        if (input$filterPeaksButton==0) { return() }
        extractFilterPeaksParams(input, values)
      })

      output$filterPeaksActionButtons <- renderUI({
        ns <- session$ns
        if (is.null(values$binnedPeaks)) { return() }
        createActionButton(isFALSE(values$filterPeaksParams),
                           ns("filterPeaksButton"), "Filter peaks",
                           ns("resetFilterPeaksButton"), "Reset")
      })

      output$filteredPeaksCount <- renderText({
        paste('Number of peaks: ', computePeakCount(values$filteredPeaks))
      })

      output$filteredPeaksPlot <- renderPlot({ computeSpectraPheatmap(values$filteredPeaks) })

      output$downloadFilteredPeaksPlotButton <- renderUI({
        ns <- session$ns
        if (is.null(values$filteredPeaks)) { return () }
        downloadButton(ns('downloadFilteredPeaksPlot'), 'Download figure')
      })

      output$downloadFilteredPeaksPlot <- downloadHandler(
        filename = 'peaks_filtered.svg',
        content = function(file) { writeFilteredPeaksPlot(file, values) }
      )

      # computeIntensityMatrix

      observeEvent(input$resetComputeIntensityMatrixButton, {
        if (input$resetComputeIntensityMatrixButton==0) { return() }
        removeComputeIntensityMatrixParams(values)
      })

      observeEvent(input$computeIntensityMatrixButton, {
        if (input$computeIntensityMatrixButton==0) { return() }
        extractComputeIntensityMatrixParams(input, values)
      })

      output$computeIntensityMatrixActionButtons <- renderUI({
        ns <- session$ns
        if (is.null(values$filteredPeaks) | is.null(values$alignedSpectra)) { return() }
        createActionButton(isFALSE(values$computeIntensityMatrixParams),
                           ns("computeIntensityMatrixButton"), "Compute intensity matrix",
                           ns("resetComputeIntensityMatrixButton"), "Reset")
      })

      output$intensityMatrixPlot <- renderPlot({
        computeIntensityMatrixPheatmap(values$intensityMatrix, values$alignedSpectra, TRUE)
      })

      output$downloadIntensityMatrixPlotButtons <- renderUI({
        ns <- session$ns
        if (is.null(values$intensityMatrix)) { return () }
        tagList(
          downloadButton(ns('downloadIntensityMatrixPlot32'), 'Download matrix (32x16)'),
          downloadButton(ns('downloadIntensityMatrixPlot16'), 'Download matrix (16x8)'),
          downloadButton(ns('downloadIntensityMatrixPlot8'), 'Download matrix (8x4)'),
          downloadButton(ns('downloadDendrogram'), 'Download dendrogram')
        )
      })

      output$downloadIntensityMatrixPlot32 <- downloadHandler(
        filename = 'intensity_matrix_32x16.svg',
        content = function(file) { writeIntensityMatrixPlot(file, values, 32, 16, TRUE) }
      )

      output$downloadIntensityMatrixPlot16 <- downloadHandler(
        filename = 'intensity_matrix_16x8.svg',
        content = function(file) { writeIntensityMatrixPlot(file, values, 16, 8, TRUE) }
      )

      output$downloadIntensityMatrixPlot8 <- downloadHandler(
        filename = 'intensity_matrix_8x4.svg',
        content = function(file) { writeIntensityMatrixPlot(file, values, 8, 4, FALSE) }
      )

      output$downloadDendrogram <- downloadHandler(
        filename = 'dendrogram.svg',
        content = function(file) { writeDendrogramPlot(file, values) }
      )
    }
  )
}
