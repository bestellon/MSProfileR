extractAllProcessingParams <- function(params, values) {
  params$computePeaksBySNRParams <- values$computePeaksBySNRParams
  params$detectPeaksParams <- values$detectPeaksParams
  params$computeReferencePeaksParams <- values$computeReferencePeaksParams
  params$alignSpectraParams <- values$alignSpectraParams
  params$binPeaksParams <- values$binPeaksParams
  params$filterPeaksParams <- values$filterPeaksParams
  params$computeIntensityMatrixParams <- values$computeIntensityMatrixParams
  return(params)
}

initializeProcessingParams <- function(values) {
  values$computePeaksBySNRParams <- FALSE
  values$detectPeaksParams <- FALSE
  values$computeReferencePeaksParams <- FALSE
  values$alignSpectraParams <- FALSE
  values$binPeaksParams <- FALSE
  values$filterPeaksParams <- FALSE
  values$computeIntensityMatrixParams <- FALSE
}

executeProcessing <- function(values) {
  values$peakDetectionSpectra <- values$mergedSpectra
  values$peakCountsBySNR <- computePeakCountsBySNR(values$peakDetectionSpectra, values$computePeaksBySNRParams)
  values$peaks <- detectPeaks_(values$peakDetectionSpectra, values$detectPeaksParams)
  values$referencePeaks <- computeReferencePeaks(values$peaks, values$computeReferencePeaksParams)
  values$alignedSpectra <- alignSpectra_(values$peakDetectionSpectra,
                                         values$peaks,
                                         values$referencePeaks,
                                         values$alignSpectraParams)
  values$binnedPeaks <- binPeaks_(values$peaks, values$binPeaksParams)
  values$filteredPeaks <- filterPeaks_(values$binnedPeaks, values$filterPeaksParams)
  values$intensityMatrix <- computeIntensityMatrix(values$filteredPeaks,
                                                   values$alignedSpectra,
                                                   values$computeIntensityMatrixParams)
  return(values)
}

computePeakCountsBySNR <- function(spectra, params) {
  if (is.null(spectra) | isFALSE(params)) { return(NULL) }
  results <- NULL
  results <- data.frame(row.names = names(spectra))
  for(SNR in 2:7) {
    peaks <- detectPeaks(spectra,
                         method = params$noiseEstimatorMethod,
                         halfWindowSize = params$peakDetectionHalfWindowSize,
                         SNR = SNR)
    peakCounts <- lapply(X = peaks, function(spectrum) { length(spectrum@mass) })
    peakCounts <- t(as.data.frame(peakCounts))
    results[paste('SNR', SNR, sep=' ')] = peakCounts
  }
  return(results)
}


detectPeaks_ <- function(spectra, params) {
  if (is.null(spectra) | isFALSE(params)) { return(NULL) }
  return(detectPeaks(spectra,
                     method=params$noiseEstimatorMethod,
                     halfWindowSize = params$peakDetectionHalfWindowSize,
                     SNR = params$SNR))
}


plotSpectrumWithPeaks <- function(spectrum, peaks) {
  if (is.null(spectrum) || is.null(peaks)) { return() }
  MALDIquant::plot(spectrum)
  index <- sort(intensity(peaks), decreasing=TRUE, index.return=TRUE)$ix
  labelPeaks(peaks, index=index, labels=1:length(peaks@mass), underline=FALSE)
}

computeReferencePeaks <- function(peaks,
                                  params) {
  if (is.null(peaks) | isFALSE(params)) { return(NULL) }
  return(referencePeaks(peaks,
                        method=params$referencePeaksMethod,
                        minFrequency = params$minFrequency,
                        tolerance = params$referencePeaksTolerance))
}

alignSpectra_ <- function(spectra,
                          peaks,
                          reference,
                          params) {
  if (is.null(peaks) | is.null(reference) | isFALSE(params)) return (spectra)
  warpingFunctions <- determineWarpingFunctions(peaks,
                                                reference = reference,
                                                tolerance = params$referencePeaksTolerance,
                                                method = params$alignmentWarpingMethod,
                                                allowNoMatches = TRUE)
  return(warpMassSpectra(spectra, warpingFunctions, emptyNoMatches = TRUE))
}

computeSpectraPheatmap <- function(spectra, width = 500,
                                   cluster_rows = FALSE) {
  if (is.null(spectra)) { return() }
  pixels <- matrix(255, ncol=width, nrow=length(spectra))
  minMass <- min(unlist(lapply(spectra, function(spectrum) { min(spectrum@mass) })))
  maxMass <- max(unlist(lapply(spectra, function(spectrum) { max(spectrum@mass) })))
  maxIntensity <- max(unlist(lapply(spectra, function(spectrum) { max(spectrum@intensity) })))
  index <- 1
  for (spectrum in spectra) {
    x <- index
    for (i in 0:length(mass(spectrum))) {
      mass <- spectrum@mass[i]
      intensity <- spectrum@intensity[i]
      y <- floor((width * (mass - minMass) / (maxMass - minMass)))
      pixels[x, y] <- floor(255 * (1 - sqrt(intensity / maxIntensity)))
    }
    index <- index + 1
  }
  colnames(pixels) <- sapply(1:width, function(i) {
    if (i%%30!=1) ""
    else paste(round(i * ((maxMass - minMass) / width) + minMass, digits=1), "m/z")
  })
  return(pheatmap(pixels,
                  color = grey(seq(0, 1, length = 256)),
                  cluster_rows=cluster_rows,
                  cluster_cols=FALSE,
                  legend = FALSE))
}

binPeaks_ <- function(peaks, params) {
  if (is.null(peaks) | isFALSE(params)) return(peaks)
  return(binPeaks(peaks,
                  method=params$binPeaksMethod,
                  tolerance=params$binPeaksTolerance))
}

filterPeaks_ <- function(peaks, params) {
  if (is.null(peaks) | isFALSE(params)) { return(peaks) }
  return(filterPeaks(peaks, minFrequency=params$filterPeaksMinFrequency))
}

computeIntensityMatrix <- function(peaks, spectra, params) {
  if (is.null(peaks) | is.null(spectra) | isFALSE(params)) { return(NULL) }
  return (
    if (params$fillMissingValues) { intensityMatrix(peaks, spectra) }
    else { intensityMatrix(peaks) }
  )
}

computeIntensityMatrixPheatmap <- function(intensityMatrix_, spectra, showRownames) {
  if (is.null(intensityMatrix_) || is.null(spectra)) { return () }
  savedColnames <- colnames(intensityMatrix_)
  savedRownames <- rownames(intensityMatrix_)
  colnames(intensityMatrix_) <- paste(round(as.double(colnames(intensityMatrix_)), 1), 'm/z')
  rownames(intensityMatrix_) <- extractSpectraMetadata(spectra)$sampleName
  pheatmap_ <- pheatmap(intensityMatrix_,
                        cluster_rows = TRUE,
                        cluster_cols= FALSE,
                        show_colnames = FALSE,
                        show_rownames = showRownames)
  colnames(intensityMatrix_) <- savedColnames
  rownames(intensityMatrix_) <- savedRownames
  return(pheatmap_)
}

computePeakCount <- function(peaks) {
  differentMass <- c()
  for (massPeaks in peaks) {
    differentMass <- c(differentMass, mass(massPeaks))
    differentMass <- differentMass[!duplicated(differentMass)]
  }
  return(length(differentMass))
}
