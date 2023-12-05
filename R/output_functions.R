extractParameters <- function(values) {
  params <- list()
  params <- extractAllPreprocessingParams(params, values)
  params <- extractAllProcessingParams(params, values)
  return(params)
}

computePreprocessingReportData <- function(params) {
  params$values$trimmedSpectraMassBoundsPlot <- computeMassBoundsPlot(params$values$trimmedSpectra)
  params$values$conformityTests <- conformityTests(params$values$trimmedSpectra)
  params$values$atypicalSpectraNames <- computeNameFromSpectraIds(params$values$spectra, names(params$values$atypicalSpectra))
  params$values$addedSpectraIds <- computeAddedSpectraIds(params$values$atypicalSpectra, params$values$atypicalSpectraAfterSelection)
  params$values$removedSpectraIds <- computeRemovedSpectraIds(params$values$atypicalSpectra, params$values$atypicalSpectraAfterSelection)
  params$values$addedSpectraNames <- computeNameFromSpectraIds(params$values$spectra, params$values$addedSpectraIds)
  params$values$removedSpectraNames <- computeNameFromSpectraIds(params$values$spectra, params$values$removedSpectraIds)
  return(params)
}

computeProcessingReportData <- function(params) {
  params$values$spectrumWithPeaksPlot <- plotSpectrumWithPeaks(params$values$peakDetectionSpectra[[1]], params$values$peaks[[1]])
  params$values$alignedSpectraPheatmap <- computeSpectraPheatmap(params$values$alignedSpectra)
  params$values$binnedPeaksPheatmap <- computeSpectraPheatmap(params$values$binnedPeaks)
  params$values$filteredPeakCount <- computePeakCount(params$values$filteredPeaks)
  params$values$binnedPeakCount <- computePeakCount(params$values$binnedPeaks)
  params$values$filteredPeaksPheatmap <- computeSpectraPheatmap(params$values$filteredPeaks)
  params$values$intensityMatrixPheatmap <- computeIntensityMatrixPheatmap(params$values$intensityMatrix, params$values$alignedSpectra, TRUE)
  return(params)
}

writeReport <- function(file, values) {
  params <- list(values=values)
  tempReport <- file.path(tempdir(), "report.Rmd")
  tempDataLoadingReport <- file.path(tempdir(), "data_loading_report.Rmd")
  tempPreprocessingReport <- file.path(tempdir(), "preprocessing_report.Rmd")
  tempProcessingReport <- file.path(tempdir(), "processing_report.Rmd")
  tempAnnotationReport <- file.path(tempdir(), "annotation_report.Rmd")
  pdf()
  params <- computePreprocessingReportData(params)
  params <- computeProcessingReportData(params)
  dev.off()
  file.copy(system.file("Rmd", "report.Rmd", package = "MSProfileR"), tempReport, overwrite = TRUE)
  file.copy(system.file("Rmd", "data_loading_report.Rmd", package = "MSProfileR"), tempDataLoadingReport, overwrite = TRUE)
  file.copy(system.file("Rmd", "preprocessing_report.Rmd", package = "MSProfileR"), tempPreprocessingReport, overwrite = TRUE)
  file.copy(system.file("Rmd", "processing_report.Rmd", package = "MSProfileR"), tempProcessingReport, overwrite = TRUE)
  file.copy(system.file("Rmd", "annotation_report.Rmd", package = "MSProfileR"), tempAnnotationReport, overwrite = TRUE)
  rmarkdown::render(tempReport,
                    output_file = file,
                    params = params,
                    envir = new.env(parent = globalenv())
  )
}

writeParameters <- function(file, values) {
  params <- extractParameters(values)
  write(toJSON(params), file)
}

writeIntensityMatrixCSVFile <- function(file, values) {
  if (is.null(values$intensityMatrix)) { return() }
  intensityMatrix <- values$intensityMatrix
  intensityMatrix <- cbind(extractSpectraMetadata(values$peakDetectionSpectra)[,'sampleName'], intensityMatrix)
  write.matrix(intensityMatrix, file=file, sep = ';')
}

writeMassBoundsPlot <- function(file, values) {
  if (is.null(values$trimmedSpectra)) { return() }
  ggsave(file, computeMassBoundsPlot(values$trimmedSpectra))
}

writeScreeningPlot <- function(file, values) {
  if (is.null(values$screeningPlot)) { return() }
  svg(file=file, width = 8, height = 4)
  plot(values$screeningPlot)
  dev.off()
}


writeSelectedPlot <- function(file, values) {
  if (is.null(values$selectedSpectrum)) { return() }
  svg(file=file, width=8, height=4)
  if (values$selectedSpectrumState == 'atypical') {
    MALDIquant::plot(values$atypicalSpectraAfterSelection[[values$selectedSpectrum]])
  } else {
    MALDIquant::plot(values$selectedSpectra[[values$selectedSpectrum]])
  }
  dev.off()
}


writePeaksCountsBySNRPlot <- function(file, values) {
  if (is.null(values$peakCountsBySNR)) { return() }
  svg(file=file, width = 8, height = 4)
  boxplot(values$peakCountsBySNR)
  dev.off()
}

writeReferencePeaksPlot <- function(file, values) {
  if (is.null(values$referencePeaks)) { return () }
  svg(file=file, width = 8, height = 4)
  MALDIquant::plot(values$referencePeaks)
  dev.off()
}

writeAlignedSpectraPlot <- function(file, values) {
  if (is.null(values$alignedSpectra)) { return () }
  svg(file=file, width = 8, height = 4)
  computeSpectraPheatmap(values$alignedSpectra)
  dev.off()
}

writeBinnedPeaksPlot <- function(file, values) {
  if (is.null(values$binnedPeaks)) { return () }
  svg(file=file, width = 8, height = 4)
  computeSpectraPheatmap(values$binnedPeaks)
  dev.off()
}

writeFilteredPeaksPlot <- function(file, values) {
  if (is.null(values$filteredPeaks)) { return () }
  svg(file=file, width = 8, height = 4)
  computeSpectraPheatmap(values$filteredPeaks)
  dev.off()
}

writeIntensityMatrixPlot <- function(file, values, width, height, showRownames) {
  if (is.null(values$intensityMatrix)) { return () }
  svg(file=file, width = width, height = height)
  computeIntensityMatrixPheatmap(values$intensityMatrix, values$alignedSpectra, showRownames)
  dev.off()
}

writeDendrogramPlot <- function(file, values) {
  if (is.null(values$intensityMatrix)) { return () }
  svg(file=file, width = 16, height = 8)
  par(mar = c(0, 0, 0, 10))
  pheatmap_ = computeIntensityMatrixPheatmap(values$intensityMatrix, values$alignedSpectra, TRUE)
  plot(as.dendrogram(pheatmap_$tree_row), horiz=TRUE)
  dev.off()
}

writeFigures <- function(path, values) {
  dir.create(file.path(path, "figures"))
  writeMassBoundsPlot(file.path(path, "figures", "mass_bounds.svg"), values)
  writeScreeningPlot(file.path(path, "figures", "screening.svg"), values)
  writePeaksCountsBySNRPlot(file.path(path, "figures", "peak_counts_by_SNR.svg"), values)
  writeReferencePeaksPlot(file.path(path, "figures", "reference_peaks.svg"), values)
  writeAlignedSpectraPlot(file.path(path, "figures", "spectra_aligned.svg"), values)
  writeBinnedPeaksPlot(file.path(path, "figures", "peaks_binned.svg"), values)
  writeFilteredPeaksPlot(file.path(path, "figures", "peaks_filtered.svg"), values)
  writeIntensityMatrixPlot(file.path(path, "figures", "intensity_matrix_32x16.svg"), values, 32, 16, TRUE)
  writeIntensityMatrixPlot(file.path(path, "figures", "intensity_matrix_16x8.svg"), values, 16, 8, TRUE)
  writeIntensityMatrixPlot(file.path(path, "figures", "intensity_matrix_8x4.svg"), values, 8, 4, FALSE)
  writeDendrogramPlot(file.path(path, "figures", "dendrogram.svg"), values)
}

writeFiguresZip <- function(file, values) {
  tmp <- tempfile()
  dir.create(tmp)
  writeFigures(tmp, values)
  zipfile <- tempfile(fileext = ".zip")
  zip(file, "figures", root = tmp)
}

writeH5File <- function(file, values, withParameters, withSpectra, withIntensityMatrix, withAnnotations) {
  file.h5 <- H5File$new(file, mode="w")
  if (withParameters) {
    file.h5[['parameters']] <- toJSON(extractParameters(values))
  }
  if (withSpectra && !is.null(values$mergedSpectra)) {
    spectra <- values$mergedSpectra
    file.h5$create_group('spectra')
    for (id in names(spectra)) {
      spectrum <- spectra[[id]]
      path <- paste('spectra', id, sep='/')
      file.h5[[path]] <- rbind(intensity(spectrum), mass(spectrum))
      h5attr(file.h5[[path]], 'sampleName') <- spectrum@metaData$sampleName
      h5attr(file.h5[[path]], 'replicateName') <-spectrum@metaData$replicateName
      h5attr(file.h5[[path]], 'acquisitionDate') <- spectrum@metaData$acquisitionDate
    }
  }
  if (withIntensityMatrix && !is.null(values$intensityMatrix)) {
    file.h5$create_group('intensity_matrix')
    intensityMatrix <- values$intensityMatrix
    spectra <- values$mergedSpectra
    file.h5[['intensity_matrix/matrix']] <- intensityMatrix
    file.h5[['intensity_matrix/mass']] <- as.double(colnames(intensityMatrix))
    file.h5[['intensity_matrix/id']] <- names(spectra)
  }

  if (withAnnotations && !is.null(values$annotations)) {
    file.h5[['annotations']] <- values$annotations
  }

  file.h5$close_all()
}

writeAllFilesZip <- function(file, values, withParameters, withSpectra, withIntensityMatrix, withAnnotations) {
  tmp <- tempfile()
  dir.create(tmp)
  dir.create(file.path(tmp, "files"))
  writeParameters(file.path(tmp, "files", "parameters.csv"), values)
  writeReport(file.path(tmp, "files", "report.pdf"), values)
  writeH5File(file.path(tmp, "files", "data.h5"), values, withParameters, withSpectra, withIntensityMatrix, withAnnotations)
  writeFigures(file.path(tmp, "files"), values)
  zipfile <- tempfile(fileext = ".zip")
  zip(file, "files", root = tmp, recurse = TRUE)
}
