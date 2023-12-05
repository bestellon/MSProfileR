extractAllPreprocessingParams <- function(params, values) {
  params$trimParams <- values$trimParams
  params$conformityParams <- values$conformityParams
  params$cleanParams <- values$cleanParams
  params$screenParams <- values$screenParams
  params$mergeParams <- values$mergeParams
  return(params)
}

initializePreprocessingParams <- function(values) {
  values$trimParams <- FALSE
  values$conformityParams <- FALSE
  values$cleanParams <- FALSE
  values$screenParams <- FALSE
  values$mergeParams <- FALSE
}

executePreprocessing <- function(values) {
  values$trimmedSpectra <- trimSpectra(values$spectra, values$trimParams)
  values$conformitySpectra <- conformitySpectra(values$trimmedSpectra, values$conformityParams)
  values$cleanedSpectra <- cleanSpectra(values$trimmedSpectra, values$cleanParams)
  screenResults <- screenSpectra_(values$cleanedSpectra, values$screenParams)
  values$typicalSpectra <- screenResults$typicalSpectra
  values$atypicalSpectra <- screenResults$atypicalSpectra
  values$screeningPlot <- screenResults$screeningPlot
  values$atypicalSpectraAfterSelection <- values$atypicalSpectra
  values$selectedSpectra <- values$typicalSpectra
  values$screeningEstTable <- screenResults$screeningEstTable
  values$mergedSpectra <- mergeSpectra(values$selectedSpectra, values$mergeParams)
  return(values)
}

trimSpectra <- function(spectra, params) {
  if (is.null(spectra) || isFALSE(params)) { return (spectra) }
  return(trim(spectra, range=params$range*1000))
}

computeMassBoundsPlot <- function(spectra) {
  if (is.null(spectra)) { return() }
  masses <- c()
  bounds <- c()
  for (i in 1:length(spectra)){
    if (length(spectra[[i]]@mass) == 0) { next }
    masses <- c(masses, min(spectra[[i]]@mass), max(spectra[[i]]@mass))
    bounds <- c(bounds, "minimum", "maximum")
  }
  if (length(masses) == 0) {
    return(NULL)
  }
  masses = masses / 1000
  plot <- qplot(masses, rep(0, length(spectra)*2), color=bounds) +
    theme(aspect.ratio=0.05,
          legend.position = "none",
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) +
    scale_x_continuous(breaks = round(seq(min(masses), max(masses), by=1), 1)) +
    xlab("Masses (KDa)")
  return(plot)
}



conformityTests <- function(spectra) {
  if (is.null(spectra)) { return() }
  emptySpectraCount <- sum(sapply(spectra, isEmpty))
  irregularSpectraCount <- sum(!sapply(spectra, isRegular))
  lengthTable <- table(sapply(spectra, length))
  differentSpectraLengthCount <- (dim(lengthTable) > 10)
  results <- data.frame(
    Criteria = c("Number of empty spectra",
                 "Number of irregular spectra",
                 "Number of different lengths"),
    Results = c(emptySpectraCount, irregularSpectraCount, differentSpectraLengthCount)
  )
  return(results)
}

conformitySpectra <- function(spectra, params) {
  if (is.null(spectra) || isFALSE(params)) { return (spectra) }
  if (params$excludeEmptySpectra && length(spectra) > 0) {
    spectra <- spectra[!sapply(spectra, isEmpty)]
    spectra <- spectra[sapply(spectra, isRegular)]
  }
  if (length(spectra) == 0) {
    return (NULL)
  }
  return (spectra)
}

cleanSpectra <- function(spectra, params) {
  if (is.null(spectra) | isFALSE(params)) { return (spectra) }
  spectra <- transformIntensity(spectra,
                                method=params$transformIntensityMethod)
  spectra <- smoothIntensity(spectra,
                             method=params$smoothIntensityMethod,
                             halfWindowSize=params$halfWindowSize)
  if (params$removeBaselineMethod=="SNIP") {
    spectra <- removeBaseline(spectra,
                              method=params$removeBaselineMethod,
                              iterations=params$removeBaselineIterations)
  } else {
    spectra <- removeBaseline(spectra,
                              method=params$removeBaselineMethod)
  }
  spectra <- calibrateIntensity(spectra,
                                method=params$calibrateIntensityMethod)
  names(spectra) <- seq(from=1, to=length(spectra))
  return(spectra)
}

screenSpectra_ <- function(spectra, params) {
  if (is.null(spectra) | isFALSE(params)) {
    return(list(
      typicalSpectra=spectra,
      atypicalSpectra=list(),
      screeningPlot=NULL
    ))
  }
  screenResults <- screenSpectra(spectra,
                                 estimator = params$scaleEstimator,
                                 method=params$atypicalIdentificationMethod,
                                 threshold = params$threshold)
  if (params$includeLowerSpectra) {
    screenResults$lower <- 0
  }
  est <- screenResults$est.table$`A score`
  typicalSpectra <- spectra[which(est <= screenResults$upper & est >= screenResults$lower)]
  atypicalSpectra <- spectra[which(est > screenResults$upper | est < screenResults$lower | is.na(est))]
  screeningPlot <- plot.scSpectra(screenResults, labels = TRUE)
  return(list(typicalSpectra=typicalSpectra,
              atypicalSpectra=atypicalSpectra,
              screeningPlot=screeningPlot,
              screeningEstTable=screenResults$est.table))
}

mergeSpectra <- function(spectra, params) {
  if (is.null(spectra) | isFALSE(params)) { return (spectra) }
  metadata <- extractSpectraMetadata(spectra)
  mergedSpectra <- averageMassSpectra(spectra,
                                      labels=factor(metadata$sampleName),
                                      method=params$averageMassSpectraMethod)
  names(mergedSpectra) <- seq(from=1, to=length(mergedSpectra))
  for (id in names(mergedSpectra)) {
    spectrum <- mergedSpectra[[id]]
    mergedSpectra[[id]]@metaData$id <- id
    mergedSpectra[[id]]@metaData$sampleName <- spectrum@metaData$sampleName[1]
    mergedSpectra[[id]]@metaData$replicateName <- paste(spectrum@metaData$replicateName, collapse=' ')
    mergedSpectra[[id]]@metaData$acquisitionDate <- spectrum@metaData$acquisitionDate[1]
    mergedSpectra[[id]]@metaData$name <- paste( mergedSpectra[[id]]@metaData$sampleName,
                                                ' [', mergedSpectra[[id]]@metaData$replicateName, ']', sep='')
  }
  return (mergedSpectra)
}


computeListForSelection <- function(spectra, estTable, decreasing) {
  displayName <- function(spectrum) {
    paste(spectrum@metaData$id,': ', spectrum@metaData$sampleName,' [',spectrum@metaData$replicateName,']',sep='')
  }
  if (!is.null(estTable)) {
    ids <- data.frame('ID'=names(spectra), 'displayName'=unlist(lapply(spectra, displayName)))
    dataframe <- merge(x = ids, y = estTable, by = "ID", all.x = TRUE)
    dataframe <- dataframe[order(dataframe[['A score']], decreasing=decreasing),]
    row.names(dataframe) <- dataframe[['ID']]
    tryCatch(
      {dataframe[['str']] <- paste(dataframe[['displayName']],' (', round(dataframe[['A score']],2),')', sep='')},
      error=function(cond) {}
    )
    result <- as.list(dataframe[['ID']])
    names(result) <- dataframe[['str']]
    return(result)
  } else {
    result <- names(spectra)
    names(result) <- unlist(lapply(spectra, displayName))
    return(result)
  }
}

computeNameFromSpectraIds <- function(spectra, ids) {
  if (is.null(spectra) || is.null(ids)) { return() }
  displayName <- function(id) {
    paste(spectra[[id]]@metaData$id,': ', spectra[[id]]@metaData$sampleName,' [',spectra[[id]]@metaData$replicateName,']',sep='')
  }
  return (unlist(lapply(ids, displayName)))
}

computeAddedSpectraIds <- function(atypicalSpectra, atypicalSpectraAfterSelection) {
  if (is.null(atypicalSpectra) || is.null(atypicalSpectraAfterSelection)) { return(c()) }
  atypicalSpectraIds <- names(atypicalSpectra)
  atypicalSpectraAfterSelectionIds <- names(atypicalSpectraAfterSelection)
  return(setdiff(atypicalSpectraIds, atypicalSpectraAfterSelectionIds))
}

computeRemovedSpectraIds <- function(atypicalSpectra, atypicalSpectraAfterSelection) {
  if (is.null(atypicalSpectra) || is.null(atypicalSpectraAfterSelection)) { return(c()) }
  atypicalSpectraIds <- names(atypicalSpectra)
  atypicalSpectraAfterSelectionIds <- names(atypicalSpectraAfterSelection)
  return(setdiff(atypicalSpectraAfterSelectionIds, atypicalSpectraIds))
}
