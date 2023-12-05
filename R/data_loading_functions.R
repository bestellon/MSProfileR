
extractSampleName <- function(fidFile, directoryLevel) {
  fidFile <- chartr(old="\\", new="/", x=fidFile)
  dirs <- strsplit(x=fidFile, split="/")[[1L]]
  numDirs <- length(dirs)
  if (numDirs < directoryLevel) {
    return('SampleName')
  }
  sampleName <- dirs[numDirs-directoryLevel]
  sampleName <- gsub(pattern="[[:punct:]]|[[:space:]]", replacement="_", x=sampleName)
  sampleName
}

loadSpectra <- function(spectraPath, directoryLevel) {
  out <- tryCatch(
    {
      spectra <- import(spectraPath, verbose=TRUE)
      names(spectra) <- seq(from=1, to=length(spectra))
      for (id in names(spectra)) {
        spectrum <- spectra[[id]]
        sampleName <- extractSampleName(spectrum@metaData$file, directoryLevel)
        metaData <- list(
          id=id,
          name=paste(sampleName,' [', spectrum@metaData$spot, ']', sep=''),
          sampleName=sampleName,
          replicateName=spectrum@metaData$spot,
          acquisitionDate=spectrum@metaData$acquisitionDate
        )
        spectra[[id]]@metaData <- metaData
        print(spectrum@metaData$name)
      }
      return(spectra)
    },
    error=function(condition) {
      return (NULL)
    }
  )
  return (out)
}

extractSpectraMetadata <- function(spectra) {
  metadata <- data.frame(matrix(nrow = 0, ncol = 4))
  colnames(metadata) <- c("id", "sampleName", "replicateName", "acquisitionDate")
  
  for (id in names(spectra)) {
    spectrum <- spectra[[id]]
    row <- nrow(metadata) + 1
    metadata[row, "id"] <- spectrum@metaData$id
    metadata[row, "sampleName"] <- spectrum@metaData$sampleName
    metadata[row, "replicateName"] <- spectrum@metaData$replicateName
    metadata[row, "acquisitionDate"] <- spectrum@metaData$acquisitionDate
  }
  
  return(metadata)
}
