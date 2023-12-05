

createActionButton <- function(enabled, actionId, actionLabel, resetId, resetLabel) {
  if (enabled) {
    tagList(
      icon("times", style = "color: rgb(160,20,20); width:15px"),
      actionButton(actionId, actionLabel)
    )
  } else {
    tagList(
      icon("check", style = "color: rgb(20,160,20); width:15px"),
      actionButton(resetId, resetLabel)
    )
  }
}

enableOrDisableParams <- function(elements, params) {
  if (isFALSE(params)) {
    for(element in elements) { enable(element) }
  } else {
    for(element in elements) { disable(element) }
  }
}
