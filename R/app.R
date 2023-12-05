ui <- function() {
  dashboardPage(
    dashboardHeader(title = "MSProfileR Tool"),
    dashboardSidebar(sidebarMenu(id = "sidebarMenu",
                                 menuItem("1. Data loading", tabName = "data_loading", icon = icon("upload")),
                                 menuItem("2. Preprocessing", tabName = "preprocessing", icon = icon("gear")),
                                 menuItem("3. Processing", tabName = "processing", icon = icon("gear")),
                                 menuItem("4. Annotations", tabName = "annotation", icon = icon("upload")),
                                 menuItem("5. Output", tabName = "output", icon = icon("download"))
    )
    ),
    dashboardBody(
      useShinyjs(),
      add_busy_spinner(spin = "fading-circle"),
      tabItems(
        tabItem(tabName = "data_loading", fluidPage(dataLoadingUI("data_loading"))),
        tabItem(tabName = "preprocessing", fluidPage(preprocessingUI("preprocessing"))),
        tabItem(tabName = "processing", fluidPage(processingUI("processing"))),
        tabItem(tabName = "annotation", fluidPage(annotationUI("annotation"))),
        tabItem(tabName = "output", fluidPage(reportingUI("reporting")))
      )
    )
  )
}

server <- function(input, output, session) {
  values <- reactiveValues(newParams=list())
  dataLoadingServer("data_loading", values)
  preprocessingServer("preprocessing", values)
  processingServer("processing", values)
  annotationServer("annotation", values)
  reportingServer("reporting", values)
}

