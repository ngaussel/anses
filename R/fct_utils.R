bs4SidebarUserAnses <- function(name, image = NULL, href = "#") {
  shiny::tags$div(
    class = "user-panel mt-3 pb-3 mb-3 d-flex",
    if (!is.null(image)) {
      shiny::tags$div(class = "image", shiny::img(
        src = image,
        class = "img-circle elevation-2"
      ))
    }, shiny::tags$div(class = "info_user", shiny::a(
      class = "d-block",
      href = href,
      target = "_blank",
      name
    ))
  )
}


styleDT <- function(table) {

  n<- length(names(table))
  col_data <- 1:(n-1)
  table |>
    DT::datatable(
      rownames = FALSE,
      filter = "top",
      extensions = "FixedColumns",
      options = list(
        pageLength = 8,
        autowidth = TRUE,
        fixedColumns = TRUE,
        scrollX = TRUE,
        columnDefs = list(list(className = 'dt-center', targets = col_data)),
        sDom = '<"top">lrt<"bottom">ip',
        initComplete = DT::JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({ 'color': '#1e90ff'});",
          "}"
        ),
        buttons = c("copy", "csv", "excel")
      )
    )
}
