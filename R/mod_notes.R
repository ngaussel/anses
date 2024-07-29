#' notes UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_notes_ui <- function(id){
  ns <- NS(id)

  tags$iframe(src="www/notes.html",width="800px",height="800px")

}

#' notes Server Functions
#'
#' @noRd
mod_notes_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_notes_ui("notes_1")

## To be copied in the server
# mod_notes_server("notes_1")
