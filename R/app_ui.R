#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom bs4Dash bs4DashPage bs4DashNavbar bs4DashSidebar
#' @importFrom bs4Dash bs4SidebarMenu bs4SidebarMenuItem bs4DashBody bs4TabItems bs4TabItem
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    tags$head(
      HTML("
        <!-- Global site tag (gtag.js) - Google Analytics -->
        <script async src='https://www.googletagmanager.com/gtag/js?id=G-SR5REQFHW3'></script>
        <script>
          window.dataLayer = window.dataLayer || [];
          function gtag(){dataLayer.push(arguments);}
          gtag('js', new Date());

          gtag('config', 'G-SR5REQFHW3');
        </script>
      ")
    ),
    # Your application UI logic
    bs4DashPage(
      title = "Analyse des aliments",
      header = bs4DashNavbar("Analyse des aliments"),
      sidebar = bs4DashSidebar(
        collapsed = TRUE,
        bs4SidebarUserAnses('Base "Ciqual", Anses',
          image = "www/logo_anses_full.png",
          href = "https://www.data.gouv.fr/en/datasets/table-de-composition-nutritionnelle-des-aliments-ciqual/"
        ),
        bs4SidebarMenu(
          bs4SidebarMenuItem("exploration", tabName = "exploration", icon = icon("chart-bar")),
          bs4SidebarMenuItem("Notes", tabName = "notes", icon = icon("user-graduate"))
        )
      ),
      body = bs4DashBody(
        bs4TabItems(
          bs4TabItem(
            tabName = "exploration",
            mod_exploration_ui("exploration")
          ),
          bs4TabItem(
            tabName = "notes",
            mod_notes_ui("notes")
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "anses"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
