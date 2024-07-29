  library(anses)
  library(DT)
  library(dplyr)
  library(stringr)



  styleDT <- function(table) {


    n <- length(names(table))
    col_data <- which(str_detect(names(table),"\\(|score")) -1
    col_names <-setdiff( (1:n)-1,col_data)

    table |>
      DT::datatable(rownames = FALSE,
                    filter = 'top',
                    extensions = 'FixedColumns',
                    options=list(
                      pageLength = 10,
                      autowidth=TRUE,
                      fixedColumns = TRUE,
                      # columnDefs = list(list(width = '400px', targets = 0)),
                      scrollX = TRUE,
                      sDom= '<"top">lrt<"bottom">ip',
                      initComplete = JS(
                      "function(settings, json) {",
                      "$(this.api().table().header()).css({ 'color': '#1e90ff'});",
                      "}"
                    )
                    )
                    )
  }

  ui <- fluidPage(
    # Titre de l'application
    titlePanel("Application Shiny Vide"),

    tags$style(
      HTML("
        table.dataTable td {
          white-space: nowrap;
          overflow: hidden;
          text-overflow: ellipsis;
        }
        .fixed-width {
          width: 150px;
        }
      ")
      ),

    # Contenu principal
    mainPanel(
      "Bienvenue dans votre application Shiny vide!",
     fluidRow(
       DT::dataTableOutput("matable")
    )
  )
  )

  # Définir le serveur (server)
  server <- function(input, output) {


    output$matable<-DT::renderDT({

      table <- data_clean |>
          select(!ends_with("_score")) |>
          mutate(across(where(is.numeric),\(x) round(x,0))) |>
        select(-`Nom Scientifique`)


      table <- table[,-c(1,2,3)]

      table |>
      styleDT()


    })


  }

  # Exécuter l'application Shiny
  shinyApp(ui = ui, server = server)
