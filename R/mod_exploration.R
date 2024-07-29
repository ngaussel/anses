#' exploration UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @importFrom shinyWidgets pickerOptions prettyRadioButtons
#' @importFrom plotly plotlyOutput
#'
#' @importFrom shinyWidgets pickerInput
#' @importFrom shiny NS tagList
#' @noRd
mod_exploration_ui <- function(id) {
  ns <- NS(id)


  tagList(
    tags$style(
      HTML("
        table.dataTable td {
          white-space: nowrap;
          overflow: hidden;
          text-overflow: ellipsis;
        }
        .fixed-width {
          max-width: 150px;
        }
      ")
    ),
    titlePanel("Critères de choix"),
    br(),
    fluidRow(
      column(
        width = 4,
        pickerInput(
          inputId = ns("criteres"),
          label = "Vos critères",
          choices = criteresList,
          multiple = TRUE,
          options = pickerOptions(
            noneSelectedText = "Choisir au moins un critère"
          ),
          selected = criteresList[6]
        )
      ),
      column(
        8,
        tags$p("Vos objectifs", style = "font-weight:bold"),
        uiOutput(ns("selectedList"))
      )
    ),
    fluidRow(
      column(
        3,
        tags$strong("Choix des informations : ")
      ),
      column(
        3,
        prettyRadioButtons(
          inputId = ns("hoverChoice"),
          label = NULL,
          choices = c("Exhaustif", "Choix", "Score"),
          selected = "Choix",
          shape = "curve",
          inline = TRUE
        )
      ),
      column(
        6,
        tags$strong("Pour plus d'explications, voir l'onglet Notes.")
      )
    ),
    bs4Dash::tabsetPanel(
      id = ns("tabcards"),
      side = "left",
      tabPanel(
        title = "Graphique",
        value = "graphique",
        active = FALSE,
        h3("Vue graphique"),
        plotlyOutput(ns("graphHierarchical"), width = "100%", height = "600px")
      ),
      tabPanel(
        title = "Données",
        value = "donnes",
        active = TRUE,
        h3("Données plus détaillés"),
        fluidRow(
          column(
            1,
            tags$strong("Niveau de détail : ")
          ),
          column(
            4,
            prettyRadioButtons(
              inputId = ns("detail"),
              label = NULL,
              choices = c("Par catégorie", "Par sous-catégorie", "Par plat"),
              selected = "Par sous-catégorie",
              shape = "curve",
              inline = TRUE
            )
          )
        ),
        fluidRow(
        DT::dataTableOutput(ns("tableAliments"))
      )
      )
    )
  )
}

#' exploration Server Functions
#'
#' @importFrom purrr map
#' @importFrom dplyr select left_join
#' @importFrom shinyWidgets switchInput
#' @importFrom stringr str_detect str_extract str_remove regex
#' @importFrom plotly plot_ly renderPlotly
#' @noRd
mod_exploration_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    dns <- function(x) str_remove(x, ns("")) # Remove name_spacing

    # Prefixe des inputs de choix
    prefChoices <- "obj"

    r <- reactiveValues(
      choices = ""
    )

    observeEvent(input$criteres, {
      criteria <- tagList()



      r$names <- paste0(prefChoices, input$criteres)
      r$namesNS <- ns(r$names)

      # Insertion des switch de choix correspondants au critères sélectionnés
      # c <- input$criteres[1]

      i <- 1
      for (c in input$criteres) {
        nameC <- paste0(prefChoices, c)

        previous <- input[[nameC]]

        # Test d'une éventuelle valeur précédente à laisser inchangée
        if (is.null(previous)) {
          criteria[[i]] <- switchInput(
            size = "mini",
            width = "120px",
            inputId = ns(nameC),
            label = c,
            onLabel = "Max",
            offLabel = "Min",
            onStatus = "success", # Green when ON
            offStatus = "danger", # Red when OFF,
            value = ifelse(objectifCriteres[c] == "max", TRUE, FALSE),
            inline = TRUE
          )
        } else {
          criteria[[i]] <- switchInput(
            size = "mini",
            width = "120px",
            inputId = ns(nameC),
            label = c,
            onLabel = "Max",
            offLabel = "Min",
            onStatus = "success", # Green when ON
            offStatus = "danger", # Red when OFF,
            value = previous,
            inline = TRUE
          )
        }

        i <- i + 1

        # Renvoi effectif des critères dans l'UI
        output$selectedList <- renderUI({
          criteria
        })
      }
    })

    # Calcul des données avec score
    observeEvent(
      lapply(r$names, \(x) {
        input[[x]]
      }),
      ignoreInit = TRUE,
      {
        # x <- criteresList[6]
        r$w <- map(criteresList, function(x) {
          if (x %in% stringr::str_remove(r$names, prefChoices)) {
            res <- ifelse(input[[paste0(prefChoices, x)]], 1, -1)
          } else {
            res <- 0
          }
          res
        }) |> unlist()

        r$dataPlot <- dataHierViz |>
          add_score(weight = r$w)
      }
    )

    # Création du graphe
    output$graphHierarchical <- plotly::renderPlotly({
      req(r$dataPlot, input$hoverChoice)

      # Creation d'un hover plus ou moins complet
      if (input$hoverChoice == "Exhaustif") {
        hover <- map(labelsHover, \(x) paste0(x, collapse = "\n")) |> unlist()
      } else if (input$hoverChoice == "Score") {
        hover <- paste0("score = ", r$dataPlot$score)
      } else {
        hover <- map(labelsHover, \(x) paste0(x[which(abs(r$w) > 1e-2)], collapse = "\n")) |> unlist()
      }


      dd <- r$dataPlot |>
        select(ids, labels, parents, score) |>
        mutate(
          hover = hover
        )

      zz <- dd |>
        plot_ly(
          ids = ~ids,
          labels = ~labels,
          parents = ~parents,
          hovertext = ~hover,
          text = ~labels,
          values = ~score,
          color = ~score,
          type = "sunburst",
          hovertemplate = "%{hovertext}<extra></extra>",
          texttemplate = "%{text}",
          maxdepth = 2
        )


    })


    # Création de la table
    output$tableAliments <- DT::renderDT({
      req(r$dataPlot)

      # Niveau de détails
      if (input$detail == "Par catégorie") {
        grouping <- c("G1")
      } else if (input$detail == "Par sous-catégorie") {
        grouping <- c("G2")
      } else {
        grouping <- c("Nom")
      }

      maTable <- data_clean |>
        select(!ends_with("_score")) |>
        mutate(across(where(is.numeric),\(x) round(x,0))) |>
        left_join(r$dataPlot |> select(labels, score), by = c("Nom" = "labels")) |>
        relocate(score, .after = "Nom") |>
        group_by(across(all_of(grouping))) |>
        summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE) |> round(0))) |>
        ungroup() |>
        arrange(-score)


      if (input$hoverChoice == "Exhaustif") {
        maTable <- maTable
      } else if (input$hoverChoice == "Score") {
        maTable <- maTable[,c(1,2)]
      } else {
        m1 <- maTable[,c(1,2)]
        m2 <-
        maTable <- maTable |>
          select(starts_with(input$criteres))
        maTable <-m1 |> bind_cols(maTable)

      }


        maTable |> styleDT()

    })

  })
}












## To be copied in the UI
# mod_exploration_ui("exploration_1")

## To be copied in the server
# mod_exploration_server("exploration_1")
