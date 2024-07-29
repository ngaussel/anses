#' ansesPalette
#'
#' Palette de couleur prélevée sur le logo de l'anses
#' @format un vecteur de nombres hexadécimaux
#' @source couleur prélevées par pipette sur le logo de l'anses
"ansesPalette"


#' couleursCriteres
#' Attribution de certaines couleurs de la palette à certains critères
#'
#' @format un vecteur nommé de nombres hexadécimaux
#'
"couleursCriteres"


#' objectifCriteres
#' Objectif a priori concernant chacun des critères
#'
#' @format un vecteur constitué de "max" ou de "min"
#'
"objectifCriteres"


#' criteresList
#' Liste des critères utilisés dans la caractérisation des aliments
#'
#' @format un vecteur de charactères
#'
"criteresList"


#' data_clean
#' Les données de l'anses un peu toilettées mais non modifiées
#'
#' @format un tibble
#' @source <https://www.data.gouv.fr/fr/datasets/table-de-composition-nutritionnelle-des-aliments-ciqual/>
"data_clean"


#' dataHierViz
#' Les données de l'anses reformatées pour être utilisées dans un graphe hiérarchique type sunburst ou treemap
#'
#' @format un tibble avec des colonnes ids, parents et labels
"dataHierViz"

#' labelsHover
#' Les éléments de texte qui permettront de créer les labels personnalisés
#'
#' @format un tibble
"labelsHover"
