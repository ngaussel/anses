#' create_potly_params
#'
#' fonction generique qui cree les champs ids, parents et labels necessaires à l'utilisation des graphiques hierarchiques plotly
#' les valeurs numeriques sont moyennees lors de l'aggregation
#'
#' @param initial_data le dataframe a mettre en forme
#' @param categories le vecteur de noms de groupes qui servira au naming.
#' Ces colonnes doivent être ordonnees par ordre decroissant d'aggregation. Si une ligne contient un NA, toutes les lignes de droite, sauf la dernière, doivent contenir un NA.
#' Donc ni la première categorie ni la dernière ne doivent contenir de NA. La fonction s'assure que cette condition est bien respectee
#' @param sep le separateur utilise pour le naming
#' @param root le symbole utilise pour les categories sans parents
#'
#' @import dplyr
#' @importFrom purrr map reduce
#' @importFrom assertthat assert_that
#' @importFrom stringr str_remove_all

#'
#' @return le même dataframe auquel ont ete adjointes les categories ids, parents et labels
#' @export
#'
create_potly_params <- function(initial_data, categories, sep = " -- ", root = "") {


  assert_that(length(categories) > 1, msg = "Il faut au moins un groupement")
  assert_that(length(which(is.na(initial_data[[categories[length(categories)]]]))) == 0, msg = "Il y a des elements terminaux sans nom")
  assert_that(length(which(is.na(initial_data[[categories[1]]]))) == 0, msg = "Il y a des elements de la première categorie sans nom")



  # Si une categorie autre que la première a un NA, alors toutes les suivantes, sauf la dernière, sont mises en NA aussi
  # Cette condition ne s'applique donc qu'avec un vecteur de categories de longueur strictement superieure à 3

  modified_data <- initial_data

  # Fonction d'agregation. POurrait être passe en argument si besoin
  agregate_method <- function(x) mean(x, na.rm = TRUE)

  if (length(categories)>3) {
    for (i in 2:(length(categories) - 2)) {
      for (j in (i + 1):(length(categories) - 1)) {
        modified_data[which(is.na(initial_data[[categories[i]]])), categories[j]] <- NA
      }
    }
  }
  create_one_col_params <- function(categorie_name) {

    categorie_rank <- which(categories == categorie_name)
    categorie_parents <- categories[seq_len(categorie_rank)]

    modified_data |>
      dplyr::filter(!is.na(!!dplyr::sym(categorie_name))) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(categorie_parents |> rev()))) |>
      dplyr::summarise(dplyr::across(.cols = dplyr::where(is.numeric), .fns = list(average = agregate_method), .names = "{.col}")) |>
      dplyr::ungroup() |>
      dplyr::relocate(dplyr::any_of(categorie_parents)) |>
      dplyr::rowwise() |>
      dplyr::mutate(
        ids = paste(dplyr::c_across(dplyr::any_of(categorie_parents) ),
          collapse = sep
        ),
        parents = ifelse(categorie_rank > 1,
          paste(dplyr::c_across(dplyr::any_of(categorie_parents)[-categorie_rank]),
            collapse = sep
          ),
          root
        ),
        .before = 1
      ) |>
      dplyr::ungroup() |>
      dplyr::mutate(labels = !!dplyr::sym(categorie_name), .after = 2) |>
      dplyr::select(-dplyr::any_of(categorie_parents))
  }

  map(categories, create_one_col_params)  |>
    reduce(bind_rows) |>
    mutate(parents = str_remove_all(string=parents,pattern=paste0(sep,NA)),
           ids = str_remove_all(string=ids,pattern=paste0(sep,NA)))
}
