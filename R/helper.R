
#' scorifyGauss
#' Transforme une serie de nombre de type gaussien en score
#' @param x un vecteur
#'
#' @importFrom stats sd
#' @return un score
#' @export
#'
scorifyGauss <- function(x) {
  (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
  }

#' scorifyGamma
#' Transforme une serie de nombre de type gamma en score
#' Le paramétrage est déterminé de façon à trouver un intermédiaire entre approche cardinale
#' et ordinale i.e. on prend un paramètre de forme >1
#'
#' @param x un vecteur
#' @param shape un paramètre de forme. Egal à 1 par défaut. Comportement différent si <1 ou >1
#'
#' @importFrom stats var pgamma
#'
#' @return un score
#' @export
#'
scorifyGamma <- function(x,shape=1){

  # shape <- mean(x,na.rm=TRUE)^2/var(x,na.rm=TRUE)
  scale <- var(x,na.rm=TRUE)/mean(x,na.rm=TRUE)
  pgamma(x,shape = shape,scale = scale)
}


#' scorifyNonPara
#' Transforme une serie de nombre en score de façon non paramétrique
#'
#' @param x les valeurs initiales
#'
#' @return les scores
#' @export
#'
scorifyUni <- function(x){
  ecdf(x)(x)
}


#' scorifyUni
#' Transforme une serie de nombre de type uniforme en score
#'
#' @param x les valeurs initiales
#'
#' @return les scores
#' @export
#'
scorifyUni <- function(x){
  (x-min(x,na.rm=TRUE))/(max(x,na.rm=TRUE)-min(x,na.rm=TRUE))
}




#' add_score
#' Prend en entrée un tibble qui contient des scores et renvoie avec un score calculé dans une colonne score
#' Attention, la façon d'agréger les scores doit être cohérente avec la façon dont ils ont été calculés individuellement.
#'
#' @param weight un vecteur de poids constitué uniquement de 0, 1 ou -1
#' @param dataset les données utilisées
#'
#'
#' @importFrom stringr str_detect
#' @importFrom dplyr mutate select across case_when ends_with
#' @importFrom stringr str_detect str_remove
#' @return le data_frame avec une colonne score
#' @export
#'
add_score <- function(dataset,weight ) {
  eps <- 1e-8
  if (!is.null(dataset$score)) dataset$score <- NULL

  n_scores <- dataset |>
    names() |>
    str_detect(pattern = "score") |>
    sum()

  # Reduction de la dimension aux critères non nuls (pour la future gestion des NA)

  norm1 <- sum(abs(weight))

  if (length(weight) != n_scores | prod((abs(weight) > 1)) != 0 | abs(norm1) < eps) {
    dataset <- dataset |>
      mutate(score = NA)
  } else {

    weightN <- weight[abs(weight) >eps] |> rep(nrow(dataset))/norm1
    weightN =  matrix(abs(weightN),nrow=nrow(dataset),byrow = TRUE)


    # Lorsqu'un poids est négatif, on inverse le score au sens où on prend 1- score plutôt que score
    scores <- dataset |>
      select(ends_with("score")) |>
      mutate(across(which(weight<0),\(x) 1-x)) |>
      select(which(abs(weight) >eps)) |>
      as.matrix(nrow=nrow(dataset))

    # browser()

    score = apply(scores * weightN,1,function (x) {

      if (all(is.na(x))){
        res <- NA
      }else{
        res<- sum(x,na.rm=TRUE)
      }
      res

      })



    dataset |>
      mutate(score = (100*score) |> round(0)
        )
  }
}

#
# fibresRaw <- data_clean$`Fibres (g/100 g)`
# scoreFibre <- ecdf(fibresRaw)(fibresRaw)
#
#
# plot(x=scoreFibre,y=data_clean$`Fibres (g/100 g)_score`)
