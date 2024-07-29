# Preparation des jeux de données
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(anses)



##################### Pre-processing des données anses ########



# Fonction d'enlevage des parenthèses dans un vecteur de nom
rm_parentheses <- function(x) {

  x |>
    str_remove(pattern = regex("\\(mg/100 g\\)")) |>
    str_remove(pattern = regex("\\(g/100 g\\)")) |>
    str_remove(pattern = regex("\\(kcal /100 g\\)")) |>
    str_trim()

}


# Attention, des tirets on été ajoutés en amont dans la feuille excel pour permettre de lire la colonne
# "alim_nom_sci"
# Par ailleurs, les noms du ssgrp du code 0305 ont été ajoutés à la main

raw_data <-readxl::read_xls(path = "data-raw/Table Ciqual 2020_FR_2020 07 07.xls",col_types = "text",na=c("","-")) |>
  mutate(alim_nom_sci=as.character(alim_nom_sci))


# Transformation des datas en data numériques + élagage et synthèse

data_clean_A <- raw_data[,c(1:9,13,16:19,27,49,50,56:59, 61:62,65:76)]  |>
  slice(-1) |>
  select(!contains("µ")) |>
  mutate(across(.cols=where(is.character),.fns = ~case_when(str_detect(string=.x,pattern = "<")~str_remove(string=.x,pattern = "<"),
                                                            TRUE~.x))) |>
  mutate(across(.cols=where(is.character),.fns = ~case_when(.x=="traces"~"0.0001",TRUE~.x))) |>
  mutate(across(10:30,.fns=~str_replace(string=.x,pattern=",",replacement = "."))) |>
  mutate(across(10:30,.fns=as.numeric)) |>
  rowwise() |>
  mutate(`Vitamines(mg/100 g)`=sum(c_across(starts_with("Vitamine")),na.rm = TRUE)) |>
  mutate(`Sels Mineraux (mg/100 g)`=sum(c_across(18:23),na.rm = TRUE)) |>
  dplyr::ungroup()




# Gestion des noms
# Filtrage des lignes ayant un minimum de 8 données et scorification (attention à la fonction utilisée)
data_clean <- data_clean_A[,-c(18:30)] |>
  rowwise() |>
  mutate(available=sum(!is.na(c_across(where(is.numeric))))) |>
  ungroup() |>
  filter(available > 7) |>
  select(-available) |>
  select(-ends_with("code")) |>
  rename (`Protéines (g/100 g)`=`Protéines, N x 6.25 (g/100 g)`,
          `Calories (kcal /100 g)`= `Energie, N x facteur Jones, avec fibres  (kcal/100 g)`,
          `G1` =alim_grp_nom_fr,
          `G2`=alim_ssgrp_nom_fr,
          `G3` =alim_ssssgrp_nom_fr,
          `Nom Scientifique` =alim_nom_sci,
          Nom = alim_nom_fr,
          `Sel (g/100 g)`= `Sel chlorure de sodium (g/100 g)`,
          `Fibres (g/100 g)`=`Fibres alimentaires (g/100 g)`
  )  |>
  mutate(across(.cols=where(is.numeric),.fns=\(x) scorifyGamma(x,shape=1),.names="{.col}_score"))

usethis::use_data(data_clean,overwrite = TRUE)


# Structuration des données pour utilisation dans Plotly


dataHierViz<- create_potly_params(initial_data = data_clean,
                                  categories = c("G1","G2","G3","Nom")
                                  )

# Création d'une liste permettant de reconstituer les labels
colsAll<- names(dataHierViz)[!str_detect(names(dataHierViz),"score")][-(1:3)]


labelsHover <- map(colsAll,\(x) paste0(paste0(x," : "),dataHierViz[[x]] |> round(0))) |>
  bind_cols() |>
  rowwise() |>
  mutate(hover = list(c_across(everything())) ) |>
  select(hover) |>
  pull()


# Naming éléments de la liste
names(labelsHover) <- dataHierViz$labels
for (i in (1:length(labelsHover))) names(labelsHover[[i]])=colsAll

usethis::use_data(dataHierViz,labelsHover,overwrite = TRUE)

############ Gestion des couleurs ##################

# Palette de couleur de l'anses

ansesPalette <- c("#000091",
                  "#FFFFFF",
                  "#E1000F",
                  "#FF9940",
                  "#FFE800",
                  "#00AC8C",
                  "#5770BE"
)


# Liste des critères
criteresList <- data_G1 |>
  select(ends_with("score")) |>
  names() |>
  str_remove(pattern = "_score")

# Choix de couleur pour chacun des critères
# Même dimension que le vecteur de critères
couleursCriteres <- c("#E1000F","#000091", "#FFE800","#FF9940","#FFE800","#5770BE","#E1000F","#5770BE","#00AC8C","#00AC8C")
names(couleursCriteres)<- criteresList

# Objectif a priori concernant les critères
objectifCriteres <- c("min","max","min","min","min","min","min","min","max","max")
names(objectifCriteres)<- criteresList

usethis::use_data(criteresList,couleursCriteres,objectifCriteres,overwrite = TRUE)
usethis::use_data(objectifCriteres,overwrite = TRUE)

# nn <- 6
# paletteHigh <- seq_gradient_pal(low =ansesPalette[7],
#                                 high = ansesPalette[6]
# )(seq(0,1,length.out=nn))[-1]
#
# paletteMed <- seq_gradient_pal(low =ansesPalette[4],
#                                high = ansesPalette[7]
# )(seq(0,1,length.out=nn))[-1]
#
# paletteLow <- seq_gradient_pal(low =ansesPalette[3],
#                                high = ansesPalette[4]
# )(seq(0,1,length.out=nn))[-1]
#
# paletteSunburst <- c(paletteLow,paletteMed,paletteHigh)





