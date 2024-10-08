% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/formatToViz.R
\name{create_potly_params}
\alias{create_potly_params}
\title{create_potly_params}
\usage{
create_potly_params(initial_data, categories, sep = " -- ", root = "")
}
\arguments{
\item{initial_data}{le dataframe a mettre en forme}

\item{categories}{le vecteur de noms de groupes qui servira au naming.
Ces colonnes doivent être ordonnees par ordre decroissant d'aggregation. Si une ligne contient un NA, toutes les lignes de droite, sauf la dernière, doivent contenir un NA.
Donc ni la première categorie ni la dernière ne doivent contenir de NA. La fonction s'assure que cette condition est bien respectee}

\item{sep}{le separateur utilise pour le naming}

\item{root}{le symbole utilise pour les categories sans parents}
}
\value{
le même dataframe auquel ont ete adjointes les categories ids, parents et labels
}
\description{
fonction generique qui cree les champs ids, parents et labels necessaires à l'utilisation des graphiques hierarchiques plotly
les valeurs numeriques sont moyennees lors de l'aggregation
}
