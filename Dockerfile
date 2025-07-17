FROM rocker/shiny:latest

# Installer remotes (utile pour lire DESCRIPTION)
RUN R -e "install.packages('remotes', repos = 'https://cloud.r-project.org')"

# Copier le code complet
COPY . /app
WORKDIR /app

# Installer les dépendances automatiquement
RUN R -e "remotes::install_deps(dependencies = TRUE)"

# Lancer l'app
CMD ["R", "-e", "shiny::runApp('/app')"]
