FROM rocker/shiny:latest

RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev

COPY . /app
WORKDIR /app

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/app')"]
