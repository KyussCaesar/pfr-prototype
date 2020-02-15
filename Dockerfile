FROM rocker/verse:3.6.2

WORKDIR /home/rstudio

COPY . .

RUN R --args --bootstrap-packrat

EXPOSE 8080
CMD ["R", "--slave", "-e", "rmarkdown::run('app.Rmd', shiny_args = list(host = '0.0.0.0', port = 8080))"]
