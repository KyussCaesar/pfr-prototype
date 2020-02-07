FROM rocker/verse:3.6.0

COPY app.Rmd app.Rmd

EXPOSE 8080
CMD ["R", "--vanilla", "--slave", "-e", "rmarkdown::run('app.Rmd', shiny_args = list(host = '0.0.0.0', port = 8080))"]
