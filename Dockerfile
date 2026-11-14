
FROM rocker/shiny:4.4.3

RUN Rscript -e "install.packages(c('shiny', 'ggplot2'), repos='https://cloud.r-project.org')"

RUN rm -rf /srv/shiny-server/*

COPY app.R /srv/shiny-server/app.R

RUN chown -R shiny:shiny /srv/shiny-server

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]
