FROM rocker/binder:4.5.0
LABEL maintainer='Your Name <your_email@somewhere.com>'
USER root
COPY . ${HOME}
RUN chown -R ${NB_USER} ${HOME}
USER ${NB_USER}

RUN wget https://github.com/<USERNAME>/<REPO>/raw/master/DESCRIPTION && R --vanilla -s -e "options(repos = list(CRAN = 'http://cran.rstudio.com'));  install.packages('renv'); renv::restore()"

RUN rm DESCRIPTION.1; exit 0
