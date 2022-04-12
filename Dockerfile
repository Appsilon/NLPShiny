FROM ubuntu:latest
ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get clean && apt-get update && apt-get install -y locales
RUN locale-gen en_US.UTF-8
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8

LABEL maintainer="Fernando Roa <froao@unal.edu.co>"

RUN apt-get -y update && apt-get install -y --no-install-recommends \
    sudo \
    python3 \
    build-essential \
    python3-pip \
    python3-setuptools \
    python3-dev \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libxml2-dev \
    libsasl2-dev \
    wget \
    && rm -rf /var/lib/apt/lists/*

RUN apt-get update && apt-get install -y gnupg
RUN apt-get install -y dirmngr apt-transport-https ca-certificates software-properties-common
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9
RUN add-apt-repository 'deb https://cloud.r-project.org/bin/linux/ubuntu focal-cran40/'
RUN apt-get install -y r-base

RUN pip3 install --upgrade pip

ENV PYTHONPATH "${PYTHONPATH}:/app"

ADD requirements.txt .
RUN pip3 install -r requirements.txt
RUN [ "python3", "-c", "import nltk; nltk.download('stopwords')" ]
RUN [ "python3", "-c", "import nltk; nltk.download('punkt')" ]
RUN cp -r /root/nltk_data /usr/local/share/nltk_data

ENV RENV_VERSION 0.15.4
RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"

RUN echo "local(options(shiny.port = 8080, shiny.host = '0.0.0.0'))" > /usr/lib/R/etc/Rprofile.site

RUN apt-get update && apt-get install -y libgdal-dev

RUN addgroup --system app \
    && adduser --system --ingroup app app

WORKDIR /home/app
COPY .Rprofile .Rprofile
COPY renv.lock renv.lock
COPY renv/* renv/
RUN chown app:app -R /home/app
USER app
RUN R -e 'renv::restore()'
COPY . .
EXPOSE 8080
CMD ["R", "-e", "shiny::runApp('/home/app')"]
