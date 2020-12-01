FROM rocker/shiny:3.6.0

RUN apt-get update -qq && apt-get install -y \
  git-core \
  libssl-dev \
  libcurl4-gnutls-dev \
  libmagick++-dev \
  libv8-dev
  
################################################
# This block copied from phantomjs docker image:
# https://hub.docker.com/r/wernight/phantomjs/dockerfile
ARG PHANTOM_JS_VERSION
ENV PHANTOM_JS_VERSION ${PHANTOM_JS_VERSION:-2.1.1-linux-x86_64}

# Install runtime dependencies
RUN apt-get update \
 && apt-get install -y --no-install-recommends \
        ca-certificates \
        bzip2 \
        libfontconfig \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*

# Install official PhantomJS release
# Install dumb-init (to handle PID 1 correctly).
# https://github.com/Yelp/dumb-init
# Runs as non-root user.
# Cleans up.
RUN set -x  \
 && apt-get update \
 && apt-get install -y --no-install-recommends \
        curl \
 && mkdir /tmp/phantomjs \
 && curl -L https://bitbucket.org/ariya/phantomjs/downloads/phantomjs-${PHANTOM_JS_VERSION}.tar.bz2 \
        | tar -xj --strip-components=1 -C /tmp/phantomjs \
 && mv /tmp/phantomjs/bin/phantomjs /usr/local/bin \
# && curl -Lo /tmp/dumb-init.deb https://github.com/Yelp/dumb-init/releases/download/v1.1.3/dumb-init_1.1.3_amd64.deb \
# && dpkg -i /tmp/dumb-init.deb \
 && apt-get purge --auto-remove -y \
        curl \
 && apt-get clean \
 && rm -rf /tmp/* /var/lib/apt/lists/* \
 && useradd --system --uid 52379 -m --shell /usr/sbin/nologin phantomjs \
 && su phantomjs -s /bin/sh -c "phantomjs --version"

# End phantomjs block
################################################  

########## Plotly depedencies ##############
# https://github.com/plotly/orca/issues/150
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        wget \
        xvfb \
        xauth \
        libgtk2.0-0 \
        libxtst6 \
        libxss1 \
        libgconf-2-4 \
        libnss3 \
        libasound2 && \
    mkdir -p /opt/orca && \
    cd /opt/orca && \
    wget https://github.com/plotly/orca/releases/download/v1.2.1/orca-1.2.1-x86_64.AppImage && \
    chmod +x orca-1.2.1-x86_64.AppImage && \
    ./orca-1.2.1-x86_64.AppImage --appimage-extract && \
    rm orca-1.2.1-x86_64.AppImage && \
    printf '#!/bin/bash \nxvfb-run --auto-servernum --server-args "-screen 0 640x480x24" /opt/orca/squashfs-root/app/orca "$@"' > /usr/bin/orca && \
    chmod +x /usr/bin/orca

###########################################

COPY packrat.lock packrat.opts /srv/shiny-server/FREDA/packrat/

RUN R -e "install.packages('https://cran.r-project.org/src/contrib/packrat_0.5.0.tar.gz', repos = NULL, type = 'source')" \
	-e "install.packages('Rcpp')"
	
RUN R -e "packrat::init('/srv/shiny-server/FREDA')" \
	-e "packrat::restore('/srv/shiny-server/FREDA')" \
	-e "install.packages('devtools')" \
	-e "devtools::install_github('EMSL-Computing/ftmsRanalysis')"

ARG keggpath
ARG metacycpath

COPY . /srv/shiny-server/FREDA

RUN R -e "install.packages('/srv/shiny-server/FREDA/$keggpath', type = 'source', repos = NULL)"
RUN R -e "install.packages('/srv/shiny-server/FREDA/$metacycpath', type = 'source', repos = NULL)"

RUN rm /srv/shiny-server/FREDA/$keggpath
RUN rm /srv/shiny-server/FREDA/$metacycpath

COPY shiny-server.conf /etc/shiny-server/shiny-server.conf
RUN chown -R shiny:shiny /srv/shiny-server/FREDA /opt/orca/squashfs-root
