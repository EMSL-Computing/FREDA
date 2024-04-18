# Builds off the FREDA base container that contains all dependencies for the
# app.  Mostly just copies in app source code and server configuration.

# Change this when we bump versions, or if you have some test version of the
# base container you can specify --build-arg base_tag=<yourtag> in docker 
# build.
ARG base_tag=docker.artifactory.pnnl.gov/mscviz/freda/base:latest

FROM $base_tag

# All app source/resources
COPY . /srv/shiny-server/FREDA

# shiny server configuration
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf

# App is run as user shiny, need to change ownership
RUN chown -R shiny:shiny /srv/shiny-server/FREDA
