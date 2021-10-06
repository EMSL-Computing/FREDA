FROM docker.artifactory.pnnl.gov/mscviz/freda/base

# All app source/resources
COPY . /srv/shiny-server/FREDA

# shiny server configuration
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf

# App is run as user shiny, need to change ownership
RUN chown -R shiny:shiny /srv/shiny-server/FREDA /opt/orca/squashfs-root
