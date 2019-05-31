# Instructions for using the FREDA Docker container

0. Be using a Mac, not Windows. :-)
1. Clone the FREDA repository from github at: https://github.com/lmbramer/FREDA. Check out the docker branch.
2. Install Docker Desktop: https://hub.docker.com/
3. At the bash prompt in the directory where FREDA was cloned execute these commands:
    * docker pull rocker/shiny-verse
    * docker build -t freda:latest .
    * docker run -p 3838:3838 -u shiny -e APPLICATION_LOGS_TO_STDOUT=false -v /Users/d3l348/tmp:/var/log/shiny-server freda
4. In a browser navigate to http://localhost:3838/FREDA/
5. To halt the Docker container and stop the FREDA app, hit Ctrl-C in the Terminal window. (Yes Ctrl not Cmd.)