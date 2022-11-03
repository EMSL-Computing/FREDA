# FREDA
<!-- badges: start -->
[![tests](https://github.com/EMSL-computing/FREDA/actions/workflows/test-app.yaml/badge.svg)](https://github.com/EMSL-computing/FREDA/actions/workflows/test-app.yaml)
<!-- badges: end -->

***

The FT-MS R Exploratory Data Analysis (FREDA) tool is designed to allow users upload and explore data generated from Fourier transform (FT) mass spectrometry (MS) instruments. Current capabilities include:

* Upload data from .csv file  
* Assign samples to named groups
* Calculate values associated with observed compounds (e.g. Kendrick Mass, elemental ratios e.g. O:C)
* Filter the compounds based on mass, number of times observed, presence of molecular formulae, or custom filters
* Remove individual samples
* Create van Krevelen, Kendrick, density, custom scatter, and principal coordinate plots for:  
  * single samples
  * multiple samples
  * comparison of two samples
  * comparisons of two groups of samples
* Download a preprocessing summary report, .csv data summaries, and plots in .pdf .jpeg and .png formats

***

#### **Recent Updates:**
  v1.0.4
  * *Linked plots sub-tab of the Visualize tab.  Interactively compare scatter or single sample histogram plots.*
  * *Map peaks to values in the Kegg and Metacyc databases.*
  
***

### Running the app locally:

#### 1.  Using R/Rstudio/Shiny
Install the required packages.  You can do this either by inspecting the global.R file and installing the appropriate packages, or by using `renv`.

To install package with `renv`, first `install.packages("renv")`.  Then call renv::restore().  This will install all packages contained in the renv.lock file.  See the [renv website](https://rstudio.github.io/renv/articles/renv.html) for more details.
Then simply call shiny::runApp()

**Pulling from minio**

FREDA has the ability to pull files from minio, currently only for use in retrieving core-ms output.  You must provide a config that contains several connection parameters and put it at cfg/minio_config.yml

See the example file cfg/minio_config_example.yml

To test pulling from minio locally, you must run a local minio docker container.  With docker installed on your machine,
run the following:

`docker pull minio/minio`  
`docker run -d -p 9000:9000 --name minio-map minio/minio server /data`

Now navigate to http://localhost:9000/, which will display a UI where you can create folders and upload files.  As an
example, do the following:

1.  Create a folder in the minio UI (call it test_folder, for example) and put a couple csv files in it.
2.  Launch FREDA from Rstudio.  
3.  Nagivate to wherever FREDA is being served at, adding /?corems-prefix=test_folder to the url.

FREDA will attempt to read all the files in the minio folder `test_folder` and load them into the reactiveValue 
corems_samples.

#### 2.  Using docker:

Either build the container as described in the development section, or pull it from pnnl artifactory if you have access:
`docker login docker.artifactory.pnnl.gov`
`docker pull docker.artifactory.pnnl.gov/mscviz/freda:latest`

Then run the docker container:  `docker run -p 3838:3838 docker.artifactory.pnnl.gov/mscviz/freda:latest`  

If you are pulling files from minio, you must mount a minio config to `/srv/shiny-server/FREDA/cfg/minio_config.yml`:

`docker run -p 3838:3838 -v /path/to/config.yml:/srv/shiny-server/FREDA/cfg/minio_config.yml docker.artifactory.pnnl.gov/mscviz/freda:latest`  

... and navigate to https://127.0.0.1:3838 in your web browser.

***

### **Development**

#### **Dockerfiles**

We build a 'base' container which has all the system libraries and R packages installed, and then build a container on top of it that simply copies the app source code and exposes the correct port.  There are two Dockerfiles, and two corresponding .dockerignore files.

**To build the base container**, you must provide a github PAT AND gitlab PAT in order to install packages from private git repos to which you have access; currently these include mapDataAccess, KeggData and MetaCycData. Generate a personal access token according to:  https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/creating-a-personal-access-token for github and https://docs.gitlab.com/ee/user/profile/personal_access_tokens.html for gitlab.  Put these tokens in a file next to the Dockerfile, say `.mysecrets`.  It should look like:

```
GITLAB_PAT=<your gitlab pat>  
GITHUB_PAT=<your github pat>
```

Now, replacing &lt;base tag&gt; with whatever version, run:  
`docker build -f Dockerfile-base --secret id=access_tokens,src=.mysecrets -t docker.artifactory.pnnl.gov/mscviz/freda/base:<base tag> .`

**To build the 'top' container**:  
Simply make sure Dockerfile refers to the correct base container if you have updated any dependencies (rebuilt the base container) and run:  
`docker build -t docker.artifactory.pnnl.gov/mscviz/freda:<top tag> .`

If all is well, push new containers to the registry:  
`docker push docker.artifactory.pnnl.gov/mscviz/freda/base:<base tag>`  
`docker push docker.artifactory.pnnl.gov/mscviz/freda:<top tag>`

#### **Dependencies**

We use [renv](https://rstudio.github.io/renv/articles/renv.html) to track dependencies.  The renv.lock file contains a list of dependencies and various details about them.  We use renv to manage the details about dependencies.  When updating the lockfile, we will do the following:

1.  Set renv to only install sub-dependencies in the "Depends" field of installed packages. `renv::settings$package.dependency.fields("Depends")`.  This should get recorded in ./renv/settings.dcf so you only have to do it once.
2.  Snapshot only packages mentioned in the project, as well as any packages mentioned in their "Depends" field by calling `renv::snapshot(type="implicit")`

Certain dependencies are forced to be recognized by renv without being explicitly loaded in the app by adding `library(somepackage)` to `renv_dependencies.R`

#### **Misc**

**Long text**:  Long tooltips or info text should go in the `ttip_text` object or other global objects in `static_objects.R` and then referenced in the app to keep code tidy.

**Developer buttons**:  It is often useful to get access to the console when debugging changes.  To activate developer buttons, set options("shiny.testmode" = TRUE) before running the app.  It helps to put this command in your local .Rprofile.  At least one button should appear floating in the corner of the screen, which when clicked, will give access to the R console for debugging while the app is running.
