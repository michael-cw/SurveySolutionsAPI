# SurveySolutionsAPI

This is the updated version of the comprehensive Survey Solutions R API package, https://docs.mysurvey.solutions/headquarters/api/api-r-package/ It allows you to export data/paradata collected through CAWI, CATI or CAPI operations, manipulate users and questionnaires, create assignments, get information about the survey progress etc.. All directly out of R. However it is more than just a simple wrapper around the Survey Solutions RESTful API, which one can easily address through using i.e. the excellent httr package which also constitutes the basis for this package. The main intention of this package though is to be integrated into your own data collection workflow, either through a shiny application or just through a simple script. This allows you to build your own workflows with your own customized user interfaces for your Survey Solutions data collection operations, no matter if you are dealing with a small scale impact evaluation or a large scale census.

Specific changes:

- Work space inclusion
- Preparation for token authentication (not active yet)
- New function: suso_Workspaces()

The package is not on CRAN yet, so you have to install it by using

```
devtools::install_github("michael-cw/SurveySolutionsAPI")

```
or in case you also want the vignettes include, please use:
```
devtools::install_github("michael-cw/SurveySolutionsAPI", build_vignettes = T)

```
Besides the API functions, the package also contains a convenience function to store your credentials during an ongoing session. To do this, execute
```
suso_set_key(suso_server = "https://xxx.mysurvey.solutions", suso_user = {api_username}, suso_pass = {api_userpass})
```
After setting your credentials, there's no need to re-type it again with every function call.

Some of the functions (in particular those with long running process) also contain parameters, to facilitate an integration into a shiny application.

As the package is under active development, some variables may change and new functions may be added. Stop by from time to time, to see any updates.


To find information on the World Bank's Survey Solutions CASS have a look on these pages:
- Survey Solutions Support Page: https://support.mysurvey.solutions/
- Survey Solutions Server request: https://mysurvey.solutions/

To find information on the API syntax, visit your own servers API documentation or got to:
- https://demo.mysurvey.solutions/primary/apidocs/index.html#

