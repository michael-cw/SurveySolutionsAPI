# SurveySolutionsAPI

This is a first version of an R package, to access the Survey Solutions RESTful API, it allows you to export data/paradata, manipulate users and 
questionnaires, as well as to create and manipulate assignments directly out of R. 

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

Some of the functions (in particular thoses with long running process) also contain parameters, to facilitate an integration into a shiny application.

As the package is under active development, some variables may change and new functions may be added. Stop by from time to time, to see any updates.


To find information on the World Bank's Survey Solutions CASS have a look on these pages:
- Survey Solutions Support Page: https://support.mysurvey.solutions/
- Survey Solutions Server request: https://mysurvey.solutions/

To find information on the API syntax, visit your own servers API documentation or got to:
- https://demo.mysurvey.solutions//apidocs/index#!

