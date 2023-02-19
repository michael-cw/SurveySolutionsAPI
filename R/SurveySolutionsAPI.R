#' Survey Solutions API package
#'
#' This is the updated version of the comprehensive Survey Solutions R API package,
#' \url{https://docs.mysurvey.solutions/headquarters/api/api-r-package/}
#' It allows you to export data/paradata collected through CAWI, CATI or CAPI operations,
#' manipulate users and questionnaires, create assignemnts, get information about the survey progress etc..
#' All directly out of R. However it is more than just a simple wraper around the Survey Solutions RESTfull API,
#' which one can easily address oneself through using i.e. the excellent \emph{httr package} which
#' also constitutes the basis for this package. The main intention though is to be integrated
#' into your own data collection workflow, either through a shiny application or just through a simple script.
#' This allows you to build your own, customized userinterfaces for your Survey Solutions data collection operations, no
#' matter if you are dealing with a small scale impact evaluation or a full scale census.
#'
#' Since such data collection operations very often come with high data volumes, the package also returns
#' most of the server responses as data.tables \url{https://cran.r-project.org/web/packages/data.table/}
#' which is effecient with high volume data sets, you can then either continue processing in the datatable environment
#' or in case you prefer the tidy approach to data processing continue with the dtlyr package
#' \url{https://github.com/tidyverse/dtplyr}
#'
#'
#' The package is not on CRAN yet, but is planned to be submitted after the next release, which will also include support for
#' shape files and tpk basemaps.
#'
#' If you want to contribute or have suggestions, please send an email to the Survey Solutions
#' team.
#'
#' @docType package
#' @name SurveySolutionsAPI
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "3.3.0")  {
  utils::globalVariables(c(
    ".", "i",
    "resp_time",
    "action",
    "time",
    "interview__id",
    "breaks",
    "test_detail",
    "UPLOADdataCHECK",
    "QuestionnaireIdentity",
    "Variable",
    "action",
    "setNames",
    "incProgress",
    "rid",
    "duration",
    "durationNOBREAK",
    "m_resp_time_varTRIM",
    "var",
    "start",
    "startHour",
    "responsible",
    "m_resp_time_var",
    "m_diff_dev",
    "response",
    "var_resp",
    "response1",
    "response2",
    "lat",
    "long",
    "count",
    "counter",
    "handlers",
    "Timestamp",
    "LastEntryDate",
    "CreationDate",
    "document.id",
    "var_resp", "V1", "V2", "V3",
    "response",
    "responsible",
    "role",
    "dateTime","tz","wDAY","mDAY","MONTH", "WEEK",
    "UpdatedAtUtc", "InterviewDuration", "intID",
    "VariableName", "Expression", "Message", "Severity",
    "QuestionnaireId", "StartDate", "CompleteDate", "pid",
    "ExportType", "Time", "importDateUtc", "CreatedAtUtc",
    "ReceivedByTabletAtUtc"
  ))
}
