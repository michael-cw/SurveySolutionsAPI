#' Survey Solutions API package
#'
#' This is a first version of an R package, to access the Survey Solutions RESTful API,
#' it allows you to export data/paradata, manipulate users and questionnaires, as well as
#' to create and manipulate assignments directly out of R.
#'
#' The package is not on CRAN yet, so you have to install it by using.
#'
#' If you want to contribute or have suggestions, please send an email to the Survey Solutions
#' team
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
    "UpdatedAtUtc", "InterviewDuration"
  ))
}
