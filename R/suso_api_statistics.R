#' Survey Solutions API call for Summary Tables
#'
#' Returns summary tables for individual questions. If no responses had been provided, an empty table will be returned
#'
#'
#' @param server Survey Solutions server address
#' @param apiUser Survey Solutions API user
#' @param apiPass Survey Solutions API password
#' @param questID only assignments for \emph{QuestionnaireId} are returned, requires \code{version} being not NULL
#' @param version version of the questionnaire, only required with \code{questID}
#' @param qQuest provide \emph{QuestionnaireId} and \emph{version} to receive all questions and responses for a specific questionnaire
#'
#' @export
#'
#' @import data.table

suso_get_stats <- function(server = suso_get_api_key("susoServer"), apiUser = suso_get_api_key("susoUser"), apiPass = suso_get_api_key("susoPass"),
    questID = "", version = "", qQuest = "") {

    # Csv structure is exported
    qExp <- "Csv"
    ## Clear questionnaire ID
    questID <- str_remove_all(questID, "-")
    ## Define server
    server<-parse_url(server)
    server$scheme <- "https"
    ## Define the api
    server$path<-file.path("api", "v1", "statistics")
    ## Define tempfile for download
    aJsonFile <- tempfile(fileext = paste0(".", tolower(qExp)))

    ## Define queries
    server$query<-list(query.questionnaireId = questID,
                       query.version = version,
                       query.question = qQuest,
                       query.exportType = qExp,
                       query.pivot = "false",
                       query.expandTeams = "true")
    test_detail <- GET(url = build_url(server), authenticate(apiUser, apiPass, type = "basic"))
    writeBin(test_detail$content, aJsonFile)
    # also empty file is returned
    test_json <- data.table(read_csv(aJsonFile))
    return(test_json)
}

#' Survey Solutions API call for questions and responses from single questionnaire
#'
#' Returns questions for single questionnaire (if they contain responses)
#'
#' @param server Survey Solutions server address
#' @param apiUser Survey Solutions API user
#' @param apiPass Survey Solutions API password
#' @param questID only assignments for \emph{QuestionnaireId} are returned, requires \code{version} being not NULL
#' @param version version of the questionnaire, only required with \code{questID}
#'
#'
#' @export
#'
#' @import data.table
#' @import httr
#' @import jsonlite
#' @import lubridate
#' @import readr
suso_getQuestionsQuestionnaire <- function(server = suso_get_api_key("susoServer"), apiUser = suso_get_api_key("susoUser"), apiPass = suso_get_api_key("susoPass"),
    questID = NULL, version = NULL) {
    ## stop if input is missing
    if (is.null(questID) | is.null(version)) stop("Missing Inputs")
    ## Clear questionnaire ID
    questID <- str_remove_all(questID, "-")
    ## Define server
    server<-parse_url(server)
    server$scheme <- "https"
    ## Define the api
    server$path<-file.path("api", "v1", "statistics", "questions")
    ## Define tempfile for download
    aJsonFile <- tempfile(fileext = ".json")

    ## Define queries
    server$query<-list(questionnaireId = questID,
                       version = version)
    test_detail <- GET(url = build_url(server), authenticate(apiUser, apiPass, type = "basic"))
    writeBin(content(test_detail, "raw"), aJsonFile)
    test_json <- data.table(fromJSON(aJsonFile))
    return(test_json)
}

# a<-getQuestionsQuestionnaire('mcw2.mysurvey.solutions', 'bahamaAPI0202', 'Bahamas2020',
# '420791ebbd1e47868ce4e27b098c34f8', 3)

########################################### EXTRACT table from site

"https://mcw2.mysurvey.solutions/api/ReportDataApi/
HeadquarterSurveysAndStatusesReport?ResponsibleName=&draw=1&
columns[0][data]=questionnaireTitle&columns[0][name]=QuestionnaireTitle&
columns[1][data]=supervisorAssignedCount&columns[1][name]=SupervisorAssignedCount&
columns[2][data]=interviewerAssignedCount&columns[2][name]=InterviewerAssignedCount&
columns[3][data]=completedCount&columns[3][name]=CompletedCount&
columns[4][data]=rejectedBySupervisorCount&columns[4][name]=RejectedBySupervisorCount&
columns[5][data]=approvedBySupervisorCount&columns[5][name]=ApprovedBySupervisorCount&
columns[6][data]=rejectedByHeadquartersCount&columns[6][name]=RejectedByHeadquartersCount&
columns[7][data]=approvedByHeadquartersCount&columns[7][name]=ApprovedByHeadquartersCount&
columns[8][data]=totalCount&columns[8][name]=TotalCount&order[0][column]=0&order[0][dir]=
asc&start=0&length=20&search[value]=&search[regex]=false&exportType=csv"
## teams and stauses
"https://mcw2.mysurvey.solutions/api/ReportDataApi/HeadquarterSupervisorsAndStatusesReport?
draw=6&order[0][column]=0&order[0][dir]=asc&
order[0][name]=Responsible&start=0&length=20&search[value]=&
search[regex]=false&templateId=420791ebbd1e47868ce4e27b098c34f8&
templateVersion=3&exportType=csv"















