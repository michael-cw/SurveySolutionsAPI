#' Survey Solutions API call for Summary Tables
#'
#' Returns summary tables for individual questions. If no responses had been provided, an empty table will be returned
#'
#'
#' @param server Survey Solutions server address
#' @param apiUser Survey Solutions API user
#' @param apiPass Survey Solutions API password
#' @param workspace server workspace, if nothing provided, defaults to primary
#' @param token If Survey Solutions server token is provided \emph{apiUser} and \emph{apiPass} will be ignored
#' @param questID only assignments for \emph{QuestionnaireId} are returned, requires \code{version} being not NULL
#' @param version version of the questionnaire, only required with \code{questID}
#' @param qQuest provide \emph{QuestionnaireId} and \emph{version} to receive all questions and responses for a specific questionnaire
#' @param byTeam should the table contain reports by team
#' @export
#'

suso_get_stats <- function(server = suso_get_api_key("susoServer"), apiUser = suso_get_api_key("susoUser"),
                           apiPass = suso_get_api_key("susoPass"),
                           workspace = NULL,
                           token = NULL,
                           questID = "", version = "", qQuest = "", byTeam = TRUE) {

    ## workspace default
    workspace<-.ws_default(ws = workspace)
    # Csv structure is exported
    qExp <- "Csv"
    ## Clear questionnaire ID
    questID <- str_remove_all(questID, "-")
    ## Define server
    server<-parse_url(server)
    server$scheme <- "https"
    ## Define the api
    server$path<-file.path(workspace, "api", "v1", "statistics")
    ## Authentication
    auth<-authenticate(apiUser, apiPass, type = "basic")
    ## Define tempfile for download
    aJsonFile <- tempfile(fileext = paste0(".", tolower(qExp)))

    ## Define queries
    server$query<-list(query.questionnaireId = questID,
                       query.version = version,
                       query.question = qQuest,
                       query.exportType = qExp,
                       query.pivot = "false",
                       query.expandTeams = ifelse(byTeam,"true","false"))
    print(build_url(server))
    test_detail <- GET(url = build_url(server), auth)
    check_response(test_detail)
    writeBin(test_detail$content, aJsonFile)
    # also empty file is returned
    test_json <- data.table(read_csv(aJsonFile))
    return(test_json)
}

#' Survey Solutions API call for questions and responses from single questionnaire
#'
#' Returns all questions for single questionnaire (ONLY if they contain responses), if you require all questions
#' from any questionnaire on the server, then you have to use \code{suso_getQuestDetails(...,
#' operation.type = "structure")}
#'
#' @param server Survey Solutions server address
#' @param apiUser Survey Solutions API user
#' @param apiPass Survey Solutions API password
#' @param workspace server workspace, if nothing provided, defaults to primary
#' @param token If Survey Solutions server token is provided \emph{apiUser} and \emph{apiPass} will be ignored
#' @param questID only assignments for \emph{QuestionnaireId} are returned, requires \code{version} being not NULL
#' @param version version of the questionnaire, only required with \code{questID}
#'
#'
#' @export
#'

suso_getQuestionsQuestionnaire <- function(server = suso_get_api_key("susoServer"), apiUser = suso_get_api_key("susoUser"), apiPass = suso_get_api_key("susoPass"),
                                           workspace = NULL,
                                           token = NULL,
                                           questID = NULL, version = NULL) {
    ## stop if input is missing
    if (is.null(questID) | is.null(version)) stop("Missing Inputs")
    ## Clear questionnaire ID
    questID <- str_remove_all(questID, "-")
    ## Define server
    server<-parse_url(server)
    server$scheme <- "https"
    ## Define the api
    server$path<-file.path(workspace,"api", "v1", "statistics", "questions")
    ## Authentication
    auth<-authenticate(apiUser, apiPass, type = "basic")
    ## Define tempfile for download
    aJsonFile <- tempfile(fileext = ".json")

    ## Define queries
    server$query<-list(questionnaireId = questID,
                       version = version)
    test_detail <- GET(url = build_url(server), auth)
    check_response(test_detail)

    writeBin(content(test_detail, "raw"), aJsonFile)
    test_json <- data.table(fromJSON(aJsonFile))
    return(test_json)
}











