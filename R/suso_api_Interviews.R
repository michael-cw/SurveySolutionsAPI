#' Survey Solutions API call to retrieve all interviews for a specific questionnaire
#'
#' Returns all interviews for the specific questionnaire.
#'
#' @param questID your Survey Solutions \emph{QuestionnaireId}. Retrieve a list of questionnaires by executing \code{suso_getQuestDetails}
#' @param version version of the questionnaire
#'
#'
#' @export
suso_getAllInterviewQuestionnaire <- function(server= suso_get_api_key("susoServer"),
                                              apiUser=suso_get_api_key("susoUser"),
                                              apiPass=suso_get_api_key("susoPass"),
                                              questID = "e4de521a-6e32-4ab0-93c5-1fa4e11dc12f", version = 2, status = NULL) {
    server <- ifelse(str_count(server, "https://") == 1, server, paste0("https://", server))
    ## Define the api
    server = paste0(server, "/api/v1/interviews?")
    # qId<-paste(paste(questID, version, sep = '$'), status, sep = '/')
    qId <- paste0("questionnaireId=", questID, "&questionnaireVersion=", version)
    if (!is.null(status))
        qId <- paste0(qId, "&status=", status)
    tmp_qId <- paste0(qId, "&page=1")
    test_detail <- GET(url = paste0(server, tmp_qId), authenticate(apiUser, apiPass, type = "basic"), progress())

    aJsonFile <- tempfile()
    writeBin(content(test_detail, "raw"), aJsonFile)
    test_json <- fromJSON(aJsonFile)
    qTot <- test_json$TotalCount
    ## 2. Check if more than 40, append rest
    if (qTot > nrow(test_json$Interviews)) {
        qTotRest <- qTot
        repCalls <- ceiling(qTotRest/nrow(test_json$Interviews))
        for (i in 2:repCalls) {
            tmp_qId <- paste0(qId, "&page=", i)
            test_detail <- GET(url = paste0(server, tmp_qId), authenticate(apiUser, apiPass, type = "basic"), progress())
            aJsonFile <- tempfile()
            writeBin(content(test_detail, "raw"), aJsonFile)
            test_json_tmp <- fromJSON(aJsonFile)
            test_json$Interviews <- rbind(test_json$Interviews, test_json_tmp$Interviews)
        }
    }
    return(test_json)
}

#' Survey Solutions API call to retrieve all answers for a specific interview
#'
#' Returns all responses for a specific interview
#'
#' @param intID the \emph{InterviewId} of the interview. To get a list of all interview for a specific questionnaire, execute \code{suso_getAllInterviewQuestionnaire}
#'
#'
#' @export
suso_getAllAnswerInterview <- function(server= suso_get_api_key("susoServer"),
                                       apiUser=suso_get_api_key("susoUser"),
                                       apiPass=suso_get_api_key("susoPass"),
                                       intID = "") {
    server <- ifelse(str_count(server, "https://") == 1, server, paste0("https://", server))
    ## Define the api
    server = paste0(server, "/api/v1/interviews/")
    test_detail <- GET(url = paste0(server, intID), authenticate(apiUser, apiPass, type = "basic"))

    aJsonFile <- tempfile()
    writeBin(content(test_detail, "raw"), aJsonFile)
    test_json <- fromJSON(aJsonFile)
    return(test_json)

}

#' Get all history for a specific interview
#'
#'
#' @export
suso_getAllHistoryInterview <- function(server= suso_get_api_key("susoServer"),
                                   apiUser=suso_get_api_key("susoUser"),
                                   apiPass=suso_get_api_key("susoPass"),
                                   intID = "") {
    ## Load the libraries
    server <- ifelse(str_count(server, "https://") == 1, server, paste0("https://", server))
    ## Define the api
    server = paste0(server, "/api/v1/interviews")
    test_detail <- GET(url = paste(server, intID, "history", sep = "/"), authenticate(apiUser, apiPass, type = "basic"))

    aJsonFile <- tempfile()
    writeBin(content(test_detail, "raw"), aJsonFile)
    test_json <- fromJSON(aJsonFile)
    return(test_json)
}

#' Reject interview
#'
#'
#' @export
suso_patchRejectInterview <- function(server= suso_get_api_key("susoServer"),
                                      apiUser=suso_get_api_key("susoUser"),
                                      apiPass=suso_get_api_key("susoPass"),
                                      intID = "", comment = "Please check errors and re-submit!") {
    ## Load the libraries
    server <- ifelse(str_count(server, "https://") == 1, server, paste0("https://", server))
    ## Define the api
    server = paste0(server)
    url <- parse_url(server)
    url$scheme <- "https"
    url$path <- c("/api/v1/interviews", intID, "reject")
    url$query <- list(comment = comment)
    server <- build_url(url)
    print(server)
    test_detail <- PATCH(url = server, authenticate(apiUser, apiPass, type = "basic"))

    # aJsonFile<-tempfile() writeBin(content(test_detail, 'raw'), aJsonFile) test_json<-fromJSON(aJsonFile)
    return(test_detail)
}
