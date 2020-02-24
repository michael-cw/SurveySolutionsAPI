#' Survey Solutions API call to retrieve all interviews for a specific questionnaire
#'
#' Returns all interviews for the specific questionnaire.
#'
#'
#' @param server Survey Solutions server address
#' @param apiUser Survey Solutions API user
#' @param apiPass Survey Solutions API password
#' @param questID your Survey Solutions \emph{QuestionnaireId}. Retrieve a list of questionnaires by executing \code{suso_getQuestDetails}
#' @param version version of the questionnaire
#' @param status which status, i.e. completed, ...
#'
#'
#' @export
#'
#' @import data.table
suso_getAllInterviewQuestionnaire <- function(server= suso_get_api_key("susoServer"),
                                              apiUser=suso_get_api_key("susoUser"),
                                              apiPass=suso_get_api_key("susoPass"),
                                              questID = "e4de521a-6e32-4ab0-93c5-1fa4e11dc12f", version = 2, status = NULL) {
    ## Define API
    server<-httr::parse_url(paste0(server))
    server$scheme<-"https"
    server$path<-file.path("api", "v1", "interviews")
    server$query<-list(questionnaireId = questID,
                    questionnaireVersion = version)
    if (!is.null(status))
        server$query<-append(url$query, c(status = status))
    test_detail <- GET(url = modify_url(server, query = c(server$query, page = 1)), authenticate(apiUser, apiPass, type = "basic"), progress())
    print(modify_url(server, query = c(server$query, page = 1)))
    aJsonFile <- tempfile()
    writeBin(content(test_detail, "raw"), aJsonFile)
    test_json <- fromJSON(aJsonFile)
    # stop if empty response
    if (length(test_json$Interviews)==0) stop("No data for this questionnaire. Did you use the correct ID?", call. = F)
    qTot <- test_json$TotalCount
    # 2. Check if more than 40, append rest
    if (qTot > nrow(test_json$Interviews)) {
        qTotRest <- qTot
        repCalls <- ceiling(qTotRest/nrow(test_json$Interviews))
        for (i in 2:repCalls) {
            test_detail <- GET(url = modify_url(server, query = c(server$query, page = i)), authenticate(apiUser, apiPass, type = "basic"), progress())
            aJsonFile <- tempfile()
            writeBin(content(test_detail, "raw"), aJsonFile)
            test_json_tmp <- fromJSON(aJsonFile)
            test_json$Interviews <- rbind(test_json$Interviews, test_json_tmp$Interviews)
        }
    }
    # Only data.table of interviews is returned
    test_json<-data.table(test_json$Interviews)
    # Set date time to utc with lubridate
    test_json[,LastEntryDate:=as_datetime(LastEntryDate)]
    return(test_json)
}

#' Survey Solutions API call to retrieve all answers for a specific interview
#'
#' Returns all responses for a specific interview
#'
#' @param server Survey Solutions server address
#' @param apiUser Survey Solutions API user
#' @param apiPass Survey Solutions API password
#' @param intID the \emph{InterviewId} of the interview. To get a list of all interview for a specific questionnaire, execute \code{suso_getAllInterviewQuestionnaire}
#'
#'
#' @export
#'
#' @import data.table
suso_getAllAnswerInterview <- function(server= suso_get_api_key("susoServer"),
                                       apiUser=suso_get_api_key("susoUser"),
                                       apiPass=suso_get_api_key("susoPass"),
                                       intID = "") {
    ## Define API
    server<-httr::parse_url(paste0(server))
    server$scheme<-"https"
    server$path<-file.path("api", "v1", "interviews", intID)
    ## Call
    test_detail <- GET(url = build_url(server), authenticate(apiUser, apiPass, type = "basic"))
    if (status_code(test_detail)!=200) stop("No data for this interview. Did you specify the correct ID?", call. = F)
    aJsonFile <- tempfile()
    writeBin(content(test_detail, "raw"), aJsonFile)
    test_json <- fromJSON(aJsonFile)
    # Export only answers: Not data.table!
    test_json<-(test_json$Answers)
    return(test_json)
}

#' Get all history for a specific interview
#'
#' @param server Survey Solutions server address
#' @param apiUser Survey Solutions API user
#' @param apiPass Survey Solutions API password
#' @param intID the \emph{InterviewId} of the interview.
#'
#' @export
suso_getAllHistoryInterview <- function(server= suso_get_api_key("susoServer"),
                                   apiUser=suso_get_api_key("susoUser"),
                                   apiPass=suso_get_api_key("susoPass"),
                                   intID = "") {
    ## Define API
    server<-httr::parse_url(paste0(server))
    server$scheme<-"https"
    server$path<-file.path("api", "v1", "interviews", intID, "history")
    ## Call
    test_detail <- GET(url = build_url(server), authenticate(apiUser, apiPass, type = "basic"))
    if (status_code(test_detail)!=200) stop("No data for this interview. Did you specify the correct ID?", call. = F)
    aJsonFile <- tempfile()
    writeBin(content(test_detail, "raw"), aJsonFile)
    test_json <- fromJSON(aJsonFile)
    # Export only records
    test_json<-data.table(test_json$Records)
    # Set date time to utc with lubridate
    test_json[,Timestamp:=as_datetime(Timestamp)]
    return(test_json)
}

#' Reject interview
#'
#' N.B. under development
#'
#' @param server Survey Solutions server address
#' @param apiUser Survey Solutions API user
#' @param apiPass Survey Solutions API password
#' @param intID the \emph{InterviewId} of the interview.
#' @param comment comment which should be sent with the questionnaire
#'
#' @export
#'
#' @import data.table
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
