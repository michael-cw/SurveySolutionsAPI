#' Survey Solutions API call to retrieve all interviews for a specific questionnaire
#'
#' Returns all interviews for the specified questionnaire and the selected status.
#'
#'
#' @param server Survey Solutions server address
#' @param apiUser Survey Solutions API user
#' @param apiPass Survey Solutions API password
#' @param workspace server workspace, if nothing provided, defaults to primary
#' @param token If Survey Solutions server token is provided \emph{apiUser} and \emph{apiPass} will be ignored
#' @param questID your Survey Solutions \emph{QuestionnaireId}. Retrieve a list of questionnaires by executing \code{suso_getQuestDetails}
#' @param version version of the questionnaire
#' @param workStatus define which statuses the file should inlude (i.e. \emph{Restored,Created,SupervisorAssigned,InterviewerAssigned,
#' RejectedBySupervisor,ReadyForInterview,
#' SentToCapi,Restarted,Completed,ApprovedBySupervisor,
#' RejectedByHeadquarters,ApprovedByHeadquarters,Deleted}), if NULL only completed interviews will be shown.
#'
#'
#' @export
#' @importFrom lubridate as_datetime

suso_getAllInterviewQuestionnaire <- function(server= suso_get_api_key("susoServer"),
                                              apiUser=suso_get_api_key("susoUser"),
                                              apiPass=suso_get_api_key("susoPass"),
                                              workspace = NULL,
                                              token = NULL,
                                              questID = "",
                                              version = 1,
                                              workStatus = "Completed") {
    ######################################
    # Check arguments
    #  - workStatus
    margs<-suso_getQuestDetails(operation.type = "statuses", workspace = workspace)
    if(!is.null(workStatus)) {
        workStatus<-match.arg(workStatus, margs)
    } else {
        workStatus<-"Completed"
    }

    ## workspace default
    workspace<-.ws_default(ws = workspace)
    ## Define API
    server<-httr::parse_url((server))
    server$scheme<-"https"
    server$path<-file.path(workspace,"api", "v1", "interviews")
    server$query<-list(questionnaireId = questID,
                       questionnaireVersion = version,
                       status = workStatus,
                       pageSize = 10,
                       page = 1)

    ## authentication
    auth<-authenticate(apiUser, apiPass, type = "basic")
    ## build url
    server<-build_url(server)
    ## request
    test_detail <- GET(url = server,
                       auth, progress())
    check_response(test_detail)

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
            test_detail <- GET(url = modify_url(server, query = c(server$query, page = i)),
                               auth, progress())
            check_response(test_detail)
            aJsonFile <- tempfile()
            writeBin(content(test_detail, "raw"), aJsonFile)
            test_json_tmp <- fromJSON(aJsonFile)
            test_json$Interviews <- rbind(test_json$Interviews, test_json_tmp$Interviews)
        }
    }
    # Only data.table of interviews is returned
    test_json<-data.table(test_json$Interviews)
    # Set date time to utc with lubridate
    test_json[,LastEntryDate:=as_datetime(LastEntryDate)][]
    return(test_json)
}

#' Survey Solutions API call to retrieve all answers for a specific interview
#'
#' Returns all responses for a specific interview
#'
#' @param server Survey Solutions server address
#' @param apiUser Survey Solutions API user
#' @param apiPass Survey Solutions API password
#' @param workspace server workspace, if nothing provided, defaults to primary
#' @param token If Survey Solutions server token is provided \emph{apiUser} and \emph{apiPass} will be ignored
#' @param intID the \emph{InterviewId} of the interview. To get a list of all interview for a specific questionnaire, execute \code{suso_getAllInterviewQuestionnaire}
#'
#'
#' @export
#'
suso_getAllAnswerInterview <- function(server= suso_get_api_key("susoServer"),
                                       apiUser=suso_get_api_key("susoUser"),
                                       apiPass=suso_get_api_key("susoPass"),
                                       workspace = NULL,
                                       token = NULL,
                                       intID = "") {
    ## workspace default
    workspace<-.ws_default(ws = workspace)
    ## Define API
    server<-httr::parse_url(paste0(server))
    server$scheme<-"https"
    server$path<-file.path(workspace,"api", "v1", "interviews", intID)
    server<-build_url(server)
    ## authentication
    auth<-authenticate(apiUser, apiPass, type = "basic")
    ## Call
    test_detail <- GET(url = server, auth)
    check_response(test_detail)
    aJsonFile <- tempfile()
    writeBin(content(test_detail, "raw"), aJsonFile)
    test_json <- fromJSON(aJsonFile)
    # Export only answers: Not data.table!
    test_json<-data.table(test_json$Answers)
    return(test_json)
}

#' Get all history for a specific interview
#'
#' @param server Survey Solutions server address
#' @param apiUser Survey Solutions API user
#' @param apiPass Survey Solutions API password
#' @param workspace server workspace, if nothing provided, defaults to primary
#' @param token If Survey Solutions server token is provided \emph{apiUser} and \emph{apiPass} will be ignored
#' @param intID the \emph{InterviewId} of the interview.
#'
#' @export
suso_getAllHistoryInterview <- function(server= suso_get_api_key("susoServer"),
                                        apiUser=suso_get_api_key("susoUser"),
                                        apiPass=suso_get_api_key("susoPass"),
                                        workspace = NULL,
                                        token = NULL,
                                        intID = "") {
    ## workspace default
    workspace<-.ws_default(ws = workspace)
    ## Define API
    server<-httr::parse_url(paste0(server))
    server$scheme<-"https"
    server$path<-file.path(workspace,"api", "v1", "interviews", intID, "history")
    ## authentication
    auth<-authenticate(apiUser, apiPass, type = "basic")

    ## Call
    test_detail <- GET(url = build_url(server), auth)
    check_response(test_detail)
    aJsonFile <- tempfile()
    writeBin(content(test_detail, "raw"), aJsonFile)
    test_json <- fromJSON(aJsonFile)
    # Export only records
    test_json<-data.table(test_json$Records)
    # Set date time to utc with lubridate
    test_json[,Timestamp:=as_datetime(Timestamp)][]
    return(test_json)
}


#' Get statistics for interview
#'
#' @param server Survey Solutions server address
#' @param apiUser Survey Solutions API user
#' @param apiPass Survey Solutions API password
#' @param workspace server workspace, if nothing provided, defaults to primary
#' @param token If Survey Solutions server token is provided \emph{apiUser} and \emph{apiPass} will be ignored
#' @param intID a single or multiple \emph{InterviewId}.
#'
#' @export



suso_get_stats_interview<-function(server= suso_get_api_key("susoServer"),
                                   apiUser=suso_get_api_key("susoUser"),
                                   apiPass=suso_get_api_key("susoPass"),
                                   workspace = NULL,
                                   token = NULL,
                                   intID = "") {
    ## workspace default
    workspace<-.ws_default(ws = workspace)
    ## Define API
    server<-httr::parse_url(paste0(server))
    server$scheme<-"https"
    ## authentication
    auth<-authenticate(apiUser, apiPass, type = "basic")

    tj<-list()
    for(id in intID){
        server$path<-NULL
        server$path<-file.path(workspace,"api", "v1", "interviews", id, "stats")
        ## Call
        test_detail <- GET(url = build_url(server), auth)
        if (status_code(test_detail)!=200){
            warning(paste0("No data for interview: " , id,". Did you specify the correct ID?"), call. = F)
            print(id)
            next()
        }
        aJsonFile <- tempfile()
        writeBin(content(test_detail, "raw"), aJsonFile)
        test_json <- fromJSON(aJsonFile)
        test_json<-as.data.table(t(unlist(test_json)))
        tj[[id]]<-test_json
    }
    tj<-rbindlist(tj, fill = T)
    # to numeric conversion of counts
    names_col<-names(tj)[c(1:9,15:17)]
    for (col in names_col) set(tj, j=col, value=as.numeric(tj[[col]]))
    ## date conversion
    tj[,UpdatedAtUtc:=as_datetime(UpdatedAtUtc)]
    tj[,InterviewDuration:=as.POSIXct(InterviewDuration, format = "%H:%M:%OS")][]
    return(tj)
}

#' Reject interviews either as supervisor or as headquarter. For details please see:
#' \url{https://docs.mysurvey.solutions/headquarters/interviews/survey-workflow/}
#'
#'
#' @param server Survey Solutions server address
#' @param apiUser Survey Solutions API user
#' @param apiPass Survey Solutions API password
#' @param workspace server workspace, if nothing provided, defaults to primary
#' @param token If Survey Solutions server token is provided \emph{apiUser} and \emph{apiPass} will be ignored
#' @param intID the \emph{InterviewId} of the interview.
#' @param HQ if FALSE, reject as supervisor, if TRUE rejected as headquarters
#' @param comment comment which should be sent with the questionnaire
#'
#' @export
#'
suso_patchRejectInterview <- function(server= suso_get_api_key("susoServer"),
                                      apiUser=suso_get_api_key("susoUser"),
                                      apiPass=suso_get_api_key("susoPass"),
                                      workspace = NULL,
                                      token = NULL,
                                      intID = "",
                                      HQ = FALSE,
                                      comment = "Please check errors and re-submit!") {
    ## select reject
    reject<-ifelse(HQ, "hqreject", "reject")
    ## workspace default
    workspace<-.ws_default(ws = workspace)
    ## Define the api
    url <- parse_url(server)
    url$scheme <- "https"
    url$path<-file.path(workspace,"api", "v1", "interviews", intID, reject)
    url$query <- list(comment = comment)
    ## authentication
    auth<-authenticate(apiUser, apiPass, type = "basic")

    server <- build_url(url)
    test_detail <- PATCH(url = server, auth)
    # check_response(test_detail)
    # aJsonFile<-tempfile() writeBin(content(test_detail, 'raw'), aJsonFile) test_json<-fromJSON(aJsonFile)
    return(status_code(test_detail))
}



