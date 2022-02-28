#' Survey Solutions API call for assignment list
#'
#'
#' \code{suso_get_assignments} calls the Survey Solutions assingment API
#'
#'
#' @param server Survey Solutions server address
#' @param apiUser Survey Solutions API user
#' @param apiPass Survey Solutions API password
#' @param token If Survey Solutions server token is provided \emph{apiUser} and \emph{apiPass} will be ignored
#' @param workspace If workspace name is provide requests are made regarding this specific workspace
#' @param questID only assignments for \emph{QuestionnaireId} are returned,
#' requires \code{version} being not NULL
#' @param version version of the questionnaire, only required with \code{questID}
#' @param AssId if NULL a list of all assignments on the server, if not NULL
#' the assignment details for a specific assignment ID
#' @param responsibleID the ID of the responsible user (Supervisor or Interviewer).
#' Retrieves all assignments for this user.
#' @param order.by determines the column by which the assignment list should be ordered
#' @param operations.type specifies the desired operation, one of assignmentQuantitySettings, history, or recordAudio,
#' if specified, requires also \emph{AssId} to be specified.
#'
#' @export
#'
suso_get_assignments<-function(questID = NULL,
                               server = suso_get_api_key("susoServer"),
                               apiUser = suso_get_api_key("susoUser"),
                               apiPass = suso_get_api_key("susoPass"),
                               token = NULL,
                               workspace = NULL,
                               AssId=NULL,
                               version= NULL,
                               responsibleID = NULL,
                               order.by="",
                               operations.type = c("assignmentQuantitySettings",
                                                   "history",
                                                   "recordAudio")) {
  ## workspace default
  workspace<-.ws_default(ws = workspace)

  ############
  # 1. create variables
  ##  BASE URL
  url<-httr::parse_url(paste0(server))
  url$scheme<-"https"
  url$path<-file.path(workspace,"api", "v1", "assignments")
  ## Authenticate
  auth<-authenticate(apiUser, apiPass, type = "basic")
  ###########
  # 2 Operations

  full_data<-list()
  if (is.null(AssId)) {
    # 2.1. All Assignments, without filtering, and ordered by ID
    url$query<-list(order = order.by, limit = 100)
    test_detail<-GET(url = build_url(url), auth)
    check_response(test_detail)
    aJsonFile<-tempfile()
    writeBin(content(test_detail, "raw"), aJsonFile)
    test_json<-fromJSON(aJsonFile)
    full_data[[1]]<-data.table(test_json$Assignments, key = "Id")


    # ii. get the total and the first number
    tot<-test_json$TotalCount
    first<-test_json$Assignments$Id[1]

    # iii. create a loop to get the rest
    rest<-tot-100
    loop.counter<-ceiling(rest/100)
    for(i in 1:loop.counter){
      offs<-100*i
      url$query<-list(order = order.by, limit = 100, offset = offs)
      test_detail<-GET(url = build_url(url), auth)
      check_response(test_detail)
      aJsonFile<-tempfile()
      writeBin(content(test_detail, "raw"), aJsonFile)
      test_json<-fromJSON(aJsonFile)
      listIndex<-i+1
      full_data[[listIndex]]<-data.table(test_json$Assignments, key = "Id")
    }
    test_json<-rbindlist(full_data)
    return(test_json)

  } else if (!is.null(AssId)) {
    # 2.2. Single Assignment by Assignment ID
    url$path<-file.path(url$path, AssId)
    print(build_url(url))
    test_detail<-GET(url = build_url(url), auth)
    check_response(test_detail)
    aJsonFile<-tempfile()
    writeBin(content(test_detail, "raw"), aJsonFile)
    test_json<-fromJSON(aJsonFile)
    return(test_json)

  } else if (!is.null(responsibleID)){
    # 2.3. All Assignment by responsible ID
    url$query<-list(responsible = responsibleID)
    test_detail<-GET(url = build_url(url), auth)
    check_response(test_detail)
    aJsonFile<-tempfile()
    writeBin(content(test_detail, "raw"), aJsonFile)
    test_json<-fromJSON(aJsonFile)
    full_data[[1]]<-data.table(test_json$Assignments, key = "Id")


    # ii. get the total and the first number
    tot<-test_json$TotalCount
    first<-test_json$Assignments$Id[1]

    # iii. create a loop to get the rest
    rest<-tot-100
    loop.counter<-ceiling(rest/100)
    for(i in 1:loop.counter){
      offs<-100*i
      url$query<-list(order = order.by, limit = 100, offset = offs, responsible = responsibleID)
      test_detail<-GET(url = build_url(url), auth)
      check_response(test_detail)
      aJsonFile<-tempfile()
      writeBin(content(test_detail, "raw"), aJsonFile)
      test_json<-fromJSON(aJsonFile)
      listIndex<-i+1
      full_data[[listIndex]]<-data.table(test_json$Assignments, key = "Id")
    }
    test_json<-rbindlist(full_data)
    return(test_json)
  }
}

#' Survey Solutions API call for assingment manipulation
#'
#' \code{suso_set_assignments} allows to (re-)assign, change limits or audio
#' recording settings, as well as archiving assignments.
#' ATTENTION: still under development.
#'
#' @param server Survey Solutions server address
#' @param apiUser Survey Solutions API user
#' @param apiPass Survey Solutions API password
#' @param token If Survey Solutions server token is provided \emph{apiUser} and \emph{apiPass} will be ignored
#' @param workspace If workspace name is provide requests are made regarding this specific workspace
#' @param AssId the assignment id for which the change is required
#' @param payload if NULL a list of all assignments on the server, if not NULL
#' the assignment details for a specific assignment ID, if specific operations type is chosen, then requires the
#' corresponding values. See details bellow.
#' @param operations.type specifies the desired operation, one of recordAudio, archive/unarchive, (re-)assign
#' changeQuantity, or close, if specified, requires also \emph{AssId} to be specified.
#'
#' @export
#' @import data.table

suso_set_assignments<-function(server = suso_get_api_key("susoServer"),
                               apiUser = suso_get_api_key("susoUser"),
                               apiPass = suso_get_api_key("susoPass"),
                               token = NULL,
                               workspace = NULL,
                               AssId=NULL,
                               payload = NULL,
                               operations.type = c("recordAudio",
                                                   "archive",
                                                   "assign",
                                                   "changeQuantity",
                                                   "close",
                                                   "unarchive")) {
  ## workspace default
  workspace<-.ws_default(ws = workspace)

  ############
  # 1. create variables
  ##  BASE URL
  url<-httr::parse_url(url = server)
  url$scheme<-"https"
  url$path<-file.path(workspace,"api", "v1", "assignments")
  ## Authenticate
  auth<-httr::authenticate(apiUser, apiPass, type = "basic")
  ###########

  js_ch<-list(Responsible=unbox(payload))
  url<-modify_url(url, path = file.path(url$path, AssId, operations.type))
  test_post<-httr::PATCH(url = url,
                         accept_json(),add_headers(charset="utf-8"),
                         auth,
                         body=js_ch, encode = "json")
  ##  ATTENTION: USE THE RESPONSE
  check_response(test_post)
  aJsonFile<-tempfile()
  writeBin(content(test_post, "raw"), aJsonFile)
  status_list<-fromJSON(aJsonFile)
  return(status_list)
}


