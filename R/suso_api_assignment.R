#' Survey Solutions API call for assignment list
#'
#'
#' \code{suso_get_assignments} calls the Survey Solutions assingment API
#'
#' @param questID only assignments for \emph{QuestionnaireId} are returned, requires \code{version} being not NULL
#' @param version version of the questionnaire, only required with \code{questID}
#' @param AssId if NULL a list of all assignments on the server, if not NULL
#' the assignment details for a specific assignment ID
#' @param responsibleID the ID of the responsible user (Supervisor or Interviewer).
#' Retrieves all assignments for this user.
#' @param order.by determines the column by which the assignment list should be ordered
#'
#' @export

suso_get_assignments<-function(questID = NULL,
                               server = suso_get_api_key("susoServer"),
                               apiUser = suso_get_api_key("susoUser"),
                               apiPass = suso_get_api_key("susoPass"),
                               AssId=NULL,
                               version= NULL,
                               responsibleID = NULL,
                               order.by=c("Id,ResponsibleId,ResponsibleName,QuestionnaireId,InterviewsCount,
                                          Quantity,Archived,CreatedAtUtc,UpdatedAtUtc,
                                          IdentifyingQuestions,Email,Password,WebMode"),
                               supervisorId=NULL,
                               quantity = NULL) {
  ############
  # 1. create variables
  ##  BASE URL
  url<-httr::parse_url(paste0(server))
  url$scheme<-"https"
  url$path<-paste0("/api/v1/assignments")
  usr<-apiUser
  pass<-apiPass
  ###########
  # 2 Operations

  full_data<-list()
  if (is.null(AssId)) {
    # 2.1. All Assignments, without filtering, and ordered by ID
    url$query<-list(order = order.by, limit = 100)
    test_detail<-GET(url = build_url(url), authenticate(usr, pass, type = "basic"))
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
      test_detail<-GET(url = build_url(url), authenticate(usr, pass, type = "basic"))
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
    test_detail<-GET(url = build_url(url), authenticate(usr, pass, type = "basic"))
    aJsonFile<-tempfile()
    writeBin(content(test_detail, "raw"), aJsonFile)
    test_json<-fromJSON(aJsonFile)
    return(test_json)

  } else if (!is.null(responsibleID)){
    # 2.3. All Assignment by responsible ID
    url$query<-list(responsible = responsibleID)
    test_detail<-GET(url = build_url(url), authenticate(usr, pass, type = "basic"))
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
      test_detail<-GET(url = build_url(url), authenticate(usr, pass, type = "basic"))
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
#'
#' @export

suso_set_assignments<-function(server = suso_get_api_key("susoServer"),
                               apiUser = suso_get_api_key("susoUser"),
                               apiPass = suso_get_api_key("susoPass"),
                               AssId=NULL,
                               responsibleID = NULL) {
  ############
  # 1. create variables
  ##  BASE URL
  url<-httr::parse_url(paste0(server))
  url$scheme<-"https"
  url$path<-paste0("/api/v1/assignments")
  usr<-apiUser
  pass<-apiPass
  ###########

  js_ch<-list(Responsible=unbox(responsibleID))
  test_post<-httr::PATCH(url = modify_url(path = file.path(url$path, AssId, "/assign")),
                         accept_json(),add_headers(charset="utf-8"),
                         authenticate(usr, pass, type = "basic"),
                         body=js_ch, encode = "json")
  ##  ATTENTION: USE THE RESPONSE
  print(status_code(test_post))
  aJsonFile<-tempfile()
  writeBin(content(test_post, "raw"), aJsonFile)
  status_list<-fromJSON(aJsonFile)
  return(status_list)
}


