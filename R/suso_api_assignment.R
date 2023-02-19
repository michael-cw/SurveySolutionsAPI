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
#' @return Returns an S3 object of assignmentClass. If you select any of the operations types, then no data.frame is returned,
#' the data.table will be NULL, however any information returned from the API can be retrieved by using the \code{getinfo()}
#' function with the corresponding arguments.
#'
#' @examples
#' \dontrun{
#'
#' # get all assignment for specific workspace
#' asslist<-suso_get_assignments(
#'                    workspace = "myworkspace"
#'                    )
#' # get all assignment for specific responsible
#' asslist<-suso_get_assignments(
#'                    workspace = "myworkspace",
#'                    responsibleID = "a67d2b82-bf28-40cf-bd1a-7901225c0885"
#'                    )
#' # get the overall count
#' getinfo(asslist, "totalcount")
#'
#' #get all single assignment details
#' asslist<-suso_get_assignments(
#'                    workspace = "myworkspace",
#'                    AssId = 1
#'                    )
#' # retrieve the uid of the person responsible
#' getinfo(asslist, "responsibleid")
#'
#' # get the status of the quantitysettings
#' asslist<-suso_get_assignments(
#'                    workspace = "myworkspace",
#'                    AssId = 1,
#'                    operations.type = "assignmentQuantitySettings")
#' asslist
#' Null data.table (0 rows and 0 cols)
#'
#' getinfo(asslist, "canchangequantity")
#' [1] TRUE
#' }
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
  if(length(operations.type)>1) {
    if (is.null(AssId) & is.null(responsibleID)) {
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

      if(tot>0) {
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
        test_json$Assignments<-rbindlist(full_data)
        test_json<-assignmentClass(test_json)

        if(nrow(test_json>0)) {
          test_json[,CreatedAtUtc:=lubridate::as_datetime(CreatedAtUtc)][,UpdatedAtUtc:=lubridate::as_datetime(UpdatedAtUtc)]
          test_json[,ReceivedByTabletAtUtc:=lubridate::as_datetime(ReceivedByTabletAtUtc)][]
        }
        return(test_json)
      } else if(tot==0) {
        return(NULL)
      }
    } else if (!is.null(AssId)) {
      # 2.2. Single Assignment by Assignment ID
      url$path<-file.path(url$path, AssId)
      test_detail<-GET(url = build_url(url), auth)
      check_response(test_detail)
      aJsonFile<-tempfile()
      writeBin(content(test_detail, "raw"), aJsonFile)
      test_json<-fromJSON(aJsonFile)
      test_json<-assignmentClass(test_json)
      return(test_json)

    } else if (!is.null(responsibleID)){
      # 2.3. All Assignment by responsible ID
      url$query<-list(Responsible = responsibleID)
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
      #test_json<-rbindlist(full_data)

      test_json$Assignments<-rbindlist(full_data)
      test_json<-assignmentClass(test_json)

      if(nrow(test_json>0)) {
        test_json[,CreatedAtUtc:=lubridate::as_datetime(CreatedAtUtc)][,UpdatedAtUtc:=lubridate::as_datetime(UpdatedAtUtc)]
        test_json[,ReceivedByTabletAtUtc:=lubridate::as_datetime(ReceivedByTabletAtUtc)][]
      }
      return(test_json)
    }

  } else if(operations.type=="assignmentQuantitySettings") {
    print("assignmentQuantitySettings")

    url$path<-file.path(url$path, AssId, operations.type, fsep = "/")
    test_detail<-GET(url = build_url(url), auth)
    check_response(test_detail)
    aJsonFile<-tempfile()
    writeBin(content(test_detail, "raw"), aJsonFile)
    test_json<-fromJSON(aJsonFile)
    test_json$Assignments<-data.table::data.table(NULL)
    test_json<-assignmentClass(test_json)
    return(test_json)



  } else if(operations.type=="history") {
    print("history")

    url$path<-file.path(url$path, AssId, operations.type, fsep = "/")
    test_detail<-GET(url = build_url(url), auth)
    check_response(test_detail)
    aJsonFile<-tempfile()
    writeBin(content(test_detail, "raw"), aJsonFile)
    test_json<-fromJSON(aJsonFile)
    test_json$Assignments<-data.table::data.table(test_json$History)
    test_json<-assignmentClass(test_json)
  } else if(operations.type=="recordAudio") {
    print("recordAudio")
    url$path<-file.path(url$path, AssId, operations.type, fsep = "/")
    test_detail<-GET(url = build_url(url), auth)
    check_response(test_detail)
    aJsonFile<-tempfile()
    writeBin(content(test_detail, "raw"), aJsonFile)
    test_json<-fromJSON(aJsonFile)
    test_json$Assignments<-data.table::data.table(test_json$History)
    test_json<-assignmentClass(test_json)
  }
}

#' Survey Solutions API call for assingment manipulation
#'
#' \code{suso_set_assignments} allows to (re-)assign, change limits or audio
#' recording settings, as well as archiving/unarchive and close assignments.
#'
#' @param server Survey Solutions server address
#' @param apiUser Survey Solutions API user
#' @param apiPass Survey Solutions API password
#' @param token If Survey Solutions server token is provided \emph{apiUser} and \emph{apiPass} will be ignored
#' @param workspace If workspace name is provide requests are made regarding this specific workspace
#' @param AssId the assignment id for which the change is required
#' @param payload requirements depend on the operations type. See details bellow.
#' @param operations.type specifies the desired operation, one of recordAudio, archive/unarchive, (re-)assign
#' changeQuantity, or close, if specified, requires in some case also \emph{pauyload} to be specified. See details bellow.
#'
#' @details If operations.type is \emph{recordAudio}, \code{TRUE/FALSE} is required as payload, if it is \emph{archive} or \emph{unarchive},
#' no payload is required, if it is \emph{assign} the payload must be the uid of the new responsible, if it is \emph{changeQuantity} the payload
#' must be the new integer number of assignments, if it is \emph{close} no payload is required.
#'
#'
#' @return Returns an S3 object of assignmentClass
#'
#' @examples
#' \dontrun{
#'
#' # (re-)assign existing assignment
#' asslist<-suso_set_assignments(
#'                    workspace = "myworkspace",
#'                    AssId = 10,
#'                    payload = "43f3d2bd-7959-4706-97ae-2653b5685c9e",
#'                    operations.type = "assign"
#'                    )
#' # get all assignment for specific responsible
#' asslist<-suso_set_assignments(
#'                    workspace = "myworkspace",
#'                    responsibleID = "a67d2b82-bf28-40cf-bd1a-7901225c0885"
#'                    )
#' # get the overall count
#' getinfo(asslist, "totalcount")
#'
#' #get all single assignment details
#' asslist<-suso_set_assignments(
#'                    workspace = "myworkspace",
#'                    AssId = 1
#'                    )
#' # retrieve the uid of the person responsible
#' getinfo(asslist, "responsibleid")
#'
#' }
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


  ##  BASE URL
  url<-httr::parse_url(url = server)
  url$scheme<-"https"
  url$path<-file.path(workspace,"api", "v1", "assignments")
  url<-modify_url(url, path = file.path(url$path, AssId, operations.type))

  ## Authenticate
  auth<-httr::authenticate(apiUser, apiPass, type = "basic")

  ## operation type payload:
  ## rec audio requires payload, returns 204 if success
  ## archive requires no payload, returns identifying data
  ## assign requires payload, returns identifying data
  ## changeQuantity requires simple payload, returns identifying data
  ## close requires no payload, returns identifying data
  ## unarchive requires no payload, returns identifying data

  if(operations.type=="recordAudio") js_ch<-list(Enabled=unbox(payload))
  if(operations.type=="assign") js_ch<-list(Responsible=unbox(payload))
  if(operations.type=="changeQuantity") js_ch<-as.character(payload)
  if(operations.type=="close" | operations.type=="archive" | operations.type=="unarchive") js_ch<-NULL

  test_post<-httr::PATCH(url = url,
                         httr::accept_json(),
                         httr::user_agent("r api v1"),
                         httr::add_headers(charset="utf-8"),
                         httr::content_type_json(),
                         auth,
                         encode = "json",
                         body=js_ch
  )

  ##  ATTENTION: USE THE RESPONSE
  check_response(test_post)
  aJsonFile<-tempfile()
  writeBin(content(test_post, "raw"), aJsonFile)
  status_list<-fromJSON(aJsonFile)
  status_list<-assignmentClass(status_list)
  return(status_list)
}


