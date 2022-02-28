#' Survey Solutions API call to generate and download the data
#'
#' Generates and downloads the data from your Survey Solutions server.
#'
#' @param server Survey Solutions server address
#' @param questID Questionnaire ID
#' @param version Questionnaire version
#' @param apiUser Survey Solutions API user
#' @param apiPass Survey Solutions API password
#' @param workspace server workspace, if nothing provided, defaults to primary
#' @param token If Survey Solutions server token is provided \emph{apiUser} and \emph{apiPass} will be ignored
#' @param workStatus define which statuses the file should inlude (i.e. \emph{Restored,Created,SupervisorAssigned,InterviewerAssigned,
#' RejectedBySupervisor,ReadyForInterview,
#' SentToCapi,Restarted,Completed,ApprovedBySupervisor,
#' RejectedByHeadquarters,ApprovedByHeadquarters,Deleted}), if NULL all is exported
#' @param reloadTimeDiff time difference in hours between last generated file and now
#' @param inShinyApp if True, file interacts with shiny progress bar
#' @param n_id_vars  specify the number of identification variables (not used for now!)
#'
#'
#' @details
#'
#' This API call uses the STATA export format to retrieve the categorical variables and labels.The result
#' has the following characteristics:
#' \itemize{
#'   \item it is returned as a LIST with up to 4 different lists. The list names are:
#'           \itemize{
#'              \item \emph{main} Contains the top level data, and (if available interviewer comments)
#'              \item \emph{R1} All rosters in roster level 1
#'              \item \emph{R2} All rosters in roster level 2
#'              \item \emph{R3} All rosters in roster level 3
#'           }
#'   \item Number of lists depends on the level of roster nesting
#'   \item All variable names are transformed to lower case and categorical variables are consistently labeled
#'   \item Consistent id variables are generated with
#'          \itemize{
#'          \item interview__id transformed to id
#'          \item parent ids consistently number starting from id (questionnairid) to idX (maximum id3)
#'          }
#'   \item List elements are returned as data.tables
#'   \item Allows for specification of reload time (i.e. generation of new download file)
#'   \item PRESERVES categorical labels \emph{and} values.
#'   }
#'
#'
#'
#' @export
#'
#' @import zip
#' @import haven
#' @import readstata13
#'
suso_export<-function(server= suso_get_api_key("susoServer"),
                      apiUser=suso_get_api_key("susoUser"),
                      apiPass=suso_get_api_key("susoPass"),
                      workspace = NULL,
                      token = NULL,
                      questID="",
                      version=1,
                      workStatus="Completed",
                      reloadTimeDiff=1,
                      inShinyApp=F,
                      n_id_vars=11){
  #######################################
  ## use stata for labels
  format_para<-"STATA"
  #######################################
  ## Check arguments
  ##  - workStatus
  margs<-suso_getQuestDetails(operation.type = "statuses", workspace = workspace)
  if(!is.null(workStatus)) {
    workStatus<-match.arg(workStatus, margs)
  } else {
    workStatus<-"All"
  }

  ########################################
  ## BASE SET-UP
  ##  OPTIONS: i) No scientific notation
  options("scipen"=100, "digits"=4)
  ## Base file
  dataPath<-file.path(tempdir(),"application_data.zip")
  if (file.exists(dataPath)) file.remove(dataPath)
  ## workspace default
  workspace<-.ws_default(ws = workspace)
  ##  BASE URL
  url<-parse_url((server))
  url$scheme<-"https"
  ##  QUEST ID
  quid=paste0(stringr::str_replace_all(questID, "-", ""), "$", version)
  url$path<-file.path(workspace,"api", "v2", "export")
  ##  CREDENTIALS
  usr<-apiUser
  pass<-apiPass
  auth<-authenticate(usr, pass, type = "basic")

  ######################################################################################
  ##  GET QUESTIONNAIRE NAME FOR FILE SELECTION
  ######################################################################################
  url_quest<-parse_url(server)
  url_quest$path<-paste0(workspace,"/api/v1/questionnaires")
  test_quest<-GET(url = build_url(url_quest),
                  auth)
  stop_for_status(test_quest, status_code(test_quest))

  aJsonFile<-tempfile()
  writeBin(content(test_quest, "raw"), aJsonFile)
  test_json<-data.table(fromJSON(aJsonFile)$Questionnaires)
  questName<-subset(test_json, QuestionnaireIdentity==quid, Variable)
  questName<-questName$Variable


  ###############################################
  ##          CHECK TIME OF LAST FILE
  ###############################################
  time_limit<-strptime(suso_details_lastexport(quid=questID,
                                               version = version,
                                               workspace = workspace,
                                               format=format_para)$StartDate, format = "%Y-%m-%dT%H:%M:%S")
  ###############################################################################
  ##          START FILE CREATION
  ##              -IFF time diff is larger than treshold,
  ##                  -->then new file is generated (DEFAULT IS 0)
  ###############################################################################
  ##  1. START FILE CREATION --> file is only created when time difference is larger then reloadTimeDiff
  current_time<-strptime(Sys.time(), format = "%Y-%m-%d %H:%M:%S")
  timeDiff<-difftime(current_time, time_limit, units = "hours")
  ## reset time difference
  if(length(timeDiff)==0) timeDiff=reloadTimeDiff+1

    cat("The last file has been created", timeDiff, "hours ago.")
  if((timeDiff>reloadTimeDiff) | length(time_limit)==0){
    cat("A new file will be generated. This may take a while.\n")
    ## Create Request
    js_ch <- list(
      ExportType = jsonlite::unbox(format_para),
      QuestionnaireId = jsonlite::unbox(quid),
      InterviewStatus = jsonlite::unbox(workStatus)
    )


    url_start<-build_url(url)
    test_post<-httr::POST(url = url_start,
                          auth,
                          body=js_ch,
                          encode = "json")
    stop_for_status(test_post, "Request Failed")
    job_id<-content(test_post)$JobId

    ##########################
    ##  2. STATUS CHECK
    test_json<-suso_details_lastexport(pid = job_id,
                                       workspace = workspace)


    ## 2.1. Wait until finished
    while ((test_json$ExportStatus!="Completed")) {
      cat(test_json$Progress, "\n")
      test_json<-suso_details_lastexport(pid = job_id,
                                         workspace = workspace)
      Sys.sleep(10)
    }
  } else if (timeDiff<=reloadTimeDiff){
    ## get job id from existing file
    job_id<-suso_details_lastexport(quid=questID,
                                    version = version,
                                    workspace = workspace,
                                    format=format_para)$JobId[1]
  }

  ##########################
  ##  3. GET URL AND DOWNLOAD
  test_exp<-GET(url = modify_url(url, path = file.path(url$path, job_id, "file")),
                auth,
                write_disk(dataPath, overwrite = TRUE),
                config(
                  followlocation = 1L,
                  unrestricted_auth = 0L,
                  tcp_keepalive = 1L
                ))

  ## 3.1. Stop by status code
  if (status_code(test_exp)!=200) stop(
    cat("Not data for download.", "Did you specify the correct questionnaire and version?",
        paste0("Status: ", status_code(test_exp)), sep = "\n"),
    call. = F
  )




  ###################################################################################################
  ##                            DATA PROCESSING                                                     #
  ##            All data is extracted into a list seperated by its hirarichial positin in the
  ##            questionnaire. The structure is
  ##            [DF name]$main ... inferview comments, level 0
  ##            [DF name]$R1   ... level 1
  ##            [DF name]$R2   ... level 2 etc.
  ###################################################################################################
  ## Load the data
  uploadFile<-dataPath
  file_list<-tryCatch(utils::unzip(uploadFile, list=T), error=function(e) return("No File Found!"))



  #########################################
  ## Extracting files with loop
  files<-file_list[file_list$Length>0,1]
  files<-files[grep(pattern = ".dta$", files)]

  ##  Functions for files
  file_collector<-list()
  file_collector.main<-list()
  file_collector.rost.L1<-list()
  file_collector.rost.L2<-list()
  file_collector.rost.L3<-list()

  ##  Extract to tmpdir
  tmp_zip_dir<-tempdir()
  zip::unzip(uploadFile, exdir = tmp_zip_dir)

  statusControl<-haven::read_dta(paste0(tmp_zip_dir,"/interview__actions.dta"))
  statusControl<-data.table(as_factor(statusControl))
  names(statusControl)<-tolower(names(statusControl))
  setnames(statusControl,c("interview__id"),"id")
  tmpName<-names(statusControl)
  MAINparIDcol<-grep(pattern = "^id", tmpName)
  statusControl<-data.table(statusControl, key=tmpName[MAINparIDcol])
  if(!is.null(workStatus)){
    statusControl<-statusControl[action==workStatus, ]
    statusControl<-unique(statusControl, by="id")
  }
  ########################################
  ##  The export loop
  ##    - all from tmp dir
  ##    - checks roster by id
  ##    - USE haven stata, as else file corrupted
  for (file_zip in files) {
    name<-strsplit(file_zip, ".dta")[[1]]
    if (exists("tmp_file")) tmp_file<-NULL
    tmp_file <-tryCatch(data.table(haven::read_dta(file.path(tmp_zip_dir, file_zip), encoding = 'latin1')),
                        error=function(e) return(NULL))
    if(is.null(tmp_file)) {print(paste("ERROR in dta file:", file_zip));next()}

    ######################################################################
    ##  LABELS
    ##  1. Value Labels (single select)
    ##    - identified by attribute...?? check for other solutions, not stable!!!
    catVars<-sapply(names(tmp_file), function(x) ifelse(is.null(attr(tmp_file[[x]], "labels")),
                                                        return(NULL), return(attr(tmp_file[[x]], "labels"))), simplify = T, USE.NAMES = T)
    catVars<-catVars[-which(sapply(catVars, is.null))]
    ##  1.2. Assign
    coll<-names(catVars)
    for (v in coll) {
      set(tmp_file, NULL, v, as_factor(tmp_file[[v]]))
    }
    ##  2. Multi Select
    ms<-grep("__[0-9]*$",names(tmp_file), perl=T)
    ######################################################################
    ##  READING THE FILES
    ##  1. MAIN FILE (by questionnaire variable identification)
    if (name==questName){
      tmpName<-tolower(names(tmp_file))
      names(tmp_file)<-tmpName

      MAINparIDcol<-grep(pattern = "^.+__id$", tmpName)
      main_file<-copy(tmp_file)
      setnames(main_file,c("interview__id"),"id")
      attributes(tmp_file)$rosterlevel<-"main"
      setkeyv(main_file, c("id", "interview__key"))
      setkeyv(statusControl, c("id", "interview__key"))
      main_file<-main_file[statusControl, nomatch=0]
      setnames(main_file,"id",c("interview__id"))
      file_collector.main[[paste(name)]]<-main_file
    } else if (name=="interview__comments") {
      ##  2. COMMENT FILE (standard name, parentid=interview__id)
      tmpName<-tolower(names(tmp_file))
      names(tmp_file)<-tmpName
      tmp.parIDcol<-grep(pattern = "^interview__id", names(tmp_file))
      tmp.idCol<-grep(pattern = "^id", names(tmp_file))
      comments<-data.table(tmp_file, key=tmpName[tmp.parIDcol])
      file_collector.main[[paste(name)]]<-comments

    } else {
      ##  4. OTHER FILE (length parentid defines nesting)
      tmpName<-tolower(names(tmp_file))
      names(tmp_file)<-tmpName
      tmp.parIDcol<-grep(pattern = "__id", names(tmp_file))
      nesting<-length(tmp.parIDcol)-1
      print(name)
      print(nesting)
      print("****")
      if (nesting==1 & length(tmp.parIDcol)==2 & nrow(tmp_file)!=0){
        ##  4.1. ROSTER LEVEL 1
        tmp.parIDcol<-names(tmp_file)[tmp.parIDcol]
        id<-paste0(tolower(name), "__id")
        names(tmp_file)<-tolower(names(tmp_file))
        tmp_file<-plyr::rename(tmp_file, replace=c("interview__id"="id", setNames("id1", id)))
        tmpName<-tolower(names(tmp_file))
        MAINparIDcol<-grep(pattern = "^id$", tmpName)
        r1id<-grep(pattern = "^id1$", tmpName)
        tmp_file<-data.table(tmp_file, key=tmpName[MAINparIDcol])

        setkeyv(tmp_file, c("id", "interview__key"))
        setkeyv(statusControl, c("id", "interview__key"))
        tmp_file<-tmp_file[statusControl, nomatch=0]

        setkeyv(tmp_file, c(tmpName[MAINparIDcol], tmpName[r1id]) )
        file_collector.rost.L1[[paste(name)]]<-tmp_file
      } else if (nesting==1.5 & length(tmp.parIDcol)==2& nrow(tmp_file)!=0){
        ##  4.1. ROSTER LEVEL 1 + FIXED ROSTER
        tmp_file<-plyr::rename(tmp_file, replace=c("parentid2"="id","parentid1"="id1", "id"="fix.id"))
        tmpName<-tolower(names(tmp_file))
        MAINparIDcol<-grep(pattern = "^id$", tmpName)
        r1id<-grep(pattern = "^id1$", tmpName)
        tmp_file<-data.table(tmp_file, key=tmpName[MAINparIDcol])
        tmp_file<-merge(tmp_file, statusControl, by=c("id", "interview__key"), allow.cartesian=TRUE)
        setkeyv(tmp_file, c(tmpName[MAINparIDcol], tmpName[r1id]) )
        file_collector.rost.L1[[paste(name)]]<-tmp_file
      } else if (nesting==2& nrow(tmp_file)!=0) {
        ##  4.2. ROSTER LEVEL 2
        tmp.parIDcol<-names(tmp_file)[tmp.parIDcol]
        id<-paste0(tolower(name), "__id")
        parentid1<-tmp.parIDcol[tmp.parIDcol!=id & tmp.parIDcol!="interview__id"]
        tmp_file<-plyr::rename(tmp_file, replace=c("interview__id"="id", setNames("id1", parentid1), setNames("id2", id)))
        tmpName<-tolower(names(tmp_file))
        MAINparIDcol<-grep(pattern = "^id$", tmpName)
        r1id<-grep(pattern = "^id1$", tmpName)
        r2id<-grep(pattern = "^id2$", tmpName)
        tmp_file<-data.table(tmp_file, key=tmpName[MAINparIDcol])
        tmp_file<-merge(tmp_file, statusControl, by=c("id", "interview__key"), allow.cartesian=TRUE)
        setkeyv(tmp_file, c(tmpName[MAINparIDcol], tmpName[r1id], tmpName[r2id]))
        file_collector.rost.L2[[paste(name)]]<-tmp_file
      } else if (nesting==2.5& nrow(tmp_file)!=0) {
        ##  4.3. ROSTER LEVEL 2
        tmp_file<-plyr::rename(tmp_file, replace=c("parentid3"="id", "parentid2"="id1", "parentid3"="id2", "id"="id3"))
        tmpName<-tolower(names(tmp_file))
        MAINparIDcol<-grep(pattern = "^id$", tmpName)
        r1id<-grep(pattern = "^id1$", tmpName)
        r2id<-grep(pattern = "^id2$", tmpName)
        r3id<-grep(pattern = "^id3$", tmpName)
        tmp_file<-data.table(tmp_file, key=tmpName[MAINparIDcol])
        tmp_file<-merge(tmp_file, statusControl, by=c("id", "interview__key"), allow.cartesian=TRUE)
        setkeyv(tmp_file, c(tmpName[MAINparIDcol], tmpName[r1id], tmpName[r2id], tmpName[r3id]))
        file_collector.rost.L3[[paste(name)]]<-tmp_file
      }
    }

    if (inShinyApp) incProgress(0.1)
  }

  ######################################################################
  ##  MERGING THE FILES
  ##    i) By Section for Nested roster
  ##    ii) Exclude the nested rosters by using is.numeric
  ##  1. Split of section identifier, must be by "_"



  #TO DO#



  ######################################################################
  ##  COLLECTING THE LISTS
  ##  1. MAIN (always exists, no check)
  file_collector[["main"]]<-file_collector.main
  ##  2. ROSTER LEVEL 1

  if(exists("file_collector.rost.L1")){
    file_collector[["R1"]]<-file_collector.rost.L1}
  ##  3. ROSTER LEVEL 2
  if(exists("file_collector.rost.L2")){
    file_collector[["R2"]]<-file_collector.rost.L2}
  ##  4. ROSTER LEVEL 3
  if(exists("file_collector.rost.L3")){
    file_collector[["R3"]]<-file_collector.rost.L3}
  return(file_collector)


  ################################################ FIN ######################################################
}
