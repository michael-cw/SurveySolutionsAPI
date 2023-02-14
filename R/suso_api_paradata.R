#'  SURVEY SOLUTIONS PARADATA EXPORT FUNCTION
#'
#' Exports Survey Solutions Paradata, and returns a data.table.
#'
#'
#'  \code{suso_export_paradata} returns a data.table. Calculates the response time
#'  and separtes multiple responses into individual columns. It also creates a variable
#'  \emph{counter} which preserves the sequence of events.
#'
#'
#' @param server Survey Solutions server address
#' @param apiUser Survey Solutions API user
#' @param apiPass Survey Solutions API password
#' @param workspace server workspace, if nothing provided, defaults to primary
#' @param token If Survey Solutions server token is provided \emph{apiUser} and \emph{apiPass} will be ignored
#' @param workStatus define which statuses the file should inlude (i.e. \emph{Restored,Created,SupervisorAssigned,InterviewerAssigned,
#' RejectedBySupervisor,ReadyForInterview,
#' SentToCapi,Restarted,Completed,ApprovedBySupervisor,
#' RejectedByHeadquarters,ApprovedByHeadquarters,Deleted}), if NULL all is exported
#' @param questID \emph{QuestionnaireId} for which the paradata should be generated
#' @param version questionnnaire version
#' @param reloadTimeDiff time difference in hours between last generated file and now
#' @param inShinyApp if True, file interacts with shiny progress bar
#' @param multiCore if not NULL, an integer number specifying the number of cores to use
#' @param onlyActiveEvents if TRUE only active events are exported, decreases processing time and memory requirements
#' @param allResponses if TRUE all responses will be extracted. Setting it to FALSE may decrease processing time and
#' memory requirements
#' @param gpsVarName provide GPS variable name. If not provided, identification is attempted by lookin for a variable containing gps in its name.
#' @param verbose if TRUE, shows messages about the operation carried out. Can be useful for longrunning operations.
#' @param showProgress also display the progress bars.
#'
#' @details The return value is a list with a separate list element for each event. If any of the variable
#' names contains \emph{gps} this function also attempts to identify (and extract) the geo-reference location. In case of multiple gps variables,
#' it identifies the first variable, with not all missing values. This in turn
#' facilitates the creation of paradata maps (see vignette on paradata). In addition it also returns all the variables and responses in separate columns and
#' as factors.
#' Exporting \emph{onlyActiveEvents} substantially decrease processing time. The events may be sufficient for most of the paradata analysis.
#'
#' To further decrease the processing time, one could set \emph{allResponses} to FALSE. Doing so will still export all the data, however it will
#' not attempt to extract all responses and setting them to factors.
#'
#' @export
#'
#'
#' @import future
#' @import doFuture
#' @import foreach
#' @importFrom progressr progressor
#' @importFrom progressr with_progress


suso_export_paradata<-function(server= suso_get_api_key("susoServer"),
                               apiUser=suso_get_api_key("susoUser"),
                               apiPass=suso_get_api_key("susoPass"),
                               workspace = NULL,
                               token = NULL,
                               questID="xxxx-xxx-xxxx-xxx-xxx",
                               version=1,
                               workStatus="Completed",
                               reloadTimeDiff=1,
                               inShinyApp=FALSE,
                               multiCore = NULL,
                               onlyActiveEvents = FALSE,
                               allResponses = TRUE,
                               gpsVarName = NA,
                               verbose = T,
                               showProgress = F){
  ######################################################################################
  ##          SETUP
  ######################################################################################
  ## workspace default
  workspace<-.ws_default(ws = workspace)
  ##  1. options
  format_para="Paradata"
  options(warn = -1)
  ##  2. TEMP files and lists
  aJsonFile<-tempfile(fileext = ".json")
  dataPath<-file.path(tempdir(),"application_data.zip")
  if (file.exists(dataPath)) file.remove(dataPath)
  para_data<-list()

  ##  BASE URL
  url<-httr::parse_url((server))
  url$scheme<-"https"
  ##  QUEST ID
  quid=paste0(stringr::str_replace_all(questID, "-", ""), "$", version)
  url$path<-file.path(workspace, "api", "v2", "export")
  ## Authentication
  auth<-authenticate(apiUser, apiPass, type = "basic")
  ###############################################
  ##          CHECK TIME OF LAST FILE
  ###############################################
  time_limit<-strptime(suso_details_lastexport(quid=questID,
                                               version = version,
                                               workspace = workspace,
                                               format=format_para)$StartDate, format = "%Y-%m-%dT%H:%M:%S")
  # Check if no file has been created
  if(length(time_limit)==0) time_limit<-strptime(Sys.time(), format = "%Y-%m-%d %H:%M:%S") - lubridate::ddays(28)
  # Select latest file created if more than 1 file
  if(length(time_limit)>1) time_limit<-time_limit[1]

  ###############################################################################
  ##          START FILE CREATION
  ##              -IFF time diff is larger than treshold,
  ##                  -->then new file is generated (DEFAULT IS 0)
  ###############################################################################
  ##  1. START FILE CREATION --> file is only created when time difference is larger then reloadTimeDiff
  current_time<-strptime(Sys.time(), format = "%Y-%m-%d %H:%M:%S")
  timeDiff<-ceiling(difftime(current_time, time_limit, units = "hours"))

  cat(paste("\nThe last file has been created", timeDiff[1], "hours ago.\n\n"))

  ## Create Request
  js_ch <- list(
    ExportType = jsonlite::unbox(format_para),
    QuestionnaireId = jsonlite::unbox(quid),
    InterviewStatus = jsonlite::unbox(workStatus)
  )

  if(timeDiff>reloadTimeDiff){
    cat("A new file will be generated. This may take a while.\n")
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
      cat(test_json$Progress, "%\n")
      test_json<-suso_details_lastexport(pid = job_id,
                                         workspace = workspace)
      Sys.sleep(10)
    }
    cat("Finished file generation.\n\n")
  } else if (timeDiff<=reloadTimeDiff) {
    job_id<-suso_details_lastexport(quid=questID,
                                    version = version,
                                    workspace = workspace,
                                    format=format_para)$JobId[1]
  }
  ##########################
  ##  3. GET URL AND DOWNLOAD
  cat("Starting download & file extraction. \n\n")
  if (showProgress){
    test_exp<-GET(url = modify_url(url, path = file.path(url$path, job_id, "file")),
                  auth,
                  httr::progress(),
                  write_disk(dataPath, overwrite = TRUE),
                  config(
                    followlocation = 1L,
                    unrestricted_auth = 0L,
                    tcp_keepalive = 1L
                  )
    )
  } else {
    test_exp<-GET(url = modify_url(url, path = file.path(url$path, job_id, "file")),
                  auth,
                  write_disk(dataPath, overwrite = TRUE),
                  config(
                    followlocation = 1L,
                    unrestricted_auth = 0L,
                    tcp_keepalive = 1L
                  )
    )
  }
  ## 3.1. Stop by status code
  if (status_code(test_exp)!=200) stop(
    cat("Not data for download.", "Did you specify the correct questionnaire and version?",
        paste0("Status: ", status_code(test_exp)), sep = "\n"),
    call. = F
  )
  ########################### FILE ############################################
  para_data<-list()
  #############################################################################
  uploadFile<-dataPath
  FILE.list<-tryCatch(utils::unzip(uploadFile, list=T), error=function(e) return("No File Found!"))
  #########################################################################
  ##  Unpack, create data.tables and stor in list, create subsets by action
  #########################################################################
  file.name<-FILE.list[grep(".tab$", FILE.list$Name), 1]
  nfiles<-length(file.name)
  ##  Create final file path

  thefiles<-tempdir()
  zip::unzip(uploadFile, exdir = thefiles)

  ##  Unpack (w. function and lapply-->helpers)
  prog<-1
  fp<-file.path(thefiles, file.name)
  info <- (file.info(fp))
  if (info$size <=75) stop('\nNo Records yet!', call. = F)
  ## UNPACK
  paradata_files<-unpack(fp=fp, allResponses = allResponses, inShinyServer = inShinyApp)
  ## STOP when empty
  if (is.null(paradata_files)) stop('\nNo Records yet!', call. = F)
  if(inShinyApp) incProgress(amount = 0.25, message = "Transformation completed")
  ## TRANSFORMATIONS
  ## A add rid if it doesnt exist
  if (length(grep("rid", names(paradata_files)))==0) paradata_files[,rid:=0]

  ##################################################################
  ##  2.1. Get Start/End date
  #fromDate<-as.character(min(paradata_files$date, na.rm = T), "%d %B, %Y")
  #toDate<-as.character(max(paradata_files$date, na.rm = T), "%d %B, %Y")

  ##################################################################
  ##  2.2. GET ALL ACTION COUNTS
  actionDistr<-paradata_files[,.(count=.N), by=.(action)]
  setorderv(actionDistr, "count", order = -1)
  ##################################################################
  ##  2.3. GET ALL RESPONSIBLE COUNTS
  userDistr<-paradata_files[,.(count=.N), by=.(responsible)]
  setorderv(userDistr, "count", order = -1)
  ##################################################################
  ##  2.4. GET ALL ROLE COUNTS
  roleDistr<-paradata_files[,.(count=.N), by=.(role)]
  setorderv(roleDistr, "count", order = -1)

  ##  2.5. Extract questionnaire ID and Key
  KeyAssigned<-paradata_files[action=="KeyAssigned"][,c("responsible", "role", "var_resp", "rid"):=NULL]
  setnames(KeyAssigned, "var", "key")
  KeyAssigned<-droplevels(KeyAssigned)
  paradata_files<-paradata_files[action!="KeyAssigned"]
  paradata_files<-droplevels(paradata_files)
  KeyAssigned<-KeyAssigned[,.SD[1], by=.(interview__id)]
  KeyAssigned_merge<-KeyAssigned[ ,.(interview__id, key)]
  setkeyv(KeyAssigned, "interview__id")
  para_data$KeyAssigned<-KeyAssigned
  ##  2.6. Comments
  CommentSet<-paradata_files[action=="CommentSet"]
  if(nrow(CommentSet)>0){
    setnames(CommentSet, "var_resp", "comment")
    CommentSet<-droplevels(CommentSet)
    paradata_files<-paradata_files[action!="CommentSet"]
    paradata_files[,action:=droplevels(action)]
    para_data$CommentSet<-CommentSet
  }
  ## 2.7 Completed
  Completed<-paradata_files[action=="Completed"][,c("responsible", "role" ,"var_resp", "rid"):=NULL]
  if(nrow(Completed)>0){
    setnames(Completed, "var", "comment")
    Completed<-droplevels(Completed)
    paradata_files<-paradata_files[action!="Completed"]
    paradata_files[,action:=droplevels(action)]
    para_data$Completed<-Completed
  }
  ##  2.8. AnswerSet
  para1_answer<-paradata_files[action=="AnswerSet"|action=="Paused"]
  para1_answer[,action:=droplevels(action)]

  ##  3. Time Difference (SORT by counter)
  ##  3.1. Function (use shift/lead, and check lead date is the same)
  cat("\nCalculating Response Timings.\n")
  para1_answer<-calcTimeDiff(para1_answer)
  ##  3.2. Other calculations
  para1_answer<-para1_answer[!is.na(breaks)]
  para1_answer[,duration:=round((sum(resp_time, na.rm = T))/60, 2), by = .(interview__id)]
  para1_answer[breaks==0,durationNOBREAK:=round((sum(resp_time, na.rm = T))/60, 2), by = .(interview__id)]
  para1_answer[,m_resp_time_varTRIM:=(mean(resp_time, na.rm = T, trim = 0.05)), by = .(var)]
  para1_answer[,m_resp_time_var:=(mean(resp_time, na.rm = T)), by = .(var)]
  para1_answer[breaks==0,m_diff_dev:=resp_time-m_resp_time_varTRIM]
  para1_answer[,start:=min(time, na.rm = T), by=.(interview__id)]
  para1_answer[,startHour:=min(hour(time), na.rm = T), by=.(interview__id)]
  para1_answer[,role:=droplevels(role)][,responsible:=droplevels(responsible)]
  para1_answer[,var:=as.factor(var)]
  para1_answer_merge<-para1_answer[,.SD[1], by=.(interview__id, role)]
  para1_answer_merge<-para1_answer_merge[ ,.(interview__id, responsible, role)]

  ##  2. GPS extract -->if no name, try identification through grepl
  varNames<-levels(para1_answer$var)
  if(is.na(gpsVarName)) {
    gpsVarMain<-varNames[grepl("gps", varNames)]
  } else {
    stopifnot(is.character(gpsVarName), gpsVarName %in% varNames)
    gpsVarMain<-gpsVarName
  }

  ## create gps file when exists
  if (length(gpsVarMain)>0) {
    ## Select first gps variable
    cat("\nExtracting GPS variable.\n")
    gpsVar<-gpsVarMain[1]
    gps_file<-para1_answer[var==gpsVar]
    if(nrow(gps_file)==0) stop(cat("No GPS values found with: ", gpsVarName ))
    if (!allResponses) {
      gp<-gps_file[,tstrsplit(response, ",", fixed=T, fill = "<NA>", names = TRUE)][]
      gps_file<-cbind(gps_file, gp)
      setnames(gps_file, c("V1", "V2"), c("response1", "response2"))
    }

    gps_file<-gps_file[, .(interview__id, responsible, time, var_resp, var,
                           date, durationNOBREAK, response1, response2)]
    gps_file<-gps_file[,c("long"):=tstrsplit(response2, "[", fixed=T ,keep=c(1))][]
    gps_file[,lat:=as.numeric(as.character(response1))]
    gps_file[,long:=as.numeric(as.character(long))]
    gpsSelect<-sum(!is.na(gps_file$lat))
    ## If empty iterate over next/only if length>1/until length==k
    k<-2
    while(gpsSelect>=0 & gpsSelect<=nrow(gps_file) & length(gpsVarMain) >1 & length(gpsVarMain) != k) {
      gpsVar<-gpsVarMain[k]
      gps_file<-para1_answer[var==gpsVar]
      if (!allResponses) {
        gp<-gps_file[,tstrsplit(response, ",", fixed=T, fill = "<NA>", names = TRUE)][]
        gps_file<-cbind(gps_file, gp)
        setnames(gps_file, c("V1", "V2"), c("response1", "response2"))
      }
      gps_file<-gps_file[, .(interview__id, responsible, time, var_resp,
                             date, durationNOBREAK, response1, response2)]
      gps_file<-gps_file[,c("long"):=tstrsplit(response2, "[", fixed=T ,keep=c(1))][]
      gps_file[,lat:=as.numeric(as.character(response1))]
      gps_file[,long:=as.numeric(as.character(long))]
      k<-k+1
      gpsSelect<-sum(!is.na(gps_file$lat))
    }
    ##  For merge with EVENT data
    gps_file_merge<-gps_file[,.(interview__id, lat, long)]
    gps_file_merge<-gps_file_merge[,.SD[1], by=.(interview__id)]
    setkeyv(gps_file_merge, "interview__id")
  }
  ##  Subset with function, key and lapply
  ## loop over levels of action with LAPPLY
  ## a<-lapply(levels(CHECK$action), FUN = subsetDataTableAction, CHECK)
  ##  not used for now
  subsetDataTableAction<-function(dt, x) {
    setkeyv(x, "action")
    file<-x[dt]
    return(file)
  }

  if (is.null(multiCore)) {
    cat("Processing: ")
    cat("\n\tAnswerSet\n")
    ##  ACTIVE EVENTS
    ##  2.8 Answer Set
    AnswerSet<-para1_answer
    AnswerSet<-AnswerSet[!is.na(interview__id)]
    setkeyv(AnswerSet, "interview__id")
    if(exists("gps_file_merge"))  AnswerSet<-gps_file_merge[AnswerSet, on="interview__id"]
    AnswerSet<-KeyAssigned_merge[AnswerSet, on="interview__id"]
    para_data$AnswerSet<-AnswerSet
    ##  2.9. Answer Removed (COUNT the number of Removed answer by questionnaire)
    cat("\n\tAnswerRemoved\n")
    AnswerRemoved<-paradata_files[action=="AnswerRemoved"]
    AnswerRemoved<-AnswerRemoved[!is.na(interview__id)]
    AnswerRemoved[, count:=length(counter), by=interview__id]
    AnswerRemoved[,c("responsible", "role"):=NULL]
    #AnswerRemoved<-droplevels(AnswerRemoved)
    AnswerRemoved<-merge(AnswerRemoved, para1_answer_merge, by="interview__id", allow.cartesian=T)
    setkeyv(AnswerRemoved, "interview__id")
    if(exists("gps_file_merge")) AnswerRemoved<-gps_file_merge[AnswerRemoved, on="interview__id"]
    AnswerRemoved<-KeyAssigned_merge[AnswerRemoved, on="interview__id"]
    para_data$AnswerRemoved<-AnswerRemoved
    ##  2.10. Approved
    cat("\n\tApproveByHeadquarter\n")
    ApproveByHeadquarter<-paradata_files[action=="ApproveByHeadquarter"]
    ApproveByHeadquarter<-droplevels(ApproveByHeadquarter)
    ApproveBySupervisor<-paradata_files[action=="ApproveBySupervisor"]
    ApproveBySupervisor<-droplevels(ApproveBySupervisor)
    ##  2.13 Restarted
    cat("\n\tRestarted\n")
    Restarted<-paradata_files[action=="Restarted"]
    Restarted<-Restarted[!is.na(interview__id),]
    Restarted[, count:=length(counter), by=interview__id]
    setkeyv(Restarted, "interview__id")
    if(exists("gps_file_merge")) Restarted<-gps_file_merge[Restarted, on="interview__id"]
    Restarted<-KeyAssigned_merge[Restarted, on="interview__id"]
    para_data$Restarted<-Restarted
    ##  2.14. Rejected
    cat("\n\tReject\n")
    Reject<-paradata_files[action=="RejectedBySupervisor"|action=="RejectedByHeadquarter"][,c("var_resp", "rid"):=NULL]
    setnames(Reject, "var", "comment")
    Reject<-droplevels(Reject)
    setkeyv(Reject, "interview__id")
    if(exists("gps_file_merge")) Reject<-gps_file_merge[Reject, on="interview__id"]
    Reject<-KeyAssigned_merge[Reject, on="interview__id"]
    para_data$Reject<-Reject
    ##  PASSIVE EVENTS (ONLY IF REQUESTED)
    if (!onlyActiveEvents) {
      ##  2.11. Invalid
      cat("\n\tQuestionDeclaredInvalid\n")
      QuestionDeclaredInvalid<-paradata_files[action=="QuestionDeclaredInvalid"]
      QuestionDeclaredInvalid<-QuestionDeclaredInvalid[!is.na(interview__id)]
      QuestionDeclaredInvalid[, count:=length(counter), by=interview__id]
      setkeyv(QuestionDeclaredInvalid, "interview__id")
      if(exists("gps_file_merge")) QuestionDeclaredInvalid<-gps_file_merge[QuestionDeclaredInvalid, on="interview__id"]
      QuestionDeclaredInvalid<-KeyAssigned_merge[QuestionDeclaredInvalid, on="interview__id"]
      para_data$QuestionDeclaredInvalid<-QuestionDeclaredInvalid
      ##  2.12. Valid
      cat("\n\tQuestionDeclaredValid\n")
      QuestionDeclaredValid<-paradata_files[action=="QuestionDeclaredValid"]
      QuestionDeclaredValid<-QuestionDeclaredValid[!is.na(interview__id),]
      QuestionDeclaredValid[, count:=length(counter), by=interview__id]
      para_data$QuestionDeclaredValid<-QuestionDeclaredValid
    }
    para_data[["actionDistr"]]<-actionDistr
    para_data[["userDistr"]]<-userDistr
    para_data[["roleDistr"]]<-roleDistr
    cat("\nExport & Transformation finished.\n")
    return(para_data)
  } else {
    ###############################
    ## MULTICORE: SET to 20gb for future parallel
    simu<-length(levels(droplevels(paradata_files$action)))

    options(future.globals.maxSize=15000*1024^2)
    multiCore <- min(simu, multiCore)
    cat("\nStarting Multicore with:\t", multiCore, " cores.\n")
    checkNum(multiCore)
    pack_dp_sp<-c("data.table")
    registerDoFuture()
    plan(multiprocess, workers = multiCore)
    handlers("progress")
    progressr::with_progress({
      p<-progressr::progressor(along = 1:simu)
      para_dataMC<-foreach(i=1:simu, .packages = pack_dp_sp,
                           .combine=c,
                           .multicombine = T,
                           .export = c("para_data"),
                           #.verbose = T,
                           .errorhandling="pass") %dopar% {

                             ## progress
                             #p(sprintf("event = %g", event))
                             event<-levels(paradata_files$action)[i]
                             p(sprintf("i=%g", i))
                             if (event=="AnswerSet"){
                               AnswerSet<-para1_answer
                               AnswerSet<-AnswerSet[!is.na(interview__id)]
                               setkeyv(AnswerSet, "interview__id")
                               if(exists("gps_file_merge"))  AnswerSet<-gps_file_merge[AnswerSet, on="interview__id"]
                               AnswerSet<-KeyAssigned_merge[AnswerSet, on="interview__id"]
                               para_data[[event]]<-AnswerSet
                               rm(AnswerSet)

                             } else if (event=="AnswerRemoved"){
                               ##  2.9. Answer Removed (COUNT the number of Removed answer by questionnaire)
                               AnswerRemoved<-paradata_files[action=="AnswerRemoved"]
                               AnswerRemoved<-AnswerRemoved[!is.na(interview__id)]
                               AnswerRemoved[, count:=length(counter), by=interview__id]
                               AnswerRemoved[,c("responsible", "role"):=NULL]
                               AnswerRemoved<-droplevels(AnswerRemoved)
                               AnswerRemoved<-merge(AnswerRemoved, para1_answer_merge, by="interview__id", allow.cartesian=T)
                               setkeyv(AnswerRemoved, "interview__id")
                               if(exists("gps_file_merge")) AnswerRemoved<-gps_file_merge[AnswerRemoved, on="interview__id"]
                               AnswerRemoved<-KeyAssigned_merge[AnswerRemoved, on="interview__id"]
                               para_data[[event]]<-AnswerRemoved
                               rm(AnswerRemoved)
                             } else if (event=="ApproveByHeadquarter") {
                               ##  2.10. Approved
                               ApproveByHeadquarter<-paradata_files[action=="ApproveByHeadquarter"]
                               ApproveByHeadquarter<-droplevels(ApproveByHeadquarter)
                               ApproveBySupervisor<-paradata_files[action=="ApproveBySupervisor"]
                               ApproveBySupervisor<-droplevels(ApproveBySupervisor)
                               para_data[[event]]<-ApproveBySupervisor
                               rm(ApproveBySupervisor)
                             } else if (event=="QuestionDeclaredInvalid" & !onlyActiveEvents) {
                               ##  2.11. Invalid
                               QuestionDeclaredInvalid<-paradata_files[action=="QuestionDeclaredInvalid"]
                               QuestionDeclaredInvalid<-QuestionDeclaredInvalid[!is.na(interview__id)]
                               QuestionDeclaredInvalid[, count:=length(counter), by=interview__id]
                               setkeyv(QuestionDeclaredInvalid, "interview__id")
                               if(exists("gps_file_merge")) QuestionDeclaredInvalid<-gps_file_merge[QuestionDeclaredInvalid, on="interview__id"]
                               QuestionDeclaredInvalid<-KeyAssigned_merge[QuestionDeclaredInvalid, on="interview__id"]
                               para_data[[event]]<-QuestionDeclaredInvalid
                               rm(QuestionDeclaredInvalid)
                             } else if (event=="QuestionDeclaredValid" & !onlyActiveEvents) {
                               ##  2.12. Valid
                               QuestionDeclaredValid<-paradata_files[action=="QuestionDeclaredValid"]
                               QuestionDeclaredValid<-QuestionDeclaredValid[!is.na(interview__id),]
                               QuestionDeclaredValid[, count:=length(counter), by=interview__id]
                               para_data[[event]]<-QuestionDeclaredValid
                               rm(QuestionDeclaredValid)
                             } else if (event=="Restarted") {
                               ##  2.13 Restarted
                               Restarted<-paradata_files[action=="Restarted"]
                               Restarted<-Restarted[!is.na(interview__id),]
                               Restarted[, count:=length(counter), by=interview__id]
                               setkeyv(Restarted, "interview__id")
                               if(exists("gps_file_merge")) Restarted<-gps_file_merge[Restarted, on="interview__id"]
                               Restarted<-KeyAssigned_merge[Restarted, on="interview__id"]
                               para_data[[event]]<-Restarted
                               rm(Restarted)
                             } else if (event=="Reject") {
                               ##  2.14. Rejected
                               Reject<-paradata_files[action=="RejectedBySupervisor"|action=="RejectedByHeadquarter"][,c("var_resp", "rid"):=NULL]
                               setnames(Reject, "var", "comment")
                               Reject<-droplevels(Reject)
                               #paradata_files<-paradata_files[action!="RejectedBySupervisor"&action!="RejectedByHeadquarter"]
                               #paradata_files<-droplevels(paradata_files)
                               setkeyv(Reject, "interview__id")
                               if(exists("gps_file_merge")) Reject<-gps_file_merge[Reject, on="interview__id"]
                               Reject<-KeyAssigned_merge[Reject, on="interview__id"]
                               para_data[[event]]<-Reject
                               rm(Reject)
                             }
                             return(para_data)
                           }
    })
    para_dataMC[["actionDistr"]]<-actionDistr
    para_dataMC[["userDistr"]]<-userDistr
    para_dataMC[["roleDistr"]]<-roleDistr
    return(para_dataMC)
  }
}
