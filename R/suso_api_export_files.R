#'  Survey Solutions API call to generate and download the data
#'
#'  It has the following characteristics.
#'  \itemize{
#'    \item Function returns a LIST with up to 4 different lists
#'    \item Number of lists depends on the level of roster nesting
#'    \item All variable names are transformed to lower case and categorical variables are consistently labeled
#'    \item Consistent id variables are generated with
#'          \itemize{
#'          \item interview__id transformed to id
#'          \item parent ids consistently number starting from id (questionnairid) to idX (maximum id3)
#'          }
#'    \item List elements are returnde as data.tables
#'    \item Allows for specification of reload time (i.e. generation of new download file)
#'    }
#'
#'
#'    @param workStatus define which statuses the file should inlude, if NULL all is exported
#'    @param reloadTimeDiff time difference in hours between last generated file and now
#'    @param inShinyApp if True, file interacts with shiny progress bar
#'    @param n_id_vars  specify the number of identification variables
#'
#'    @export

suso_export<-function(questName="PAPEL - ENCOVI Cuestionario de Listado FINAL",
                      server= suso_get_api_key("susoServer"),
                      apiUser=suso_get_api_key("susoUser"),
                      apiPass=suso_get_api_key("susoPass"),
                      questID="e4de521a-6e32-4ab0-93c5-1fa4e11dc12f",
                      version=2,
                      workStatus="Completed",
                      reloadTimeDiff=1,
                      inShinyApp=F,
                      n_id_vars=11){
  #######################################
  ## Load the libraries
  ##  - all required packages are loaded as dependencies at start-up (-->DESCRIPTION file)

  ########################################
  ## BASE SET-UP
  ##  OPTIONS: i) No scientific notation
  options("scipen"=100, "digits"=4)
  ##  BASE URL
  url<-parse_url((server))
  url$scheme<-"https"
  ##  QUEST ID
  quid=paste0(str_replace_all(questID, "-", ""), "$", version)
  url$path<-file.path("api", "v1", "export", "STATA", quid)
  ##  CREDENTIALS
  usr<-apiUser
  pass<-apiPass


  ######################################################################################
  ##  GET QUESTIONNAIRE DETAILS
  ######################################################################################
  url_quest<-parse_url(server)
  url_quest$path<-"/api/v1/questionnaires"
  test_quest<-GET(url = build_url(url_quest),
                  authenticate(usr, pass, type = "basic"))
  stop_for_status(test_quest, status_code(test_quest))

  aJsonFile<-tempfile()
  writeBin(content(test_quest, "raw"), aJsonFile)
  test_json<-data.table(fromJSON(aJsonFile)$Questionnaires)
  questName<-subset(test_json, QuestionnaireIdentity==quid, Variable)
  questName<-questName$Variable


  ###############################################
  ##          CHECK TIME OF LAST FILE
  ###############################################
  time_limit<-strptime(suso_details(url=server,
                                      usr=apiUser,
                                      pass=apiPass,
                                      quid=questID,
                                      version = version,
                                      format="STATA",
                                      q_name=questName)$LastUpdateDate, format = "%Y-%m-%dT%H:%M:%S")
  ###############################################################################
  ##          START FILE CREATION
  ##              -IFF time diff is larger than treshold,
  ##                  -->then new file is generated (DEFAULT IS 0)
  ###############################################################################
  ##  1. START FILE CREATION --> file is only created when time difference is larger then reloadTimeDiff
  current_time<-strptime(Sys.time(), format = "%Y-%m-%d %H:%M:%S")
  dataPath<-file.path(tempdir(),"application_data.zip")
  print(difftime(current_time, time_limit, units = "hours"))
  if(difftime(current_time, time_limit, units = "hours")>reloadTimeDiff){
    url_start<-modify_url(url, path = file.path(url$path, "start"))
    test_post<-httr::POST(url = url_start, authenticate(usr, pass, type = "basic"))
    stop_for_status(test_post, "Request Failed")
    job_id<-content(test_post)$JobId

    ##########################
    ##  2. STATUS CHECK
    test_json<-suso_details(url=server,
                              usr=apiUser,
                              pass=apiPass,
                              quid=questID,
                              version = version,
                              format="STATA",
                              q_name=questName)


    ## 2.1. Wait until finished
    while (!is.null(test_json$RunningProcess)) {
      print(paste0(test_json$RunningProcess$ProgressInPercents, "%"))
      test_json<-suso_details(url=server,
                                usr=apiUser,
                                pass=apiPass,
                                quid=questID,
                                version = version,
                                format="STATA",
                                q_name=questName)
      Sys.sleep(10)
    }
  }

  ##########################
  ##  3. GET URL AND DOWNLOAD
  test_exp<-GET(url = build_url(url),
                authenticate(usr, pass, type = "basic"),
                write_disk(dataPath, overwrite = TRUE),
                config(                                 # use curl options to:
                  followlocation = 1L,                      # follow redirects
                  unrestricted_auth = 0L,                    # but not pass auth to redirects
                  tcp_keepalive = 1L
                ))
  ## 3.1. Separate by STATUS CODE (302 means data is in s3 bucket)
  if (status_code(test_exp)==302)
    download.file(headers(test_exp)$url, dataPath, quiet = F)




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
  file_list<-tryCatch(unzip(uploadFile, list=T), error=function(e) return("No File Found!"))



  #########################################
  ## Extracting files with loop
  files<-file_list[file_list$Length>0,1]
  files<-files[files!="export__readme.txt"]

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
  CHECKlist<-list()
  for (file_zip in files) {
    name<-strsplit(file_zip, ".dta")[[1]]
    #CHECKlist[[name]]<-read.dta13(file = paste0(tmp_zip_dir,"/", file_zip), fromEncoding = "UTF-8", convert.factors = T, generate.factors = T)
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
    CHECKlist[[name]]<-tmp_file
    ######################################################################
    ##  READING THE FILES
    ##  1. MAIN FILE (defined by user)
    if (name==questName){
      tmpName<-tolower(names(tmp_file))
      names(tmp_file)<-tmpName
      MAINparIDcol<-grep(pattern = "^.+__id$", tmpName)
      main_file<-copy(tmp_file)
      setkeyv(main_file, tmpName[MAINparIDcol])
      setnames(main_file,c("interview__id"),"id")
      attributes(tmp_file)$rosterlevel<-"main"

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
      if (nesting==1 & length(tmp.parIDcol)==2 & nrow(tmp_file)!=0){
        ##  4.1. ROSTER LEVEL 1
        tmp.parIDcol<-names(tmp_file)[tmp.parIDcol]
        id<-paste0(name, "__id")
        parentid1<-tmp.parIDcol[tmp.parIDcol!=id & tmp.parIDcol!="interview__id"]
        names(tmp_file)<-tolower(names(tmp_file))
        tmp_file<-plyr::rename(tmp_file, replace=c("interview__id"="id", setNames("id1", id)))
        tmpName<-tolower(names(tmp_file))
        MAINparIDcol<-grep(pattern = "^id$", tmpName)
        r1id<-grep(pattern = "^id1$", tmpName)
        tmp_file<-data.table(tmp_file, key=tmpName[MAINparIDcol])

        tmp_file<-merge(tmp_file, statusControl, by=c("id", "interview__key"), allow.cartesian=F)
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
        id<-paste0(name, "__id")
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
      } else if (nesting==2& nrow(tmp_file)!=0) {
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
  sectionR1<-sapply(names(file_collector.rost.L1), function(x) strsplit(x, "_")[[1]][1], simplify = T)
  attributes(sectionR1)<-NULL
  sectionR2<-sapply(names(file_collector.rost.L2), function(x) strsplit(x, "_")[[1]][1], simplify = T)
  attributes(sectionR2)<-NULL

  ##  2. Merge them
  if(is.matrix(sectionR1) & is.matrix(sectionR2)){
    R1inR2<-as.data.frame(stringdistmatrix(sectionR1, sectionR2, method = "osa"))
    if (nrow(R1inR2)!=0){
      R1inR2match<-lapply(1:length(R1inR2[,1]), function(i) which(R1inR2[i,]==0) )
      for (i in 1:length(file_collector.rost.L1)){
        if (length(R1inR2match[[i]])!=0){
          for (k in R1inR2match[[i]]) {
            file_collector.rost.L1[[i]]<-plyr::join(file_collector.rost.L1[[i]], file_collector.rost.L2[[k]],
                                                    by=c("id", "id1"))
          }
        }
      }
    }} else if (is.matrix(sectionR1)) {
      for (i in 1:length(file_collector.rost.L1)){
        file_collector.rost.L1[[i]]<-file_collector.rost.L1[[i]]
      }

    }
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
  #return(CHECKlist)
  return(file_collector)


  ################################################ FIN ######################################################
}
