#'  SURVEY SOLUTIONS PARADATA EXPORT FUNCTION
#'
#' Exports Survey Solutions Paradata, and returns a data.table.
#'
#'
#'  \code{suso_export_paradata} returns a data.table. Calculates the response time
#'  and separtes multiple responses into individual columns. It also creates a variable
#'  \emph{counter} which preserves the sequence of events.
#' @export



suso_export_paradata<-function(server= suso_get_api_key("susoServer"),
                            apiUser=suso_get_api_key("susoUser"),
                            apiPass=suso_get_api_key("susoPass"),
                            questID="xxxx-xxx-xxxx-xxx-xxx",
                            version=1,
                            format_para="Paradata",
                            user=user(),
                            inShinyServer=FALSE){
  ##  1. Libs
  options(warn = -1)
  ##  2. TEMP file
  aJsonFile<-tempfile(fileext = ".json")
  dataPath<-file.path(tempdir(),"application_data.zip")
  ##  3. Normalize URL
  server<-ifelse(str_count(server, "https://")==1,
                 server, paste0("https://", server))
  server=paste0(server, "/api/v1/export/")
  ##  4. CHECK for update and file
  test_detail<-GET(url = paste0(server, format_para,"/", questID, "$", version, "/", "details"),
                   authenticate(apiUser, apiPass, type = "basic"),
                   write_disk(aJsonFile, overwrite = T))

  test_json<-fromJSON(aJsonFile)
  status<-test_json$HasExportedFile
  # lastUpdate<-data.table(test_json$LastUpdateDate)
  # lastUpdate<-lastUpdate[,tstrsplit(lastUpdate, "T", fixed = T)]
  # lastUpdate[,V2:=tstrsplit(V2, ".", fixed = T, keep = 1L)]
  # names(lastUpdate)<-c("date", "time")
  # if (dir.exists())
  # write_rds()

  ##  Create the FILE (para data)
  if (!status){
  test_post<-POST(url = paste0(server, format_para,"/", questID, "$", version, "/", "start"),
                  authenticate(apiUser, apiPass, type = "basic"), timeout(1200))
  ##  check detail
  test_detail<-GET(url = paste0(server, format_para,"/", questID, "$", version, "/", "details"),
                   authenticate(apiUser, apiPass, type = "basic"),
                   write_disk(aJsonFile, overwrite = T))
  test_json<-fromJSON(aJsonFile)
  print(str(test_json))
  while (!is.null(test_json$RunningProcess)) {
    test_detail<-GET(url = paste0(server, format_para,"/", questID, "$", version, "/", "details"),
                     authenticate(apiUser, apiPass, type = "basic"),
                     write_disk(aJsonFile, overwrite = T))
    test_json<-fromJSON(aJsonFile)
  }

  }
  ##  Download the FILE
  test_exp<-GET(url = paste0(server, format_para,"/", questID, "$", version, "/", ""),
                authenticate(apiUser, apiPass, type = "basic"))
  ## Download from link
  download.file(test_exp$url, dataPath)
  ########################### FILE ############################################
  para_data<-list()

  #############################################################################
  uploadFile<-dataPath
  FILE.list<-tryCatch(unzip(uploadFile, list=T), error=function(e) return("No File Found!"))
  #########################################################################
  ##  Unpack, create data.tables and stor in list, create subsets by action
  #########################################################################
  file.name<-FILE.list[grep(".tab$", FILE.list$Name), 1]
  nfiles<-length(file.name)
  ##  Create final file path

  thefiles<-tempdir()
  zip::unzip(uploadFile, exdir = thefiles)
  ##  Unpack (w. function and lapply)
  prog<-1
  fp<-file.path(thefiles, file.name)
  unpack<-function(fp){
    ####################################################
    ##  1. Use fread from data.table (fastest, needs to be tested for reliability)
    file<-fread(file=fp,
                header = T,
                #autostart = ""
                #skip = 1L,
                fill = T,sep = "\t",
                colClasses = list(character=c(1,3:8),
                                  numeric=c(2)),
                blank.lines.skip=TRUE,
                encoding = "UTF-8")
    if(nrow(file)==0) return(NULL)

    if (length(file)==9) {
      names(file)<-c("interview__id", "counter", "action", "responsible", "role", "time", "tz","var_resp", "rid")
      resps<-file[,tstrsplit(var_resp, "||", fixed=T, names = T, fill = "<NA>")][]
      file[,c("var",paste0("response", 1:(length(resps)-1))):=resps]
      splits <- (length(resps)-1)
    } else {
      names(file)<-c("interview__id", "counter", "action", "responsible", "role", "time", "tz","var_resp")
      resps<-file[,tstrsplit(var_resp, "||", fixed=T, names = T, fill = "<NA>")][]
      resps1<-resps[,tstrsplit(V2, "(\\|)|(,)", fixed=F, names = T, fill = "<NA>")][]
      resps2<-resps[,tstrsplit(V3, "(,)", fixed=F, names = T, fill = "<NA>")][]
      splits <- (length(resps1))
      file[,c("var"):=resps[,.(V1)]]
      file[,c(paste0("response", 1:(length(resps1)))):=resps1]
      file[,c(paste0("rid", 1:(length(resps2)))):=resps2]

    }
    if(inShinyServer) incProgress(amount = 0.25, message = "Data loaded")
    if (nrow(file)==0) return(NULL)
    ####################################################
    ##  3. Create Factors
    print(file)
    file[,interview__id:=as.factor(interview__id)]
    file[,action:=as.factor(action)]
    file[,responsible:=as.factor(responsible)]
    file[,role:=as.factor(role)]
    file[,var:=as.factor(var)]

    for (i in 1:splits) {
      ip<-paste0("response", (i))
      file[,c(ip):=as.factor(get(ip))]
    }
    # file[,response1:=as.factor(response1)]
    # file[,response2:=as.factor(response2)]

    ##  4. DAte
    file[,c("date", "time"):=tstrsplit(time, "T", fixed=TRUE)][]
    file[,time:=as.ITime(time)]
    file[,date:=as.IDate(date)]
    file[,wDAY:=wday(date)]
    file[,mDAY:=mday(date)]
    file[,MONTH:=month(date)]
    file[,WEEK:=isoweek(date)]
    setkeyv(file, c("interview__id", "responsible"))
    return(file)
    prog<-prog+1
  }
  dir(fp)
  paradata_files<-unpack(fp=fp)
  if(inShinyServer) incProgress(amount = 0.25, message = "Transformation completed")
  return(paradata_files)

}
