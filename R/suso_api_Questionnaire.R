#' Survey Solutions API call for questionnaire
#'
#'
#' \code{suso_getQuestDetails} implements all Questionnaire API commands. If \emph{operation.type = structure}
#' the return file will be a list, whith the content of the raw json string containing the questionnaire variables
#'
#' @param operation.type if \emph{list} is specified a list of all questionnaires on the server. If
#' \emph{statuses} a vector of all questionnaire statuses. If \emph{structure} is specified, it returns a list
#' containing the content of the questionnaire's json string. To make use of it, put the output into
#' \code{suso_transform_fullMeta} and receive a data.table with all questionnaire elements. If \emph{interviews}, all interviews
#' for a specific questionnaire.
#' @export

suso_getQuestDetails <- function(url=suso_get_api_key("susoServer"), usr = suso_get_api_key("susoUser"), pass = suso_get_api_key("susoPass"),
    quid = NULL, version = NULL, limit = 40, offset = 0, operation.type = c("list", "statuses", "structure", "interviews")) {
    ## Set temporary file
    aJsonFile <- tempfile(fileext = ".json")
    ## Define the api
    url <- parse_url(url)
    url$scheme <- "https"
    url$path <- "/api/v1/questionnaires"
    ## Default operation type is 'list'
    operation.type = ifelse(is.null(operation.type), "list", operation.type)

    ################################################################################ 1. Get all Questionnaires on the server
    if (operation.type == "list") {
        url$query <- list(limit = 40)
        test_detail <- GET(url = build_url(url), authenticate(usr, pass, type = "basic"), write_disk(aJsonFile, overwrite = T))
        test_json <- fromJSON(aJsonFile)
        qTot <- test_json$TotalCount
        ## 2. Check if more than 40, append rest
        if (qTot > 40) {
            qTotRest <- qTot
            repCalls <- ceiling(qTotRest/40)
            for (i in 2:repCalls) {
                url$query <- list(limit = 40, offset = i)
                test_detail <- GET(url = build_url(url), authenticate(usr, pass, type = "basic"), write_disk(aJsonFile, overwrite = T))
                test_json_tmp <- fromJSON(aJsonFile)
                test_json$Questionnaires <- rbind(test_json$Questionnaires, test_json_tmp$Questionnaires)
            }
        }

        ################################################################################ 2. Get all STATUS on server
    } else if (operation.type == "statuses") {
        test_detail <- GET(url = modify_url(url, path = file.path(url$path, "statuses")), authenticate(usr, pass, type = "basic"),
            write_disk(aJsonFile, overwrite = T))
        test_json <- fromJSON(aJsonFile)

        ################################################################################ 4. Get Questionnaire JSON string -->contains all sections/variables/rosters -->to get final dataframe use
        ################################################################################ suso_transform_full_Meta()
    } else if (operation.type == "structure") {
        if (is.null(quid) | is.null(version))
            stop("Quid and/or version missing.")
        test_detail <- GET(url = modify_url(url, path = file.path(url$path, quid, version, "document")), authenticate(usr,
            pass, type = "basic"), write_disk(aJsonFile, overwrite = T))
        test_json <- jsonlite::fromJSON(aJsonFile)  #, flatten = T, simplifyDataFrame = T)
        ################################################################################ 5. INTERVIEWS
    } else if (operation.type == "interviews") {
        if (is.null(quid) | is.null(version))
            stop("Quid and/or version missing.")
        test_detail <- GET(url = modify_url(url, path = file.path(url$path, quid, version, "interviews")),
                           authenticate(usr, pass, type = "basic"), write_disk(aJsonFile, overwrite = T))
        test_json <- fromJSON(aJsonFile)
    }

    return(test_json)
    ########################## F I N#########################################################
}


#' Transforms list of questionnair structure to single data.table with all variables
#'
#'
#' \code{suso_transform_fullMeta} transforms the list containing the structure (\emph{operation.type = structure})
#' is transformed into a single data.table
#' with all variable names, types etc.
#'
#'
#' @param input returned by \code{suso_getQuestDetails} structure operation
#'
#' @export

suso_transform_fullMeta <- function(input = NULL) {
    ## Get section names
    secNames <- input$Children$Title

    qstr <- list()
    nSection <- length(secNames)
    for (i in 1:nSection) {
        ## 1.1. Check for roster on Top level, if no roster drop all sections with text and create dt.
        checkName <- names(input$Children$Children[[i]])
        # drop text only sections rosterpos<-grep(checkName, pattern = 'IsRoster')
        rosterpos <- sum(input$Children$Children[[i]]$`$type` == "Group")
        if (length(checkName) <= 11 & length(rosterpos) == 0)
            (next)()
        if (length(rosterpos) == 0) {
            if (is.na(secNames[i]))
                (next)()
            tmp_q <- rbind(input$Children$Children[[i]])
            if (is.null(tmp_q))
                (next)()
            tmp_q$Properties <- NULL
            tmp_q <- data.table(tmp_q)
            tmp_q[, `:=`(c("Children"), NULL)]

            tmp_q <- tmp_q[]
            qstr[[secNames[i]]] <- tmp_q
        } else {
            if (is.na(secNames[i]))
                (next)()
            tmp_q <- rbind(input$Children$Children[[i]])
            if (is.null(tmp_q))
                (next)()
            tmp_q$Properties <- NULL
            secNames1 <- tmp_q$Title
            tmp_q <- data.table(tmp_q)
            if (is.null(secNames1)) {
                tmp_q
                tmp_q[, `:=`(c("Children"), NULL)]
                tmp_q <- tmp_q[]
                qstr[[paste0(secNames[i], "_")]] <- tmp_q
                (next)()
            }
            nSection1 <- length(secNames1)
            for (i1 in 1:nSection1) {
                if (is.na(secNames1[i1])) {
                  tq <- copy(tmp_q)
                  tq[, `:=`(c("Children"), NULL)]
                  tq <- tq[]
                  qstr[[paste0(secNames[i], i1, "_")]] <- tq[i1]
                  (next)()
                }
                checkName1 <- names(tmp_q$Children[[i1]])
                # rosterpos1<-grep(checkName1, pattern = 'IsRoster')
                rosterpos1 <- sum(tmp_q$Children[[i1]]$`$type` == "Group")
                if (length(rosterpos1) == 0) {
                  tmp_q1 <- (rbind(tmp_q$Children[[i1]]))
                  if (is.null(checkName1))
                    tmp_q1 <- copy(tmp_q)
                  if (length(checkName1) == 0)
                    tmp_q1 <- copy(tmp_q)
                  if (is.null(tmp_q1))
                    (next)()
                  tmp_q1$Properties <- NULL
                  tmp_q1 <- data.table(tmp_q1)
                  tmp_q1[, `:=`(c("Children"), NULL)]
                  tmp_q1 <- tmp_q1[]
                  qstr[[paste0(secNames[i], "_", secNames1[i1])]] <- tmp_q1
                } else {
                  if (is.na(secNames1[i1])) {
                    tq <- copy(tmp_q)
                    tq[, `:=`(c("Children"), NULL)]
                    tq <- tq[]
                    qstr[[paste0(secNames[i], i1, "_")]] <- tq[i1]
                    (next)()
                  }
                  tmp_q1 <- (rbind(tmp_q$Children[[i1]]))
                  if (is.null(tmp_q1))
                    (next)()
                  tmp_q1$Properties <- NULL
                  secNames2 <- tmp_q1$Title
                  tmp_q1 <- data.table(tmp_q1)
                  if (is.null(secNames2)) {
                    tmp_q1[, `:=`(c("Children"), NULL)]
                    tmp_q1 <- tmp_q1[]
                    qstr[[paste0(secNames[i], "_", secNames1[i1], "_")]] <- tmp_q1
                    (next)()
                  }
                  nSection2 <- length(secNames2)
                  for (i2 in 1:nSection2) {
                    checkName2 <- names(tmp_q1$Children[[i2]])
                    # rosterpos2<-grep(checkName2, pattern = 'IsRoster')
                    rosterpos2 <- sum(tmp_q1$Children[[i2]]$`$type` == "Group")
                    if (length(rosterpos2) == 0) {
                      if (is.na(secNames2[i2])) {
                        tq <- copy(tmp_q1)
                        tq[, `:=`(c("Children"), NULL)]
                        tq <- tq[]
                        qstr[[paste0(secNames[i], "_", secNames1[i1], i2, "_")]] <- tq[i2]
                        (next)()
                      }
                      tmp_q2 <- (rbind(tmp_q1$Children[[i2]]))
                      if (is.null(tmp_q2))
                        (next)()
                      tmp_q2$Properties <- NULL
                      tmp_q2 <- data.table(tmp_q2)
                      tmp_q2[, `:=`(c("Children"), NULL)]
                      if (length(checkName2) == 0)
                        tmp_q2 <- copy(tmp_q1)

                      tmp_q2 <- tmp_q2[]
                      qstr[[paste0(secNames[i], "_", secNames1[i1], "_", secNames2[i2])]] <- tmp_q2
                    } else {
                      if (is.na(secNames2[i2])) {
                        tq <- copy(tmp_q1)
                        tq[, `:=`(c("Children"), NULL)]
                        tq <- tq[]
                        qstr[[paste0(secNames[i], "_", secNames1[i1], i2, "_")]] <- tq[i2]
                        (next)()
                      }
                      tmp_q2 <- (rbind(tmp_q1$Children[[i2]]))
                      if (is.null(tmp_q2))
                        (next)()
                      tmp_q2$Properties <- NULL
                      secNames3 <- tmp_q2$Title
                      tmp_q2 <- data.table(tmp_q2)
                      if (is.null(secNames3)) {
                        tmp_q2[, `:=`(c("Children"), NULL)]
                        tmp_q2 <- tmp_q2[]
                        qstr[[paste0(secNames[i], "_", secNames1[i1], "_", secNames2[i2], "_")]] <- tmp_q2
                        (next)()
                      }
                      nSection3 <- length(secNames3)
                      for (i3 in 1:nSection3) {
                        checkName3 <- names(tmp_q2$Children[[i3]])
                        if (is.na(secNames3[i3])) {
                          tq <- copy(tmp_q2)
                          tq[, `:=`(c("Children"), NULL)]
                          tq <- tq[]
                          qstr[[paste0(secNames[i], "_", secNames1[i1], "_", secNames2[i2], i3, "_")]] <- tq[i3]
                          (next)()
                        }
                        # rosterpos3<-grep(checkName3, pattern = 'IsRoster')
                        rosterpos3 <- sum(tmp_q2$Children[[i3]]$`$type` == "Group")
                        if (length(rosterpos3) == 0) {
                          tmp_q3 <- copy(tmp_q2)
                          # tmp_q3<-(rbind(tmp_q2$Children[[i3]]))
                          if (is.null(tmp_q3))
                            (next)()
                          tmp_q3$Properties <- NULL
                          tmp_q3 <- data.table(tmp_q3)
                          tmp_q3[, `:=`(c("Children"), NULL)]
                          if (length(checkName3) == 0)
                            tmp_q3 <- copy(tmp_q2)
                          tmp_q3 <- tmp_q3[]
                          qstr[[paste0(secNames[i], "_", secNames1[i1], "_", secNames2[i2], "_", secNames3[i3])]] <- tmp_q3
                        } else {
                          if (is.na(secNames3[i3])) {
                            tq <- copy(tmp_q2)
                            tq[, `:=`(c("Children"), NULL)]
                            tq <- tq[]
                            qstr[[paste0(secNames[i], "_", secNames1[i1], "_", secNames2[i2], i3, "_")]] <- tq[i3]
                            (next)()
                          }
                          tmp_q3 <- (rbind(tmp_q2$Children[[i3]]))
                          tmp_q3$Properties <- NULL
                          tmp_q3 <- data.table(tmp_q3)
                          checkName4 <- (names(tmp_q3))
                          secNames4 <- tmp_q3$Title
                          if (is.null(secNames4)) {
                            tmp_q3[, `:=`(c("Children"), NULL)]
                            tmp_q3 <- tmp_q3[]
                            qstr[[paste0(secNames[i], "_", secNames1[i1], "_", secNames2[i2], "_", secNames3[i3], "_")]] <- tmp_q3
                            (next)()
                          }
                          nSection4 <- length(secNames4)
                          for (i4 in 1:nSection4) {
                            if (is.na(secNames4[i4]))
                              (next)()
                            checkName4 <- (names(tmp_q3))
                            # rosterpos4<-grep(checkName4, pattern = 'IsRoster')
                            rosterpos4 <- sum(tmp_q3$Children[[i4]]$`$type` == "Group")
                            if (length(rosterpos4) == 0) {
                              tmp_q4 <- (rbind(tmp_q3$Children[[i4]]))
                              tmp_q4$Properties <- NULL
                              tmp_q4 <- data.table(tmp_q4)
                              if (length(checkName4) == 0)
                                tmp_q4 <- copy(tmp_q3)
                              if (is.null(tmp_q4))
                                (next)()
                              tmp_q4[, `:=`(c("Children"), NULL)]
                              tmp_q4 <- tmp_q4[]
                              qstr[[paste0(secNames[i], "_", secNames1[i1], "_", secNames2[i2], "_", secNames3[i3], "_",
                                secNames4[i4])]] <- tmp_q4

                            } else {
                              ############################################################
                              if (is.na(secNames4[i4])) {
                                tq <- copy(tmp_q3)
                                tq[, `:=`(c("Children"), NULL)]
                                tq <- tq[]
                                qstr[[paste0(secNames[i], "_", secNames1[i1], "_", secNames2[i2], "_", secNames3[i3], i4,
                                  "_")]] <- tq[i4]
                                (next)()
                              }
                              tmp_q4 <- (rbind(tmp_q3$Children[[i4]]))
                              tmp_q4$Properties <- NULL
                              tmp_q4 <- data.table(tmp_q4)
                              checkName5 <- (names(tmp_q4))
                              secNames5 <- tmp_q4$Title
                              if (is.null(secNames5)) {
                                tmp_q4[, `:=`(c("Children"), NULL)]
                                tmp_q4 <- tmp_q4[]
                                qstr[[paste0(secNames[i], "_", secNames1[i1], "_", secNames2[i2], "_", secNames3[i3], "_",
                                  secNames4[i4], "_")]] <- tmp_q4
                                (next)()
                              }
                              nSection5 <- length(secNames5)
                              for (i5 in 1:nSection5) {
                                if (is.na(secNames5[i5]))
                                  (next)()
                                checkName5 <- (names(tmp_q4))
                                # rosterpos4<-grep(checkName4, pattern = 'IsRoster')
                                rosterpos5 <- sum(tmp_q4$Children[[i5]]$`$type` == "Group")
                                if (length(rosterpos5) == 0) {
                                  tmp_q5 <- (rbind(tmp_q4$Children[[i5]]))
                                  tmp_q5$Properties <- NULL
                                  tmp_q5 <- data.table(tmp_q5)
                                  if (length(checkName5) == 0)
                                    tmp_q5 <- copy(tmp_q4)
                                  if (is.null(tmp_q5))
                                    (next)()
                                  tmp_q5[, `:=`(c("Children"), NULL)]
                                  tmp_q5 <- tmp_q5[]
                                  qstr[[paste0(secNames[i], "_", secNames1[i1], "_", secNames2[i2], "_", secNames3[i3], "_",
                                    secNames4[i4], "_", secNames5[i5])]] <- tmp_q5
                                }
                              }
                            }
                          }

                        }
                      }

                    }

                  }
                }
            }
        }
    }
    qstr <- rbindlist(qstr, idcol = "Section", fill = T)
    ## Check for variable presence: 1. Featured qstr<-qstr[,.(Section, QuestionType, PublicKey,Featured,QuestionScope
    ## ,QuestionText, VariableName)]
    return(qstr)
}






################ Testing not run qestStruct<-getQuestDetails(quid = 'b4c78852-c1d7-4532-ba3f-7cc35ead489a', version = 5, operation.type
################ = 'structure') a<-fullMeta()

# b<-rbindlist(a, idcol = 'section') ## to check q47 qestList<-data.table(getQuestDetails()$Questionnaires)
# fullMETAlist<-list() for (q in 1:nrow(qestList)) { print(q) tmp_q<-qestList[q] qestStruct<-getQuestDetails(quid =
# tmp_q$QuestionnaireId, version = tmp_q$Version, operation.type = 'structure')
# fullMETAlist[[qestStruct$Title]]<-fullMeta(qestStruct) }
