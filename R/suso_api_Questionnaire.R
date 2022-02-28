#' Survey Solutions API call for questionnaire
#'
#'
#' \code{suso_getQuestDetails} implements all Questionnaire related API commands. It allows for different operation types,
#' see details bellow for further clarification.
#'
#' @param server Survey Solutions server address
#' @param usr Survey Solutions API user
#' @param pass Survey Solutions API password
#' @param workspace server workspace, if nothing provided, defaults to primary
#' @param token If Survey Solutions server token is provided \emph{usr} and \emph{pass} will be ignored
#' @param quid \emph{QuestionnaireId} for which details should be exported
#' @param version questionnaire version
#' @param operation.type if \emph{list} is specified a list of all questionnaires on the server. If
#' \emph{statuses} a vector of all questionnaire statuses. If \emph{structure} is specified, it returns a list
#' containing all questions, rosters etc. of the specific questionnaire, as well as all validations.
#' If \emph{interviews} is specified, all interviews for a specific questionnaire. See details bellow.
#'
#'
#' @details
#'
#' If list is selected, then list of questionnaires is returned.
#'
#' If statuses is selected, a list of all available questionnaire statuses is returned (deprecated).
#'
#' In case structure is chosen the return value is a list with two data.table elements:
#' \itemize{
#'   \item List element \emph{q} contains all questions, rosters etc.
#'   \item List element \emph{val} contains all validations
#' }
#' In this way it is straightforward to use the returen value for questionnaire manuals and the likes.
#'
#' In case interviews is selected, a list of all interviews for the specific questionnaire is returned.
#'
#' @export
#'
#'

suso_getQuestDetails <- function(server = suso_get_api_key("susoServer"),
                                 usr = suso_get_api_key("susoUser"),
                                 pass = suso_get_api_key("susoPass"),
                                 workspace = NULL,
                                 token = NULL,
                                 quid = NULL, version = NULL,
                                 operation.type = c("list", "statuses", "structure", "interviews")) {
    ## workspace default
    workspace<-.ws_default(ws = workspace)
    ## Set temporary file
    aJsonFile <- tempfile(fileext = ".json")
    ## Define the api
    url <- httr::parse_url(url = server)
    url$scheme <- "https"
    url$path <- file.path(workspace, "api", "v1", "questionnaires")#"primary/api/v1/questionnaires"
    ## Authorization
    auth<-authenticate(usr, pass, type = "basic")
    ## Default operation type is 'list'
    operation.type = match.arg(operation.type, c("list", "statuses", "structure", "interviews"))

    # 1. Get all Questionnaires on the server
    if (operation.type == "list") {
        url$query <- list(limit = 40)
        test_detail <- GET(url = build_url(url),
                           authenticate(usr, pass, type = "basic"),
                           httr::write_disk(aJsonFile, overwrite = T))
        check_response(test_detail)
        test_json <- jsonlite::fromJSON(aJsonFile)
        qTot <- test_json$TotalCount
        ## 1.1. Check if more than 40, append rest
        if (qTot > 40) {
            qTotRest <- qTot
            repCalls <- ceiling(qTotRest/40)
            for (i in 2:repCalls) {
                url$query <- list(limit = 40, offset = i)
                test_detail <- GET(url = build_url(url),
                                   auth,
                                   write_disk(aJsonFile, overwrite = T))
                test_json_tmp <- jsonlite::fromJSON(aJsonFile)
                test_json$Questionnaires <- rbind(test_json$Questionnaires, test_json_tmp$Questionnaires)
            }
        }
        # Only data.table of interviews is returned
        test_json<-data.table(test_json$Questionnaires)
        if(nrow(test_json)==0) return("No data available")
        # Set date time to utc with lubridate
        test_json[,LastEntryDate:=as_datetime(LastEntryDate)][]
        return(test_json)
        # 2. Get all STATUS on server
    } else if (operation.type == "statuses") {
        test_detail <- GET(url = modify_url(url, path = file.path(url$path, "statuses")),
                           auth,
                           write_disk(aJsonFile, overwrite = T))
        check_response(test_detail)
        test_json <- jsonlite::fromJSON(aJsonFile)

        # 3. Get Questionnaire JSON string as list
    } else if (operation.type == "structure") {
        if (is.null(quid) | is.null(version))
            stop("Quid and/or version missing.")
        test_detail <- GET(url = modify_url(url, path = file.path(url$path, quid, version, "document")),
                           auth,
                           write_disk(aJsonFile, overwrite = T))
        check_response(test_detail)
        test_json <- tidyjson::read_json(aJsonFile)
        test_json <- suso_transform_fullMeta(test_json)
        # 4. Get INTERVIEWS for specific questionnaires
    } else if (operation.type == "interviews") {
        if (is.null(quid) | is.null(version))
            stop("Quid and/or version missing.")
        url$query <- list(limit = 40,
                          offset = 1)
        test_detail <- GET(url = modify_url(url, path = file.path(url$path, quid, version, "interviews")),
                           auth,
                           write_disk(aJsonFile, overwrite = T))
        #check_response(test_detail)
        test_json <- jsonlite::fromJSON(aJsonFile)
        qTot <- test_json$TotalCount
        ## 1.1. Check if more than 40, append rest
        if (qTot > 40) {
            qTotRest <- qTot
            repCalls <- ceiling(qTotRest/40)
            for (i in 2:repCalls) {
                url$query <- list(limit = 40, offset = i)
                test_detail <- GET(url = modify_url(url, path = file.path(url$path, quid, version, "interviews")),
                                   authenticate(usr, pass, type = "basic"), write_disk(aJsonFile, overwrite = T))
                test_json_tmp <- jsonlite::fromJSON(aJsonFile)
                test_json$Interviews <- rbind(test_json$Interviews, test_json_tmp$Interviews)
            }
        }
        # Only data.table of interviews is returned
        test_json<-data.table(test_json$Interviews)
        # Set date time to utc with lubridate
        if(nrow(test_json)!=0) test_json[,LastEntryDate:=as_datetime(LastEntryDate)][]
    }
    return(test_json)
    ########################## F I N#########################################################
}


#' Helper function to transform list of questionnair structure to single data.table with all variables
#'
#' Uses tidyjson package
#'
#' \code{suso_transform_fullMeta} transforms the list containing the structure (\emph{operation.type = structure})
#' is transformed into a single data.table
#' with all variable names, types etc.. This also works with json strings manually exported from the server.
#'
#'
#' @param input returned by \code{suso_getQuestDetails} structure operation
#'
#'
#' @export
#' @importFrom tidyjson jstring jnumber jlogical
#' @importFrom tidyjson bind_rows spread_values enter_object gather_array spread_all

suso_transform_fullMeta <- function(input = NULL) {
    ##########################################
    ## v2.1 with validations
    ## IDs:
    ##    L0 = Section, L1=position inside section,
    ##    L2 = Roster/Subsection Nr, (when missing no roster)
    ##    L3 = position inside Roster/Subsection,
    ##    L4 = Roster/Subsection (when missing no roster)
    qfinal <-bind_rows(
        ######################################################
        ## first
        input %>% spread_values(
            Id = jstring("Id"),
            LastEntryDate = jstring("LastEntryDate")
        ) %>% enter_object("Children") %>% gather_array("L0") %>%
            spread_values(
                type = jstring("$type"),
                PublicKey = jstring("PublicKey"),
                Title = jstring("Title")
            ),
        ## second
        input %>% enter_object("Children") %>% gather_array("L0") %>%
            enter_object("Children") %>% gather_array("L1") %>%
            spread_values(
                type = jstring("$type"),
                PublicKey = jstring("PublicKey"),
                Title = jstring("Title"),
                VariableName = jstring("VariableName"),
                QuestionScope = jnumber("QuestionScope"),
                QuestionText = jstring("QuestionText"),
                Featured = jlogical("Featured")

            ),
        ## third
        input %>% enter_object("Children") %>% gather_array("L0") %>%
            enter_object("Children") %>% gather_array("L1") %>%
            enter_object("Children") %>% gather_array("L2") %>%
            spread_values(
                type = jstring("$type"),
                PublicKey = jstring("PublicKey"),
                Title = jstring("Title"),
                VariableName = jstring("VariableName"),
                QuestionScope = jnumber("QuestionScope"),
                QuestionText = jstring("QuestionText"),
                Featured = jlogical("Featured")
            ),
        ## fourth
        input %>% enter_object("Children") %>% gather_array("L0") %>%
            enter_object("Children") %>% gather_array("L1") %>%
            enter_object("Children") %>% gather_array("L2") %>%
            enter_object("Children") %>% gather_array("L3") %>%
            spread_values(
                type = jstring("$type"),
                PublicKey = jstring("PublicKey"),
                Title = jstring("Title"),
                VariableName = jstring("VariableName"),
                QuestionScope = jnumber("QuestionScope"),
                QuestionText = jstring("QuestionText"),
                Featured = jlogical("Featured")
            ),
        ## fifth
        input %>% enter_object("Children") %>% gather_array("L0") %>%
            enter_object("Children") %>% gather_array("L1") %>%
            enter_object("Children") %>% gather_array("L2") %>%
            enter_object("Children") %>% gather_array("L3") %>%
            enter_object("Children") %>% gather_array("L4") %>%
            spread_values(
                type = jstring("$type"),
                PublicKey = jstring("PublicKey"),
                Title = jstring("Title"),
                VariableName = jstring("VariableName"),
                QuestionScope = jnumber("QuestionScope"),
                QuestionText = jstring("QuestionText"),
                Featured = jlogical("Featured")
            ),
        ## sixth
        input %>% enter_object("Children") %>% gather_array("L0") %>%
            enter_object("Children") %>% gather_array("L1") %>%
            enter_object("Children") %>% gather_array("L2") %>%
            enter_object("Children") %>% gather_array("L3") %>%
            enter_object("Children") %>% gather_array("L4") %>%
            enter_object("Children") %>% gather_array("L5") %>%
            spread_values(
                type = jstring("$type"),
                PublicKey = jstring("PublicKey"),
                Title = jstring("Title"),
                VariableName = jstring("VariableName"),
                QuestionScope = jnumber("QuestionScope"),
                QuestionText = jstring("QuestionText"),
                Featured = jlogical("Featured")
            ),
        ## seventh
        input %>% enter_object("Children") %>% gather_array("L0") %>%
            enter_object("Children") %>% gather_array("L1") %>%
            enter_object("Children") %>% gather_array("L2") %>%
            enter_object("Children") %>% gather_array("L3") %>%
            enter_object("Children") %>% gather_array("L4") %>%
            enter_object("Children") %>% gather_array("L5") %>%
            enter_object("Children") %>% gather_array("L6") %>%
            spread_values(
                type = jstring("$type"),
                PublicKey = jstring("PublicKey"),
                Title = jstring("Title"),
                VariableName = jstring("VariableName"),
                QuestionScope = jnumber("QuestionScope"),
                QuestionText = jstring("QuestionText"),
                Featured = jlogical("Featured")
            ),
        ## eight
        input %>% enter_object("Children") %>% gather_array("L0") %>%
            enter_object("Children") %>% gather_array("L1") %>%
            enter_object("Children") %>% gather_array("L2") %>%
            enter_object("Children") %>% gather_array("L3") %>%
            enter_object("Children") %>% gather_array("L4") %>%
            enter_object("Children") %>% gather_array("L5") %>%
            enter_object("Children") %>% gather_array("L6") %>%
            enter_object("Children") %>% gather_array("L7") %>%
            spread_values(
                type = jstring("$type"),
                PublicKey = jstring("PublicKey"),
                Title = jstring("Title"),
                VariableName = jstring("VariableName"),
                QuestionScope = jnumber("QuestionScope"),
                QuestionText = jstring("QuestionText"),
                Featured = jlogical("Featured")
            )
        ###########################################################
    ) %>% dplyr::select_if(col_selector)
    qfinal<-data.table(qfinal)
    ###########################
    ## dynamic use of sprintf
    ##  - do.call and eval
    allSections<-names(qfinal)[grepl("^L[0-7]$", names(qfinal))]
    sprExpr<-paste(rep("%02d", length(allSections)), collapse = "")
    allSections<-paste0(".(", paste(allSections, collapse = ","), ")")
    qfinal[,intID:=do.call(sprintf, c(list(sprExpr), qfinal[,eval(parse(text = allSections))]))]
    qfinal<-qfinal[,document.id:=NULL][]
    ## Get Validations
    valfinal_1<-.suso_transform_fullValid(input = input)
    if(!is.null(valfinal_1)){
        ###########################
        ## dynamic use of sprintf
        ##  - do.call and eval
        ##  - to harmonize ID, ID var is created here!!!
        allSections<-names(valfinal_1)[grepl("^L[0-7]$", names(valfinal_1))]
        sprExpr<-paste(rep("%02d", length(allSections)), collapse = "")
        allSections<-paste0(".(", paste(allSections, collapse = ","), ")")
        ## ID for validations
        valfinal_1[,intID:=do.call(sprintf, c(list(sprExpr), valfinal_1[,eval(parse(text = allSections))]))]
        ## ID for questionnaire
        qfinal[,intID:=do.call(sprintf, c(list(sprExpr), qfinal[,eval(parse(text = allSections))]))]
        qVar<-qfinal[,.(intID, VariableName)]
        valfinal_1<-valfinal_1[,.(intID, Expression, Message, Severity)][]
        setkeyv(valfinal_1, "intID"); setkeyv(qVar, "intID")
        valfinal_1<-valfinal_1[qVar, nomatch=0]
    }
    q_final<-list(q=qfinal, val=valfinal_1)
    return(q_final)
}

