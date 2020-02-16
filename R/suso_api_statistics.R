#' Survey Solutions API call for variable specific statistics
#' ATTENTION: Test Version, will be updated soon
#'
#'
#'
#' @export

suso_getStatsQuestionnaire <- function(server = suso_get_api_key("susoServer"), apiUser = suso_get_api_key("susoUser"), apiPass = suso_get_api_key("susoPass"),
    questID = "", version = "", qQuest = "", qExp = "Csv") {

    ## Load the libraries
    library("httr")
    library("xml2")
    library("jsonlite")
    questID <- str_remove_all(questID, "-")
    server <- ifelse(str_count(server, "https://") == 1, server, paste0("https://", server))
    aJsonFile <- tempfile(fileext = paste0(".", tolower(qExp)))
    ## Define the api
    server = paste0(server, "/api/v1/statistics?")
    questID <- paste0("query.questionnaireId=", questID)
    version <- paste0("&query.version=", version)
    qQuest <- paste0("&query.question=", qQuest)
    qExp <- paste0("&query.exportType=", qExp)
    server <- paste0(server, questID, version, qQuest, qExp, "&query.pivot=false&query.expandTeams=true")
    print(server)
    test_detail <- GET(url = server, authenticate(apiUser, apiPass, type = "basic"))


    writeBin(test_detail$content, aJsonFile)
    test_json <- read.csv(aJsonFile)
    return(test_json)
}

# a<-getStatsQuestionnaire('mcw2.mysurvey.solutions', 'bahamaAPI0202', 'Bahamas2020', '420791ebbd1e47868ce4e27b098c34f8',
# 3, 'HL1a_PrivDwel_Inst')

suso_getQuestionsQuestionnaire <- function(server = suso_get_api_key("susoServer"), apiUser = suso_get_api_key("susoUser"), apiPass = suso_get_api_key("susoPass"),
    questID = "", version = "") {
    questID <- str_remove_all(questID, "-")
    server <- ifelse(str_count(server, "https://") == 1, server, paste0("https://", server))
    ## Define the api
    server = paste0(server, "/api/v1/statistics/questions?")
    questID <- paste0("questionnaireId=", questID)
    version <- paste0("&version=", version)
    server <- paste0(server, questID, version)
    test_detail <- GET(url = server, authenticate(apiUser, apiPass, type = "basic"))
    aJsonFile <- tempfile(fileext = ".json")
    writeBin(test_detail$content, aJsonFile)
    test_json <- fromJSON(aJsonFile)
    return(test_json)
}

# a<-getQuestionsQuestionnaire('mcw2.mysurvey.solutions', 'bahamaAPI0202', 'Bahamas2020',
# '420791ebbd1e47868ce4e27b098c34f8', 3)

########################################### EXTRACT table from site

"https://mcw2.mysurvey.solutions/api/ReportDataApi/
HeadquarterSurveysAndStatusesReport?ResponsibleName=&draw=1&
columns[0][data]=questionnaireTitle&columns[0][name]=QuestionnaireTitle&
columns[1][data]=supervisorAssignedCount&columns[1][name]=SupervisorAssignedCount&
columns[2][data]=interviewerAssignedCount&columns[2][name]=InterviewerAssignedCount&
columns[3][data]=completedCount&columns[3][name]=CompletedCount&
columns[4][data]=rejectedBySupervisorCount&columns[4][name]=RejectedBySupervisorCount&
columns[5][data]=approvedBySupervisorCount&columns[5][name]=ApprovedBySupervisorCount&
columns[6][data]=rejectedByHeadquartersCount&columns[6][name]=RejectedByHeadquartersCount&
columns[7][data]=approvedByHeadquartersCount&columns[7][name]=ApprovedByHeadquartersCount&
columns[8][data]=totalCount&columns[8][name]=TotalCount&order[0][column]=0&order[0][dir]=
asc&start=0&length=20&search[value]=&search[regex]=false&exportType=csv"
## teams and stauses
"https://mcw2.mysurvey.solutions/api/ReportDataApi/HeadquarterSupervisorsAndStatusesReport?
draw=6&order[0][column]=0&order[0][dir]=asc&
order[0][name]=Responsible&start=0&length=20&search[value]=&
search[regex]=false&templateId=420791ebbd1e47868ce4e27b098c34f8&
templateVersion=3&exportType=csv"















