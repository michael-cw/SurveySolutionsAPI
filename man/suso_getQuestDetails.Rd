% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/suso_api_Questionnaire.R
\name{suso_getQuestDetails}
\alias{suso_getQuestDetails}
\title{Survey Solutions API call for questionnaire}
\usage{
suso_getQuestDetails(
  server = suso_get_api_key("susoServer"),
  usr = suso_get_api_key("susoUser"),
  pass = suso_get_api_key("susoPass"),
  workspace = NULL,
  token = NULL,
  quid = NULL,
  version = NULL,
  operation.type = c("list", "statuses", "structure", "interviews")
)
}
\arguments{
\item{server}{Survey Solutions server address}

\item{usr}{Survey Solutions API user}

\item{pass}{Survey Solutions API password}

\item{workspace}{server workspace, if nothing provided, defaults to primary}

\item{token}{If Survey Solutions server token is provided \emph{usr} and \emph{pass} will be ignored}

\item{quid}{\emph{QuestionnaireId} for which details should be exported}

\item{version}{questionnaire version}

\item{operation.type}{if \emph{list} is specified a list of all questionnaires on the server. If
\emph{statuses} a vector of all questionnaire statuses. If \emph{structure} is specified, it returns a list
containing all questions, rosters etc. of the specific questionnaire, as well as all validations.
If \emph{interviews} is specified, all interviews for a specific questionnaire. See details bellow.}
}
\description{
\code{suso_getQuestDetails} implements all Questionnaire related API commands. It allows for different operation types,
see details bellow for further clarification.
}
\details{
If list is selected, then list of questionnaires is returned.

If statuses is selected, a list of all available questionnaire statuses is returned (deprecated).

In case structure is chosen the return value is a list with two data.table elements:
\itemize{
  \item List element \emph{q} contains all questions, rosters etc.
  \item List element \emph{val} contains all validations
}
In this way it is straightforward to use the returen value for questionnaire manuals and the likes.

In case interviews is selected, a list of all interviews for the specific questionnaire is returned.
}
