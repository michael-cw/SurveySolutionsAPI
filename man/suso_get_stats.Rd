% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/suso_api_statistics.R
\name{suso_get_stats}
\alias{suso_get_stats}
\title{Survey Solutions API call for Summary Tables}
\usage{
suso_get_stats(
  server = suso_get_api_key("susoServer"),
  apiUser = suso_get_api_key("susoUser"),
  apiPass = suso_get_api_key("susoPass"),
  workspace = NULL,
  token = NULL,
  questID = "",
  version = "",
  qQuest = "",
  byTeam = TRUE
)
}
\arguments{
\item{server}{Survey Solutions server address}

\item{apiUser}{Survey Solutions API user}

\item{apiPass}{Survey Solutions API password}

\item{workspace}{server workspace, if nothing provided, defaults to primary}

\item{token}{If Survey Solutions server token is provided \emph{apiUser} and \emph{apiPass} will be ignored}

\item{questID}{only assignments for \emph{QuestionnaireId} are returned, requires \code{version} being not NULL}

\item{version}{version of the questionnaire, only required with \code{questID}}

\item{qQuest}{provide \emph{QuestionnaireId} and \emph{version} to receive all questions and responses for a specific questionnaire}

\item{byTeam}{should the table contain reports by team}
}
\description{
Returns summary tables for individual questions. If no responses had been provided, an empty table will be returned
}
