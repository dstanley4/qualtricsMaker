% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/survey_maker.R
\name{make_survey}
\alias{make_survey}
\title{Make a survey}
\usage{
make_survey(survey_codebook, response_sep = ";", filename = NULL)
}
\arguments{
\item{survey_codebook}{survey_codebook A dataframe with the question in required format}

\item{response_sep}{The character for separating response options. Example is semicolon ";". Usage: Yes;No}

\item{filename}{Advanced Text Format filename. End file name with .txt. This is the file you import into Qualtrics.}
}
\value{
text for import
}
\description{
Make a survey
}
\examples{
#make_survey
}
