% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qa_qc.R
\name{unknown_qc}
\alias{unknown_qc}
\title{Checks to see if species identified as \code{FinalCode} have matches in the plant list used by Survey123. Returns PlotID FinalCode that doesn't have a match, error and fix columns.}
\usage{
unknown_qc(x, output = FALSE)
}
\arguments{
\item{x}{type list produced by \code{read_dima()} function.}
}
\description{
Checks to see if species identified as \code{FinalCode} have matches in the plant list used by Survey123. Returns PlotID FinalCode that doesn't have a match, error and fix columns.
}
