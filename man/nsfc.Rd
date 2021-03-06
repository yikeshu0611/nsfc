% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nsfc.R
\name{nsfc}
\alias{nsfc}
\title{get nsfc items}
\usage{
nsfc(url, header, subject, search, yearStart, yearEnd, itemCategory,
  fundStart, fundEnd, abstract = FALSE)
}
\arguments{
\item{url}{url}

\item{header}{headers}

\item{subject}{subject, ex:H0801}

\item{search}{string to search, ex: "m6A"}

\item{yearStart}{number, year of start}

\item{yearEnd}{number, year of start}

\item{itemCategory}{category}

\item{fundStart}{fund min}

\item{fundEnd}{fund max}

\item{abstract}{a logical argument, default is FLASE}
}
\value{
dataframe
}
\description{
get nsfc items from http://www.sciencenet.cn/, by key
}
\examples{
nsfc(yearStart=2018)
}
