% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/search.R
\name{search}
\alias{search}
\title{get nsfc abstract}
\usage{
search(url, header, search, subject, yearStart, yearEnd, year_ascend,
  itemCategory, fundStart, fundEnd)
}
\arguments{
\item{url}{url}

\item{header}{header}

\item{search}{string to search, ex: "m6A"}

\item{subject}{subject, ex:H0801}

\item{yearStart}{number, year of start}

\item{yearEnd}{number, year of start}

\item{year_ascend}{a logical argument, default is TRUE, ordre by year ascending}

\item{itemCategory}{category}

\item{fundStart}{fund min}

\item{fundEnd}{fund max}
}
\value{
dataframe
}
\description{
get nsfc abstract from http://www.sciencenet.cn/, by key
}
\examples{
search(search='m6A',yearStart=2014,yearEnd=2014)
}
