% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/melting-curve-class.R
\name{MeltingCurve-Method}
\alias{MeltingCurve-Method}
\alias{getDate}
\alias{getDate,MeltingCurve-method}
\alias{getPlate}
\alias{getPlate,MeltingCurve-method}
\alias{getPrimer}
\alias{getPrimer,MeltingCurve-method}
\alias{getData}
\alias{getData,MeltingCurve-method}
\alias{transformData}
\alias{transformData,MeltingCurve-method}
\alias{filterData}
\alias{filterData,MeltingCurve-method}
\alias{tempRange}
\alias{tempRange,MeltingCurve-method}
\alias{show,MeltingCurve-method}
\title{Accessory Methods for \code{MeltingCurve}}
\usage{
getDate(object)

\S4method{getDate}{MeltingCurve}(object)

getPlate(object)

\S4method{getPlate}{MeltingCurve}(object)

getPrimer(object)

\S4method{getPrimer}{MeltingCurve}(object)

getData(object)

\S4method{getData}{MeltingCurve}(object)

transformData(object, ...)

\S4method{transformData}{MeltingCurve}(
  object,
  limit = tempRange(object),
  step = 0.03,
  method = "spline"
)

filterData(object, from = NULL, to = NULL, well_position = NULL, ...)

\S4method{filterData}{MeltingCurve}(object, from, to, well_position)

tempRange(object, ...)

\S4method{tempRange}{MeltingCurve}(object, na.rm = TRUE)

\S4method{show}{MeltingCurve}(object)
}
\arguments{
\item{object}{a S4 class object}

\item{...}{pass to subsequent functions}

\item{limit}{start and stop point in transformation, for example: \code{c(70, 80)}}

\item{step}{increase step in transformation}

\item{method}{method used in transformation}

\item{from}{start point in filter}

\item{to}{stop point in filter}

\item{well_position}{specifying well position in filter}

\item{na.rm}{whether remove NA in calculating temperature range}
}
\description{
Multiple generic methods for \code{MeltingCurve} object.
}
