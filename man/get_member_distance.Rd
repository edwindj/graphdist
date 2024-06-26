% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_member_distance.R
\name{get_member_distance}
\alias{get_member_distance}
\title{Assess for a set of nodes how separated they are in a large network.}
\usage{
get_member_distance(edges, from, max_distance = 3)
}
\arguments{
\item{edges}{\code{data.frame} or a \verb{[sparseMatrix()]} with a \verb{$from} and \verb{$to} column with node ids.}

\item{from}{\code{character} list of node ids that are considered a member}

\item{max_distance}{\code{integer} maximum distance at which the calculation stops.}
}
\description{
This method can be used to find how many neighbors there are at distance d
for each node in \code{from}. Furthermore it also return the number of neighbors that
are part of the set \code{from}.
}
