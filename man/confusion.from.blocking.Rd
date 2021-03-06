% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/blocking-evaluations.R
\name{confusion.from.blocking}
\alias{confusion.from.blocking}
\title{Perform evaluations (recall) for blocking.}
\usage{
confusion.from.blocking(blocking, true_ids, recall.only = FALSE)
}
\arguments{
\item{blocking}{A list of the blocks}

\item{true_ids}{The true identifiers for comparisons}

\item{recall.only}{Flag that when true only prints the recall, otherwise
prints many evaluation metrics in a list}
}
\value{
A vector of that returns the recall and the precision
}
\description{
Perform evaluations (recall) for blocking.
}
\examples{
data("RLdata500")
klsh.blocks <- klsh(RLdata500, p=20, num.blocks=5, k=2)
confusion.from.blocking(klsh.blocks, identity.RLdata500)
confusion.from.blocking(klsh.blocks, identity.RLdata500, recall.only=TRUE)
}
