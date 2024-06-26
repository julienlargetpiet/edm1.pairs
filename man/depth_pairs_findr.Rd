% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/all_fun.R
\name{depth_pairs_findr}
\alias{depth_pairs_findr}
\title{depth_pairs_findr}
\usage{
depth_pairs_findr(inpt)
}
\arguments{
\item{inpt}{is the pair vector}
}
\description{
Takes the pair vector as an input and associate to each pair a level of depth, see examples
}
\examples{

print(depth_pairs_findr(c(1, 1, 2, 3, 3, 4, 4, 2, 5, 6, 7, 7, 6, 5)))

 [1] 1 1 1 2 2 2 2 1 1 2 3 3 2 1

}
