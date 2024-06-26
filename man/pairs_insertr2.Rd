% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/all_fun.R
\name{pairs_insertr2}
\alias{pairs_insertr2}
\title{pairs_insertr2}
\usage{
pairs_insertr2(
  inpt,
  algo_used = c(1:3),
  flagged_pair_v = c(")", "]"),
  corr_v = c("(", "["),
  flagged_conj_v = c("&", "|"),
  method = c("(", ")")
)
}
\arguments{
\item{inpt}{is the input character representing an arbitrary condition, like ReGex for example, or information to a parser for example}

\item{algo_used}{is a vector containing one or more of the 3 algorythms used. The first algorythm will simply put the pair of parenthesis at the condition surrounded and/or after a character flagged (in flagged_conj_v) as a conjunction. The second algorythm will put parenthesis at the condition that are located after other conditions that are surrounded by a pair. The third algorythm will put a pair at all the condition, it is very powerfull but takes a longer time. See examples and make experience to see which combination of algorythm(s) is the most efficient for your use case.}

\item{flagged_pair_v}{is a vector containing all the first character of the pairs}

\item{corr_v}{is a vector containing all the last character of the pairs}

\item{flagged_conj_v}{is a vector containing all the conjunction character}

\item{method}{is length 2 vector containing as a first index, the first character of the pair inserted, and at the last index, the second and last character of the pair}
}
\description{
Takes a character representing an arbitrary condition (like ReGeX for example) or an information (to a parser for example), vectors containing all the pair of pattern that potentially surrounds condition (flagged_pair_v and corr_v), and a vector containing all the conjuntion character, as input and returns the character with all or some of the condition surrounded by the pair characters. See examples. All the pair characters are inserted according to the closest pair they found priotizing those found next to the condition and on the same depth-level and , if not found, the pair found at the n+1 depth-level.
}
\examples{

print(pairs_insertr2(inpt = "([one]|two|twob)three(four)", algo_used = c(1), method = c("(", ")")))

[1] "([one]|(two)|(twob))three(four)"

print(pairs_insertr2(inpt = "([one]|two|twob)three(four)", algo_used = c(1), method = c("[", "]")))

[1] "([one]|[two]|[twob])three(four)"

print(pairs_insertr2(inpt = "(oneA|[one]|two|twob)three(four)", algo_used = c(1, 2)))

[1] "(oneA|[one]|(two)|(twob))(three)(four)"

print(pairs_insertr2(inpt = "(oneA|[one]|two|twob)three(four)", algo_used = c(1, 2), method = c("-", "#"),
                     flagged_pair_v = c(")", "]", "#"), corr_v = c("(", "[", "-")))

[1] "(oneA|[one]|-two#|-twob#)-three#(four)"

print(pairs_insertr2(inpt = "(oneA|[one]|two|twob)three(four)", algo_used = c(1, 2, 3)))

[1] "((oneA)|[one]|(two)|(twob))(three)(four)"

print(pairs_insertr2(inpt = "(oneA|[one]|two|twob)three(four)", algo_used = c(3), method = c("[", "]")))

[1] "([oneA]|[one]|[two]|[twob])[three](four)"

print(pairs_insertr2(inpt = "(oneA|[one]|two|twob)three((four))", algo_used = c(3)))

[1] "((oneA)|[one]|(two)|(twob))(three)((four))"

}
