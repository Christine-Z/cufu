\name{cufu_wordcoord}
\alias{cufu_wordcoord}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{cufu_wordcoord
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
cufu_wordcoord(A_ca, B_ca, d_A, d_B)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{A_ca}{
output of the performance of correspondence analysis on word matrix A 'A_ca = ca(d_A)'
}
  \item{B_ca}{
output of the performance of correspondence analysis on word matrix B 'B_ca = ca(d_B)'
}
  \item{d_A}{
word matrix A 'd_A = cufu_funword(features,A_fnames, A_short_fnames)'
}
  \item{d_A}{
word matrix B 'd_B = cufu_funword(features,B_fnames, B_short_fnames)'
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Christine Zhao
}
\note{
This package is built to simplify the code in the final report of Methods of Corpus Linguistics
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

## The function is currently defined as
function(A_ca, B_ca, d_A, d_B){

  A_word_coord <- col_pcoord(A_ca)                # coordinates of function words
  B_word_coord <- col_pcoord(B_ca)

  A_words_df <- tibble(
    word = colnames(d_A),
    x = A_word_coord[, 1],
    y = A_word_coord[, 2])

  B_words_df <- tibble(
    word = colnames(d_B),
    x = B_word_coord[, 1],
    y = B_word_coord[, 2])

  words_df <- rbind(A_words_df, B_words_df)

  return(words_df)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
