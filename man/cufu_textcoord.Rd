\name{cufu_textcoord}
\alias{cufu_textcoord}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{cufu_textcoord
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
cufu_textcoord(A_ca, B_ca)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{A_ca}{
output of the performance of correspondence analysis on word matrix A 'A_ca = ca(d_A)'
}
  \item{B_ca}{
output of the performance of correspondence analysis on word matrix B 'B_ca = ca(d_B)'
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
function(A_ca, B_ca){

  text_coord_A <- row_pcoord(A_ca)                # coordinates of texts
  text_coord_B <- row_pcoord(B_ca)

  A_texts_df <- tibble(
    text = A_short_fnames,
    sub_corp = 'First Decade',
    x = text_coord_A[, 1],
    y = text_coord_A[, 2])

  B_texts_df <- tibble(
    text = B_short_fnames,
    sub_corp = 'Second Decade',
    x = text_coord_B[, 1],
    y = text_coord_B[, 2])

  texts_df <- rbind(A_texts_df, B_texts_df)

  return(texts_df)
}
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
