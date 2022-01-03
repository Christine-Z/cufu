\name{cufu_freqlist}
\alias{cufu_freqlist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{cufu_freqlist
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
cufu_freqlist(X_fnames, stop_list)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X_fnames}{
the file names of corpora.
}
  \item{stop_list}{
words that need to be removed out of analysis.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
output a words frequency list
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
## A_fnames <-    get_fnames(".../foldername")
## A_flist <- cufu_freqlist(A_fnames,stop_list)

## The function is currently defined as
function(X_fnames, stop_list) {
  # build frequency list for target corpus
  # [use whitespace as token splitter]
  # [drop tokens containing ":", "[", or "]" ]
  X_flist <- X_fnames %>%
    freqlist(re_token_splitter = r"--[(?xi)  \s+   ]--",
             re_drop_token     = r"--[(?xi)  [:\[\]] ]--",
             file_encoding     = "UTF-8")
  # inspecting the top freq. items (after dropping stop words)
  # [resorting to concordances when the need arises]
  X_flist <- X_flist %>%
    drop_types(stop_list)
  # return list
  return(X_flist)
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
