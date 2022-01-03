\name{cufu_funword}
\alias{cufu_funword}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{cufu_funword
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
cufu_funword(feature,fnames,short_fnames)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{feature}{
featured words in Correspondence analysis
}
  \item{fnames}{
the file names of corpora.
}
  \item{short_fnames}{
the short file names of corpora 'short_names(fnames)'
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
words matrix
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
function(feature,fnames,short_fnames){

  # build a data.frame df with in its row names the features
  df <- data.frame(row.names = feature)

  for (i in 1:length(fnames)) {
    fname <- fnames[[i]]             # identify i-th filename
    short_fname <- short_fnames[[i]] # identify i-th short filename
    flist <- freqlist(fname)         # build frequency list for file
    flist <- flist[feature]         # filter that list to just features
    df[[short_fname]] <- flist        # add column to d named after filename
  }

  df <- df %>%
    as.matrix() %>%
    t()%>%
    drop_empty_rc()

  return(df)
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
