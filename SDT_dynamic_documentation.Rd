
\name{SDT_function}
\alias{SDT_function}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
function to make a signal detection plot
}
\description{
Plots two standard distributions with criterion, d prime and likelyhoods for signal and noise_mean
}
\usage{
SDT(criterion, signal_mean, noise_mean, input_sigma, = 1)
}
\arguments{
  \item{criterion}{single number, integer or float
}
\item{signal_mean}{single number, integer or float
}

\item{noise_mean}{single number, integer or float
}

\item{input_sigma}{single number, integer or float, default = 1
}

\item{color_arguments}{
  # likelihood_line_col = 'gray31', 
  # col1= rgb(70/255, 130/255, 180/255, .85)
  # col2= rgb(135/255, 206/255, 235/255, 0.35)
  # criterion_col = "pink3"
  # line_col = "black"
  }
}
  
\details{
 criterion, signal_mean, noise_mean are necessary, sigma (standard deviation) is set at 1. colors are at the moment hard coded in the function, criterion usually lies between signal and noise mean. # function only works for positive values of signal and noise, noiseis to be expected to be smaller then the signal, Function plots from zero up to signal_mean + sigma + 3
}
\value{
empty
}
\references{
empty
}
\author{
Tu???ng Vân Spiritie
Roelof Roessingh
}
\note{
empty
}

\seealso{
empty
}
\examples{
SDT(1.5, 1, 2)

c = 1
sm = -1
nm = 2
sigma 5
SDT(c, sm , nm, sigma)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
