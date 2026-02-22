% Generated manually: custom provider registry
\name{get_provider}
\alias{get_provider}
\title{Get provider definition}
\usage{
get_provider(id)
}
\arguments{
\item{id}{Provider id.}
}
\value{
A list with provider definition details.
}
\description{
Returns metadata for a built-in provider or full configuration for a custom
provider.
}
\examples{
\dontrun{
get_provider("openai")
get_provider("mycloud")
}
}
