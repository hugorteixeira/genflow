% Generated manually: custom provider registry
\name{list_providers}
\alias{list_providers}
\title{List built-in and custom providers}
\usage{
list_providers(include_builtin = TRUE, include_custom = TRUE)
}
\arguments{
\item{include_builtin}{Logical; include built-in providers.}

\item{include_custom}{Logical; include custom providers.}
}
\value{
A data frame with provider metadata.
}
\description{
Lists providers available in the package, including registered custom
OpenAI-compatible providers.
}
\examples{
list_providers()
}
