% Generated manually: custom provider registry
\name{rm_provider}
\alias{rm_provider}
\title{Remove a custom provider}
\usage{
rm_provider(id, missing_ok = FALSE)
}
\arguments{
\item{id}{Custom provider id.}

\item{missing_ok}{Logical; when TRUE, returns FALSE if provider does not exist.}
}
\value{
Invisible \code{TRUE} when removed, or \code{FALSE} when missing and
\code{missing_ok = TRUE}.
}
\description{
Removes a custom provider previously registered with
\code{\link[=set_provider_openai_compat]{set_provider_openai_compat()}}.
Built-in providers cannot be removed.
}
\examples{
\dontrun{
rm_provider("mycloud", missing_ok = TRUE)
}
}
