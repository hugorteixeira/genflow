\name{gen_normalize_object}
\alias{gen_normalize_object}
\title{Prepare native R empty object fields}
\usage{gen_normalize_object(value, fields = character())}
\arguments{\item{value}{An R value.}
\item{fields}{Names of immediate object child fields.}}
\description{Names native empty R lists so they serialize as objects. Explicit JSON arrays from gen_parse_json are preserved. This does not validate or repair invalid values.}
\value{The value with native empty objects named for JSON serialization.}
