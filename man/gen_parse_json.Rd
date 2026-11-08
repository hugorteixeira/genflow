\name{gen_parse_json}
\alias{gen_parse_json}
\title{Parse JSON with strict empty array types}
\usage{gen_parse_json(text)}
\arguments{\item{text}{A JSON string.}}
\description{Preserves empty arrays with the genflow.json_array attribute for shared validation.}
\value{Parsed data without simplifying arrays or objects.}
