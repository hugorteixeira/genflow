#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
#' @import shiny
#' @importFrom bslib bs_theme
#' @importFrom DT DTOutput datatable renderDT
#' @importFrom htmltools HTML htmlDependencies<- html_print save_html tagAppendChild tagList tags findDependencies
#' @importFrom utils browseURL capture.output head read.csv str write.table
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom httr content POST add_headers http_status upload_file
#' @importFrom magrittr %>%
#' @importFrom readr read_file
#' @importFrom stats runif setNames
## usethis namespace: end
NULL

# Suppress R CMD check notes for non-standard evaluation
utils::globalVariables(c(
  "pricing", "description", "owner", "name", "model", "type",
  "model_id_full", "name_safe", "owner_safe", "prc_value_unquoted",
  "raw_description", "description_clean", "service"
))
