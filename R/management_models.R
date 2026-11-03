# Model metadata and favorites; no presentation or Shiny dependencies.
#' Read full model catalog data without printing or truncating descriptions
#' @param provider Optional provider identifiers.
#' @param directory Catalog directory. Defaults to `genflow.models_dir`, then
#'   `tools::R_user_dir("agent_models", "data")`.
#' @return A data frame with `service`, `model`, `type`, `pricing`, `description`
#'   and `source_file`. Empty catalogs retain these columns. No remote calls.
#' @export
gen_models <- function(provider = NULL, directory = NULL) .gen_management({
  directory <- .gen_management_path(directory, "directory")
  if (!is.null(provider)) {
    .gen_management_assert(is.character(provider) && !anyNA(provider) &&
      all(nzchar(trimws(provider))), "provider")
    provider <- vapply(provider, .genflow_normalize_service_alias, character(1))
  }
  rows <- .genflow_model_catalog(directory)
  fields <- c("service", "model", "type", "pricing", "description", "source_file")
  if (!nrow(rows)) rows <- as.data.frame(stats::setNames(rep(list(character()), length(fields)), fields))
  if (!is.null(provider)) rows <- rows[rows$service %in% provider, , drop = FALSE]
  rownames(rows) <- NULL
  rows
})

#' Read or replace favorite model selections
#' @param models Optional data frame with `service`, `model` and optional `type`.
#'   NULL reads existing selections. An empty data frame clears the selection.
#' @param directory Catalog and favorites directory, as in [gen_models()].
#' @param catalog Optional catalog already read with [gen_models()].
#' @param save Persist supplied selections. FALSE only normalizes the data.
#' @return A normalized data frame with `service`, `model`, and `type`.
#'   Writes use a lock and atomic replacement of the existing `favorites.rds`.
#' @export
gen_model_favorites <- function(models = NULL, directory = NULL, catalog = NULL, save = TRUE) .gen_management({
  directory <- .gen_management_path(directory, "directory")
  .gen_management_flag(save, "save")
  if (!is.null(models)) .gen_management_assert(is.data.frame(models) &&
    all(c("service", "model") %in% names(models)), "models")
  if (!is.null(models)) {
    .gen_management_assert(all(vapply(models[c("service", "model")], function(x)
      is.character(x) && !anyNA(x) && all(nzchar(trimws(x))), logical(1))), "models")
    if ("type" %in% names(models)) .gen_management_assert(is.character(models$type), "models")
  }
  if (!is.null(catalog)) .gen_management_assert(is.data.frame(catalog) &&
    all(c("service", "model") %in% names(catalog)), "catalog")
  supplied <- !is.null(models)
  models <- models %||% .load_favorites(directory)
  rows <- .normalize_favorites(models, catalog %||% gen_models(directory = directory))
  if (supplied && save) {
    path <- .favorites_path(directory)
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    lock <- .genflow_acquire_file_lock(path)
    on.exit(.genflow_release_file_lock(lock), add = TRUE)
    .save_favorites(rows, directory)
  }
  rows
})

.favorites_path <- function(directory = NULL) {
  dir <- .genflow_resolve_models_dir(directory)
  file.path(dir, "favorites.rds")
}

.empty_favorites <- function() {
  data.frame(
    service = character(),
    model = character(),
    type = character(),
    stringsAsFactors = FALSE
  )
}

.load_favorites <- function(directory = NULL) {
  path <- .favorites_path(directory)
  if (!file.exists(path)) {
    return(.empty_favorites())
  }
  favs <- tryCatch(readRDS(path), error = function(e) NULL)
  if (!is.data.frame(favs) || !all(c("service", "model") %in% names(favs))) {
    return(.empty_favorites())
  }
  if (!"type" %in% names(favs)) {
    favs$type <- NA_character_
  }
  favs$service <- tolower(trimws(as.character(favs$service %||% "")))
  favs$model <- trimws(as.character(favs$model %||% ""))
  favs$type <- as.character(favs$type %||% "")
  favs$type[is.na(favs$type)] <- ""
  valid <- !is.na(favs$service) &
    nzchar(favs$service) &
    !.genflow_is_retired_service(favs$service) &
    !is.na(favs$model) &
    nzchar(favs$model)
  favs <- favs[valid, , drop = FALSE]
  if (!nrow(favs)) {
    return(.empty_favorites())
  }
  unique(favs)
}

.save_favorites <- function(favorites, directory = NULL) {
  path <- .favorites_path(directory)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  .genflow_atomic_save_rds(favorites, path)
  invisible(favorites)
}

.normalize_favorites <- function(favorites, catalog) {
  if (!is.data.frame(favorites) ||
      !all(c("service", "model") %in% names(favorites))) {
    return(.empty_favorites())
  }
  if (!"type" %in% names(favorites)) {
    favorites$type <- ""
  }
  favorites <- favorites[, c("service", "model", "type"), drop = FALSE]
  favorites$service <- tolower(trimws(as.character(favorites$service)))
  favorites$model <- trimws(as.character(favorites$model))
  favorites$type <- as.character(favorites$type)
  favorites$type[is.na(favorites$type)] <- ""
  valid <- !is.na(favorites$service) &
    nzchar(favorites$service) &
    !.genflow_is_retired_service(favorites$service) &
    !is.na(favorites$model) &
    nzchar(favorites$model)
  favorites <- favorites[valid, , drop = FALSE]
  if (!nrow(favorites)) {
    return(.empty_favorites())
  }
  favorites$service <- vapply(favorites$service, .genflow_normalize_service_alias, character(1), USE.NAMES = FALSE)
  if (!is.data.frame(catalog) ||
      !nrow(catalog) ||
      !all(c("service", "model") %in% names(catalog))) {
    return(unique(favorites))
  }

  catalog_service <- tolower(trimws(as.character(catalog$service)))
  catalog_model <- trimws(as.character(catalog$model))
  catalog_valid <- !is.na(catalog_service) &
    nzchar(catalog_service) &
    !is.na(catalog_model) &
    nzchar(catalog_model)
  catalog_service <- catalog_service[catalog_valid]
  catalog_model <- catalog_model[catalog_valid]
  catalog_types <- if ("type" %in% names(catalog)) {
    as.character(catalog$type[catalog_valid])
  } else {
    rep("", length(catalog_service))
  }
  catalog_types[is.na(catalog_types)] <- ""
  catalog_keys <- paste(catalog_service, catalog_model, sep = "||")
  fav_keys <- paste(tolower(favorites$service), favorites$model, sep = "||")
  keep <- fav_keys %in% catalog_keys
  favorites <- favorites[keep, , drop = FALSE]
  if (!nrow(favorites)) {
    return(.empty_favorites())
  }
  matched_idx <- match(paste(tolower(favorites$service), favorites$model, sep = "||"), catalog_keys)
  matched_types <- catalog_types[matched_idx]
  missing_type <- !nzchar(favorites$type)
  favorites$type[missing_type] <- matched_types[missing_type]
  favorites$type[is.na(favorites$type)] <- ""
  unique(favorites)
}

.genflow_model_catalog <- function(directory = NULL) {
  dir <- .genflow_resolve_models_dir(directory)
  if (!dir.exists(dir)) {
    return(data.frame())
  }
  files <- list.files(dir, pattern = "\\.csv$", full.names = TRUE)
  if (!length(files)) {
    return(data.frame())
  }

  catalogs <- lapply(files, function(path) {
    fallback_service <- tolower(tools::file_path_sans_ext(basename(path)))
    if (.genflow_is_retired_service(fallback_service)) return(NULL)
    df <- tryCatch(
      read.csv(path, stringsAsFactors = FALSE, check.names = FALSE),
      error = function(e) NULL
    )
    if (is.null(df) || nrow(df) == 0) {
      return(NULL)
    }
    names(df) <- tolower(names(df))
    if (!"service" %in% names(df)) {
      df$service <- if ("provider" %in% names(df)) df$provider else fallback_service
    }
    df$service <- tolower(trimws(as.character(df$service)))
    missing_service <- is.na(df$service) | !nzchar(df$service)
    df$service[missing_service] <- fallback_service
    df <- df[!.genflow_is_retired_service(df$service), , drop = FALSE]
    if (!nrow(df)) {
      return(NULL)
    }
    df$service <- vapply(
      df$service,
      .genflow_normalize_service_alias,
      character(1),
      USE.NAMES = FALSE
    )
    # The Python/Transformers bridge was retired. Ignore old user catalogs
    # without deleting user-owned data.
    df <- df[!.genflow_is_retired_service(df$service), , drop = FALSE]
    if (!nrow(df)) {
      return(NULL)
    }

    if (!"model" %in% names(df)) {
      if ("id" %in% names(df)) {
        df$model <- df$id
      } else {
        df$model <- ""
      }
    }
    df$model <- trimws(as.character(df$model))
    valid_model <- !is.na(df$model) & nzchar(df$model)
    df <- df[valid_model, , drop = FALSE]
    if (!nrow(df)) {
      return(NULL)
    }

    if (!"type" %in% names(df)) {
      df$type <- NA_character_
    }
    df$type <- as.character(df$type)

    if (!"pricing" %in% names(df)) {
      df$pricing <- NA_character_
    } else {
      df$pricing <- as.character(df$pricing)
    }

    if (!"description" %in% names(df)) {
      df$description <- NA_character_
    } else {
      df$description <- as.character(df$description)
    }

    df$source_file <- basename(path)
    df[, c(
      "service",
      "model",
      "type",
      "pricing",
      "description",
      "source_file"
    ), drop = FALSE]
  })

  catalogs <- Filter(Negate(is.null), catalogs)
  if (!length(catalogs)) {
    return(data.frame())
  }

  out <- do.call(rbind, catalogs)
  rownames(out) <- NULL
  out
}
