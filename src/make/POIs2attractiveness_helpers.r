`%||%` <- function(x, y) if (is.null(x)) y else x

print_progress <- function(timing_start, current_step, total_steps, name = NA, unit = "secs", digits = 0, show_timestamp = T, show_timelapse = T, show_percent = TRUE, every_n = 1, n_parts = NA, criterion = TRUE, method = "message") {
  timing_now <- Sys.time()
  elapsed_sec <- as.numeric(timing_now - timing_start, units = "secs")
  avg_per_step_sec <- elapsed_sec / current_step
  remaining_sec <- (total_steps - current_step) * avg_per_step_sec
  
  valid_units <- c("secs", "mins")
  if ( !(unit %in% valid_units) ) {
    warning(paste0("Invalid unit '", unit, "' specified; defaulting to 'secs'."))
    unit <- "secs"
  }
  unit_divisor <- if (unit == "secs") 1 else 60
  unit_string <- (if (unit == "secs") "[seconds]" else "[minutes]")
  
  elapsed <- round(elapsed_sec/unit_divisor, digits)
  avg_per_step <- round(avg_per_step_sec/unit_divisor, digits)
  remaining <- round(remaining_sec/unit_divisor, digits)
  
  prefix_name <- if ( !is.na(name) ) paste0(name, ": ") else ""
  
  if (!is.na(n_parts)) {
    if (every_n != 1) warning("Both every_n and n_parts specified; using n_parts.")
    if (!is.numeric(n_parts) ||n_parts <= 0) {
      warning("n_parts must be numeric and > 0; ignoring n_parts.")
    } else {
      every_n <- ceiling(total_steps / n_parts)
    }
  }
  
  if (show_percent) {
    percent_complete <- round((current_step / total_steps) * 100, 1)
    percent_complete_formatted <- format(percent_complete, nsmall = 1, digits = 1)
    
    indicator_percent <- paste0(" [", percent_complete_formatted, "%]")
  } else {
    indicator_percent <- ""
  }
  
  if (criterion && ((current_step %% every_n) == 0 || current_step == total_steps || current_step == 1 )) {
    
    method(paste0(
      prefix_name,
      "Step ", current_step, " of ", total_steps, " finished", indicator_percent, ".",
      " ",
      (if(show_timelapse) paste0(
        "Elapsed:", elapsed, 
        " - per step:", avg_per_step, 
        " - estimated remaining:", remaining, 
        unit_string, ".",
        " "
      ) else ""),
      (if(show_timestamp) paste("Estimated completion:", format(Sys.time() + remaining_sec, "%Y-%m-%d %H:%M:%S")) else "")
    ))
    
  }
  
}



msg <- function(...) {
  message(sprintf("[%s] %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), paste0(..., collapse = "")))
}

assert_true <- function(cond, txt) {
  if (!isTRUE(cond)) {
    stop(txt, call. = FALSE)
  }
}

as_char_vec <- function(x) {
  if (is.null(x)) return(character())
  as.character(unlist(x, use.names = FALSE))
}




find_poi_file <- function(poi_dir, category, file_prefix = "", file_suffix = ".geojson") {
  filename <- paste0(file_prefix, category, file_suffix)
  full_path <- file.path(poi_dir, filename)
  if (!file.exists(full_path)) {
    stop(
      sprintf(
        "No POI file found for category '%s'. Expected filename: '%s' in folder '%s'.",
        category, filename, poi_dir
      ),
      call. = FALSE
    )
  }
  full_path
}



write_gpkg <- function(sf_obj, path_gpkg, layer_name) {
  assert_true(inherits(sf_obj, "sf") || is.data.frame(sf_obj), "Object to write must be an sf object or a data.frame.")
  dir.create(dirname(path_gpkg), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(path_gpkg)) file.remove(path_gpkg)
  sf::st_write(sf_obj, dsn = path_gpkg, layer = layer_name, driver = "GPKG", append = FALSE, quiet = TRUE)
  msg("GeoPackage written: ", path_gpkg, " (layer: ", layer_name, ")")
}



extract_spec <- function(spec) {
  if (is.list(spec) && !is.null(names(spec)) && all(c("metric", "coefficient") %in% names(spec))) {
    return(list(
      metric = as.character(spec$metric),
      coefficient = as.numeric(spec$coefficient),
      area_field = as.character(spec$area_field %||% NA_character_),
      levels_field = as.character(spec$levels_field %||% NA_character_),
      floor_field = as.character(spec$floor_field %||% NA_character_),
      max_size = as.numeric(spec$max_size %||% NA_real_),
      adjuster = spec$adjuster %||% NULL
    ))
  }
  list(
    metric = as.character(spec[[1]]),
    coefficient = as.numeric(spec[[2]]),
    area_field = NA_character_,
    levels_field = NA_character_,
    floor_field = NA_character_,
    max_size = if (length(spec) >= 3L) as.numeric(spec[[3]]) else NA_real_,
    adjuster = NULL
  )
}



pick_first_existing_field <- function(field_names, candidates) {
  hit <- candidates[candidates %chin% field_names]
  if (length(hit) == 0L) return(NA_character_)
  hit[1L]
}




validate_categories_map <- function(pois) {
  assert_true(is.list(pois) && length(pois) > 0L, "'pois' must be a non-empty list.")
  assert_true(!is.null(names(pois)) && all(nzchar(names(pois))), "All POI categories in 'pois' must be named.")
  reserved_keys <- c("filter", "percentile_imputation")
  
  for (cat_name in names(pois)) {
    cat_spec <- pois[[cat_name]]
    assert_true(is.list(cat_spec) && length(cat_spec) > 0L, sprintf("Category '%s' contains no trip-purpose specifications.", cat_name))
    assert_true(!is.null(names(cat_spec)) && all(nzchar(names(cat_spec))), sprintf("Category '%s' contains unnamed trip-purpose entries.", cat_name))

    if ("percentile_imputation" %in% names(cat_spec)) {
      p <- as.numeric(cat_spec$percentile_imputation)
      assert_true(length(p) == 1L && is.finite(p) && p > 0 && p < 1,
                  sprintf("Category '%s': percentile_imputation must be a finite value in (0, 1).", cat_name))
    }

    if ("filter" %in% names(cat_spec)) {
      fil <- cat_spec$filter
      assert_true(is.list(fil), sprintf("Category '%s': filter must be an object.", cat_name))
      if (!is.null(fil$min_area)) {
        min_area <- as.numeric(fil$min_area)
        assert_true(length(min_area) == 1L && is.finite(min_area) && min_area >= 0,
                    sprintf("Category '%s': filter.min_area must be a finite numeric value >= 0.", cat_name))
      }
      if (!is.null(fil$area_field)) {
        area_field <- as.character(fil$area_field)
        assert_true(length(area_field) == 1L && nzchar(area_field),
                    sprintf("Category '%s': filter.area_field must be a non-empty string.", cat_name))
      }
    }

    purpose_names <- setdiff(names(cat_spec), reserved_keys)
    assert_true(length(purpose_names) > 0L,
                sprintf("Category '%s' contains no trip-purpose specifications after removing reserved fields.", cat_name))
    
    for (purpose in purpose_names) {
      spec <- cat_spec[[purpose]]
      if (is.list(spec) && !is.null(names(spec)) && all(c("metric", "coefficient") %in% names(spec))) {
        metric <- as.character(spec$metric)
        coefficient <- as.numeric(spec$coefficient)
        max_size <- as.numeric(spec$max_size %||% NA_real_)
      } else {
        assert_true(is.list(spec) && length(spec) >= 2L, sprintf("Invalid definition for category '%s', purpose '%s'.", cat_name, purpose))
        metric <- as.character(spec[[1]])
        coefficient <- as.numeric(spec[[2]])
        max_size <- if (length(spec) >= 3L) as.numeric(spec[[3]]) else NA_real_
      }
      
      assert_true(metric %in% c("Count", "Area", "FloorArea"), sprintf("Unknown metric type '%s' in category '%s', purpose '%s'.", metric, cat_name, purpose))
      assert_true(length(coefficient) == 1L && !is.na(coefficient), sprintf("Invalid coefficient in category '%s', purpose '%s'.", cat_name, purpose))
      if (!is.na(max_size)) {
        assert_true(length(max_size) == 1L && is.finite(max_size) && max_size > 0, sprintf("Invalid max_size in category '%s', purpose '%s'.", cat_name, purpose))
      }
    }
  }
}



# JSON config loading
# Translation table: JSON metric type names -> internal R metric type names
.json_metric_map <- c(
  Area      = "Area",
  FloorArea = "FloorArea",
  Count    = "Count"
)

#' Load a \code{pois} categories list with attractiveness factors from a JSON configuration file.
#'
#' The JSON must be a top-level object whose keys are category names. Each
#' category value is itself an object whose keys are trip-purpose names.
#' A purpose value may be either:
#'   \itemize{
#'     \item An array \code{[metric, coefficient]} (or \code{[metric, coefficient, max_size]}).
#'     \item An object with fields \code{metric}, \code{coefficient}, and
#'           optionally \code{max_size} and/or \code{adjuster} (a string
#'           expression, currently supporting
#'           \code{linear_size_multiplier(size_min, size_max, mult_at_min, mult_at_max)}).
#'   }
#' Metric names in the JSON use English identifiers (\code{"Area"},
#' \code{"FloorArea"}, \code{"Count"}); these are translated to the internal
#' German names expected by \code{calc_metric_vector}.
#'
#' @param json_path Path to the JSON file.
#' @return A named list suitable for passing as the \code{pois} argument of
#'   \code{calculate_attractiveness}.
load_attractiveness_factors_from_json <- function(json_path) {
  assert_true(file.exists(json_path), paste("JSON configuration file not found:", json_path))
  raw <- jsonlite::fromJSON(json_path, simplifyVector = FALSE)
  assert_true(is.list(raw) && length(raw) > 0L, "JSON configuration file is empty or not an object.")
  reserved_keys <- c("filter", "percentile_imputation")
  
  translate_metric <- function(metric_raw, category, purpose) {
    metric <- .json_metric_map[metric_raw]
    assert_true(!is.na(metric), sprintf(
      "Category '%s', purpose '%s': unknown JSON metric type '%s'. Allowed values: %s.",
      category, purpose, metric_raw, paste(names(.json_metric_map), collapse = ", ")
    ))
    unname(metric)
  }
  
  resolve_adjuster <- function(adj_expr, category, purpose) {
    build_adjuster_from_string(adj_expr, category, purpose)
  }
  
  parse_purpose_spec <- function(purpose_spec, category, purpose) {
    if (is.list(purpose_spec) && length(names(purpose_spec)) > 0L) {
      # Object form: {metric, coefficient, max_size?, adjuster?}
      metric <- translate_metric(as.character(purpose_spec$metric), category, purpose)
      result <- list(
        metric      = metric,
        coefficient = as.numeric(purpose_spec$coefficient)
      )
      if (!is.null(purpose_spec$max_size)) {
        result$max_size <- as.numeric(purpose_spec$max_size)
      }
      if (!is.null(purpose_spec$adjuster)) {
        result$adjuster <- resolve_adjuster(as.character(purpose_spec$adjuster), category, purpose)
      }
      result
    } else {
      # Array form: [metric, coefficient] or [metric, coefficient, max_size]
      assert_true(length(purpose_spec) >= 2L, sprintf(
        "Category '%s', purpose '%s': array form requires at least [metric, coefficient].", category, purpose
      ))
      metric <- translate_metric(as.character(purpose_spec[[1L]]), category, purpose)
      result <- list(metric, as.numeric(purpose_spec[[2L]]))
      if (length(purpose_spec) >= 3L) {
        result[[3L]] <- as.numeric(purpose_spec[[3L]])
      }
      result
    }
  }
  
  pois <- vector("list", length(raw))
  names(pois) <- names(raw)
  for (cat_name in names(raw)) {
    cat_spec <- raw[[cat_name]]
    assert_true(is.list(cat_spec) && length(cat_spec) > 0L,
                sprintf("Category '%s': contains no trip-purpose specifications.", cat_name))
    purpose_names <- setdiff(names(cat_spec), reserved_keys)
    assert_true(length(purpose_names) > 0L,
                sprintf("Category '%s': contains no trip-purpose specifications after removing reserved fields.", cat_name))

    purposes <- vector("list", length(purpose_names))
    names(purposes) <- purpose_names
    for (purpose in purpose_names) {
      purposes[[purpose]] <- parse_purpose_spec(cat_spec[[purpose]], cat_name, purpose)
    }

    if ("percentile_imputation" %in% names(cat_spec)) {
      purposes$percentile_imputation <- as.numeric(cat_spec$percentile_imputation)
    }

    if ("filter" %in% names(cat_spec)) {
      raw_filter <- cat_spec$filter
      filter_cfg <- list()
      if (!is.null(raw_filter$min_area)) {
        filter_cfg$min_area <- as.numeric(raw_filter$min_area)
      }
      if (!is.null(raw_filter$area_field)) {
        filter_cfg$area_field <- as.character(raw_filter$area_field)
      }
      purposes$filter <- filter_cfg
    }

    pois[[cat_name]] <- purposes
  }
  
  msg("load_pois_from_json: loaded ", length(pois), " categories from '", basename(json_path), "'.")
  pois
}


report_poi_coverage <- function(poi_dir, pois, file_prefix = "", file_suffix = ".geojson") {
  all_geojson <- list.files(poi_dir, pattern = "\\.geojson$", full.names = FALSE, ignore.case = TRUE)
  expected_files <- paste0(file_prefix, names(pois), file_suffix)
  
  all_geojson_l <- tolower(all_geojson)
  expected_files_l <- tolower(expected_files)
  
  extra_in_folder <- all_geojson[!(all_geojson_l %chin% expected_files_l)]
  missing_in_folder <- expected_files[!(expected_files_l %chin% all_geojson_l)]
  
  if (length(extra_in_folder) > 0L) {
    msg(
      "POI-Coverage: ", length(extra_in_folder),
      " files in the folder are NOT covered by 'prefix + category + suffix' from 'pois': ",
      paste(sort(extra_in_folder), collapse = ", ")
    )
  } else {
    msg("POI-Coverage: All GeoJSON files in the folder are covered by the configured naming rule.")
  }
  
  if (length(missing_in_folder) > 0L) {
    msg(
      "POI-Coverage: ", length(missing_in_folder),
      " expected files (from 'prefix + category + suffix') are missing in the folder: ",
      paste(sort(missing_in_folder), collapse = ", ")
    )
  } else {
    msg("POI-Coverage: The expected file was found for all configured categories.")
  }
  
  invisible(list(extra_in_folder = extra_in_folder, missing_in_folder = missing_in_folder))
}



hash_category_prefix <- function(category_name) {
  
  library(digest)
  hash <- digest(object = category_name, algo = "crc32", serialize = FALSE)
  
  substr(hash, 1, 4)
  
}

