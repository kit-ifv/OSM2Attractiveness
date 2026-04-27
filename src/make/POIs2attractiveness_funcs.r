library(data.table)
library(sf)

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

linear_size_multiplier <- function(size_values, size_min, size_max, mult_at_min, mult_at_max) {
  sz <- as.numeric(size_values)
  assert_true(is.finite(size_min) && is.finite(size_max) && size_max > size_min, "Invalid bounds for linear interpolation.")
  assert_true(is.finite(mult_at_min) && is.finite(mult_at_max), "Invalid multiplier values for linear interpolation.")
  
  result <- rep(NA_real_, length(sz))
  valid <- is.finite(sz)
  if (!any(valid)) return(rep(1, length(sz)))
  
  sz_v <- sz[valid]
  t <- (sz_v - size_min) / (size_max - size_min)
  t <- pmax(0, pmin(1, t))
  result[valid] <- mult_at_min + t * (mult_at_max - mult_at_min)
  
  # treat missing/invalid size values neutrally
  result[!valid] <- 1
  result
}

build_adjuster_from_string <- function(adj_string, category, purpose) {
  assert_true(is.character(adj_string) && length(adj_string) == 1L && nzchar(adj_string), sprintf(
    "Category '%s', purpose '%s': 'adjuster' must be a non-empty string.", category, purpose
  ))

  expr <- tryCatch(parse(text = adj_string)[[1L]], error = function(e) NULL)
  assert_true(!is.null(expr) && is.call(expr), sprintf(
    "Category '%s', purpose '%s': could not parse adjuster '%s'.",
    category, purpose, adj_string
  ))

  fn_name <- as.character(expr[[1L]])
  assert_true(identical(fn_name, "linear_size_multiplier"), sprintf(
    "Category '%s', purpose '%s': unsupported adjuster function '%s'. Allowed: linear_size_multiplier.",
    category, purpose, fn_name
  ))

  args <- as.list(expr)[-1L]
  arg_names <- names(args)
  named <- !is.null(arg_names) && any(nzchar(arg_names))
  required_names <- c("size_min", "size_max", "mult_at_min", "mult_at_max")

  eval_num <- function(a_expr, a_label) {
    v <- suppressWarnings(as.numeric(tryCatch(eval(a_expr, envir = baseenv()), error = function(e) NA_real_)))
    assert_true(length(v) == 1L && is.finite(v), sprintf(
      "Category '%s', purpose '%s': adjuster argument '%s' must evaluate to one finite numeric value.",
      category, purpose, a_label
    ))
    as.numeric(v)
  }

  if (named) {
    assert_true(all(nzchar(arg_names)), sprintf(
      "Category '%s', purpose '%s': if one adjuster argument is named, all must be named.",
      category, purpose
    ))
    assert_true(setequal(arg_names, required_names), sprintf(
      "Category '%s', purpose '%s': named adjuster arguments must be exactly: %s.",
      category, purpose, paste(required_names, collapse = ", ")
    ))

    size_min <- eval_num(args[["size_min"]], "size_min")
    size_max <- eval_num(args[["size_max"]], "size_max")
    mult_at_min <- eval_num(args[["mult_at_min"]], "mult_at_min")
    mult_at_max <- eval_num(args[["mult_at_max"]], "mult_at_max")
  } else {
    assert_true(length(args) == 4L, sprintf(
      "Category '%s', purpose '%s': positional adjuster call must have 4 numeric arguments.",
      category, purpose
    ))
    size_min <- eval_num(args[[1L]], "size_min")
    size_max <- eval_num(args[[2L]], "size_max")
    mult_at_min <- eval_num(args[[3L]], "mult_at_min")
    mult_at_max <- eval_num(args[[4L]], "mult_at_max")
  }

  assert_true(size_max > size_min, sprintf(
    "Category '%s', purpose '%s': adjuster requires size_max > size_min.", category, purpose
  ))

  function(dt, metric_values) {
    linear_size_multiplier(
      size_values = metric_values,
      size_min = size_min,
      size_max = size_max,
      mult_at_min = mult_at_min,
      mult_at_max = mult_at_max
    )
  }
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

validate_pois_map <- function(pois) {
  assert_true(is.list(pois) && length(pois) > 0L, "'pois' must be a non-empty list.")
  assert_true(!is.null(names(pois)) && all(nzchar(names(pois))), "All POI categories in 'pois' must be named.")
  
  for (cat_name in names(pois)) {
    cat_spec <- pois[[cat_name]]
    assert_true(is.list(cat_spec) && length(cat_spec) > 0L, sprintf("Category '%s' contains no trip-purpose specifications.", cat_name))
    assert_true(!is.null(names(cat_spec)) && all(nzchar(names(cat_spec))), sprintf("Category '%s' contains unnamed trip-purpose entries.", cat_name))
    
    for (purpose in names(cat_spec)) {
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

cap_metric_values <- function(values, max_size, category, purpose, metric) {
  if (is.na(max_size)) return(values)
  assert_true(length(max_size) == 1L && is.finite(max_size) && max_size > 0, sprintf("Category '%s', purpose '%s': 'max_size' must be > 0.", category, purpose))
  
  v <- as.numeric(values)
  idx_cap <- which(is.finite(v) & v > max_size)
  if (length(idx_cap) > 0L) {
    msg("Category '", category, "', purpose '", purpose, "': applied cap for ", metric, " at max_size=", format(round(max_size, 6), nsmall = 6), " to ", length(idx_cap), " features.")
    v[idx_cap] <- max_size
  } else {
    msg("Category '", category, "', purpose '", purpose, "': applied cap for ", metric, " at max_size=", format(round(max_size, 6), nsmall = 6), " to 0 features.")
  }
  v
}

pick_first_existing_field <- function(field_names, candidates) {
  hit <- candidates[candidates %chin% field_names]
  if (length(hit) == 0L) return(NA_character_)
  hit[1L]
}

build_poi_export_base <- function(dt, poi_sf, category, defaults) {
  field_names <- names(dt)
  id_field <- pick_first_existing_field(field_names, c("osm_id", "osmId", "osmid", "@id", "id"))
  name_field <- pick_first_existing_field(field_names, c("name", "Name", "NAME"))
  area_field <- if (!is.null(defaults$area_field) && defaults$area_field %chin% field_names) defaults$area_field else NA_character_
  floor_field <- if (!is.null(defaults$floor_field) && defaults$floor_field %chin% field_names) defaults$floor_field else NA_character_
  
  osm_id_vec <- if (!is.na(id_field)) as.character(dt[[id_field]]) else paste0(category, "_", seq_len(nrow(dt)))
  if (is.na(id_field)) {
    msg("Category '", category, "': no OSM ID field found (expected e.g. osm_id/id). Synthetic IDs will be generated.")
  }
  
  name_vec <- if (!is.na(name_field)) as.character(dt[[name_field]]) else rep(NA_character_, nrow(dt))
  area_vec <- if (!is.na(area_field)) suppressWarnings(as.numeric(dt[[area_field]])) else rep(NA_real_, nrow(dt))
  floor_vec <- if (!is.na(floor_field)) suppressWarnings(as.numeric(dt[[floor_field]])) else rep(NA_real_, nrow(dt))
  
  meta <- data.table(
    osm_id = osm_id_vec,
    name = name_vec,
    category = category,
    area = area_vec,
    floorarea = floor_vec
  )
  
  sf::st_as_sf(cbind(as.data.frame(meta), geometry = sf::st_geometry(poi_sf)), crs = sf::st_crs(poi_sf))
}

write_gpkg <- function(sf_obj, path_gpkg, layer_name) {
  assert_true(inherits(sf_obj, "sf"), "Object to write is not an sf object.")
  dir.create(dirname(path_gpkg), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(path_gpkg)) file.remove(path_gpkg)
  sf::st_write(sf_obj, dsn = path_gpkg, layer = layer_name, driver = "GPKG", append = FALSE, quiet = TRUE)
  msg("GeoPackage written: ", path_gpkg, " (layer: ", layer_name, ")")
}

apply_cleaning_rules <- function(dt, category, cleaning_rules = list()) {
  rules <- cleaning_rules[[category]]
  if (is.null(rules)) return(dt)
  
  n_before <- nrow(dt)
  keep <- rep(TRUE, n_before)
  
  require_any <- rules$require_any %||% list()
  if (length(require_any) > 0L) {
    any_keep <- rep(FALSE, n_before)
    for (field in names(require_any)) {
      assert_true(field %in% names(dt), sprintf("Category '%s': field '%s' from require_any is missing.", category, field))
      allowed <- tolower(as_char_vec(require_any[[field]]))
      vals <- tolower(trimws(as.character(dt[[field]])))
      any_keep <- any_keep | (!is.na(vals) & vals %chin% allowed)
    }
    keep <- keep & any_keep
  }
  
  require_all <- rules$require_all %||% list()
  if (length(require_all) > 0L) {
    all_keep <- rep(TRUE, n_before)
    for (field in names(require_all)) {
      assert_true(field %in% names(dt), sprintf("Category '%s': field '%s' from require_all is missing.", category, field))
      allowed <- tolower(as_char_vec(require_all[[field]]))
      vals <- tolower(trimws(as.character(dt[[field]])))
      all_keep <- all_keep & (!is.na(vals) & vals %chin% allowed)
    }
    keep <- keep & all_keep
  }
  
  exclude_any <- rules$exclude_any %||% list()
  if (length(exclude_any) > 0L) {
    ex <- rep(FALSE, n_before)
    for (field in names(exclude_any)) {
      assert_true(field %in% names(dt), sprintf("Category '%s': field '%s' from exclude_any is missing.", category, field))
      blocked <- tolower(as_char_vec(exclude_any[[field]]))
      vals <- tolower(trimws(as.character(dt[[field]])))
      ex <- ex | (!is.na(vals) & vals %chin% blocked)
    }
    keep <- keep & !ex
  }
  
  out <- dt[keep]
  msg("Category '", category, "': cleaning reduced records from ", n_before, " to ", nrow(out), ".")
  out
}

impute_with_p15 <- function(x, label) {
  x <- as.numeric(x)
  idx_missing <- which(is.na(x))
  if (!length(idx_missing)) return(list(values = x, imputed = 0L, p15 = NA_real_))
  
  valid <- x[!is.na(x) & is.finite(x) & x > 0]
  assert_true(length(valid) > 0L, sprintf("No valid values available to impute '%s' via the 15th percentile.", label))
  
  p15 <- as.numeric(stats::quantile(valid, probs = 0.15, na.rm = TRUE, names = FALSE, type = 7))
  assert_true(is.finite(p15) && p15 > 0, sprintf("Invalid 15th percentile for '%s'.", label))
  
  x[idx_missing] <- p15
  list(values = x, imputed = length(idx_missing), p15 = p15)
}

ensure_numeric_field <- function(dt, field_name, category, purpose) {
  assert_true(field_name %in% names(dt), sprintf("Category '%s', purpose '%s': required field '%s' is missing.", category, purpose, field_name))
  suppressWarnings(v <- as.numeric(dt[[field_name]]))
  assert_true(any(!is.na(v)), sprintf("Category '%s', purpose '%s': field '%s' contains no numerically usable values.", category, purpose, field_name))
  v
}

calc_metric_vector <- function(dt, category, purpose, spec, defaults) {
  metric <- spec$metric
  area_field <- if (!is.na(spec$area_field)) spec$area_field else defaults$area_field
  levels_field <- if (!is.na(spec$levels_field)) spec$levels_field else defaults$levels_field
  floor_field <- if (!is.na(spec$floor_field)) spec$floor_field else defaults$floor_field
  
  if (metric == "Count") {
    return(rep(1, nrow(dt)))
  }
  
  if (metric == "Area") {
    area <- ensure_numeric_field(dt, area_field, category, purpose)
    imp <- impute_with_p15(area, sprintf("%s/%s/%s", category, purpose, area_field))
    if (imp$imputed > 0L) {
      msg("Category '", category, "', purpose '", purpose, "': area imputation: ", imp$imputed, " features imputed; value used (P15)=", format(round(imp$p15, 6), nsmall = 6), ".")
    } else {
      msg("Category '", category, "', purpose '", purpose, "': area imputation: 0 features imputed.")
    }
    return(cap_metric_values(imp$values, spec$max_size, category, purpose, metric))
  }
  
  if (metric == "FloorArea") {
    floor_available <- floor_field %in% names(dt)
    if (floor_available) {
      suppressWarnings(floor_vals <- as.numeric(dt[[floor_field]]))
    } else {
      floor_vals <- rep(NA_real_, nrow(dt))
    }
    
    area <- ensure_numeric_field(dt, area_field, category, purpose)
    area_imp <- impute_with_p15(area, sprintf("%s/%s/%s", category, purpose, area_field))
    area2 <- area_imp$values
    if (area_imp$imputed > 0L) {
      msg("Category '", category, "', purpose '", purpose, "': area imputation (for floor area): ", area_imp$imputed, " features imputed; value used (P15)=", format(round(area_imp$p15, 6), nsmall = 6), ".")
    } else {
      msg("Category '", category, "', purpose '", purpose, "': area imputation (for floor area): 0 features imputed.")
    }
    
    levels <- if (levels_field %in% names(dt)) suppressWarnings(as.numeric(dt[[levels_field]])) else rep(NA_real_, nrow(dt))
    levels[!is.finite(levels) | is.na(levels) | levels <= 0] <- defaults$level_default
    
    computed_floor <- area2 * levels
    use_floor <- ifelse(is.finite(floor_vals) & !is.na(floor_vals) & floor_vals > 0, floor_vals, computed_floor)
    return(cap_metric_values(use_floor, spec$max_size, category, purpose, metric))
  }
  
  stop(sprintf("Metric '%s' is not implemented.", metric), call. = FALSE)
}

apply_adjuster <- function(metric_values, dt, spec, category, purpose) {
  if (is.null(spec$adjuster)) return(metric_values)
  assert_true(is.function(spec$adjuster), sprintf("Category '%s', purpose '%s': 'adjuster' must be a function.", category, purpose))
  adj_fun_nargs <- length(formals(spec$adjuster))
  adj <- if (adj_fun_nargs >= 2L) spec$adjuster(dt, metric_values) else spec$adjuster(dt)
  adj <- as.numeric(adj)
  assert_true(length(adj) == nrow(dt), sprintf("Category '%s', purpose '%s': 'adjuster' must return a vector of length nrow(dt).", category, purpose))
  assert_true(all(is.finite(adj) | is.na(adj)), sprintf("Category '%s', purpose '%s': 'adjuster' contains invalid values.", category, purpose))
  metric_values * fifelse(is.na(adj), 1, adj)
}

assign_zones <- function(pois_sf, zones_sf, zone_id_field, category) {
  rel <- sf::st_within(pois_sf, zones_sf)
  zone_idx <- vapply(rel, function(x) if (length(x) == 0L) NA_integer_ else x[1L], integer(1))
  multiple <- sum(lengths(rel) > 1L)
  outside <- sum(is.na(zone_idx))
  
  if (multiple > 0L) {
    msg("Category '", category, "': ", multiple, " POIs are in multiple zones; the first assignment is used.")
  }
  if (outside > 0L) {
    msg("Category '", category, "': ", outside, " POIs could not be assigned to any zone and will be excluded.")
  }
  
  zone_ids <- rep(NA_character_, length(zone_idx))
  valid <- !is.na(zone_idx)
  zone_ids[valid] <- as.character(zones_sf[[zone_id_field]][zone_idx[valid]])
  zone_ids
}

calculate_attractiveness <- function(
    poi_dir,
    zones_file,
    pois,
    poi_file_prefix = "",
    poi_file_suffix = ".geojson",
    zone_id_field = "zoneId",
    cleaning_rules = list(),
    defaults = list(area_field = "Area", levels_field = "Level_number", floor_field = "FloorArea", level_default = 1),
    output_csv = NULL,
    output_gpkg_all_pois = NULL,
    output_gpkg_all_poi_attractiveness = NULL
) {
  msg("Starting attractiveness calculation.")
  validate_pois_map(pois)
  
  assert_true(dir.exists(poi_dir), sprintf("POI folder does not exist: %s", poi_dir))
  assert_true(file.exists(zones_file), sprintf("Zone file does not exist: %s", zones_file))
  report_poi_coverage(poi_dir, pois, file_prefix = poi_file_prefix, file_suffix = poi_file_suffix)
  
  zones_sf <- sf::st_read(zones_file, quiet = TRUE)
  assert_true(nrow(zones_sf) > 0L, "Zone file contains no features.")
  assert_true(zone_id_field %in% names(zones_sf), sprintf("Zone field '%s' is missing in the zone file.", zone_id_field))
  zones_sf[[zone_id_field]] <- as.character(zones_sf[[zone_id_field]])
  
  zone_ids_all <- unique(as.character(zones_sf[[zone_id_field]]))
  agg_list <- list()
  detail_list <- list()
  poi_export_list_all_pois <- list()
  poi_export_list_all_poi_attractiveness <- list()
  
  # get file size of all POI files for reporting
  poi_filenames <- paste0(poi_dir, "/", poi_file_prefix, names(pois), poi_file_suffix)
  poi_sizes <- sapply(poi_filenames, function(f) file.size(f))
  total_size_mb <- sum(poi_sizes) / (1024 * 1024)
  size_done <- 0
  n_files <- length(pois)
  msg("Processing ", n_files, " POI files with a total size of ", format(round(total_size_mb, 2), nsmall = 2), " MB.")
  start <- Sys.time()
  msg("Processing start: ", format(start, "%Y-%m-%d %H:%M:%S"))
  
  for (i in seq_along(names(pois))) {
    category <- names(pois)[i]
    msg("Processing category: ", category)
    print_progress(start, i, n_files, name = "POI files", unit = "mins", show_timelapse = FALSE, show_timestamp = FALSE, method = msg)
    size_done <- size_done + (poi_sizes[i] / (1024 * 1024))
    msg("File-size progress: ", format(round(size_done, 2), nsmall = 2), " MB of ", format(round(total_size_mb, 2), nsmall = 2), " MB (", format(round((size_done / total_size_mb) * 100, 1), nsmall = 1), "%)")
    
    poi_file <- find_poi_file(poi_dir, category, file_prefix = poi_file_prefix, file_suffix = poi_file_suffix)
    msg("Reading file: ", basename(poi_file))
    
    poi_sf <- sf::st_read(poi_file, quiet = TRUE)
    if (nrow(poi_sf) == 0L) {
      msg("Category '", category, "': file is empty. Skipping category.")
      next
    }
    
    if (sf::st_crs(poi_sf) != sf::st_crs(zones_sf)) {
      poi_sf <- sf::st_transform(poi_sf, sf::st_crs(zones_sf))
    }
    
    dt <- as.data.table(sf::st_drop_geometry(poi_sf))
    set(dt, j = "source_row__", value = seq_len(nrow(dt)))
    n_rows_before_cleaning <- nrow(dt)
    dt <- apply_cleaning_rules(dt, category, cleaning_rules)
    assert_true(nrow(dt) > 0L, sprintf("Category '%s': no records remain after cleaning.", category))
    
    poi_sf <- poi_sf[dt$source_row__, ]
    set(dt, j = "source_row__", value = NULL)
    
    zone_ids <- assign_zones(poi_sf, zones_sf, zone_id_field, category)
    set(dt, j = "zone_id", value = zone_ids)
    dt <- dt[!is.na(dt[["zone_id"]])]
    assert_true(nrow(dt) > 0L, sprintf("Category '%s': no POIs within zones.", category))
    
    poi_export_base <- build_poi_export_base(dt, poi_sf[!is.na(zone_ids), ], category, defaults)
    poi_export_list_all_pois[[length(poi_export_list_all_pois) + 1L]] <- poi_export_base[, c("osm_id", "name", "category", "area", "floorarea", attr(poi_export_base, "sf_column"))]
    
    cat_specs <- pois[[category]]
    for (purpose in names(cat_specs)) {
      spec <- extract_spec(cat_specs[[purpose]])
      
      metric_vals <- calc_metric_vector(dt, category, purpose, spec, defaults)
      metric_vals <- apply_adjuster(metric_vals, dt, spec, category, purpose)
      attractiveness_vals <- metric_vals * spec$coefficient
      
      tmp <- data.table(
        zoneId = dt$zone_id,
        category = category,
        purpose = purpose,
        attractiveness = attractiveness_vals
      )
      
      tmp_agg <- tmp[, list(attractiveness = sum(.SD[[1L]], na.rm = TRUE)), by = c("zoneId", "purpose"), .SDcols = "attractiveness"]
      agg_list[[length(agg_list) + 1L]] <- tmp_agg
      
      detail_list[[length(detail_list) + 1L]] <- tmp[, .SD, .SDcols = c("zoneId", "category", "purpose", "attractiveness")]
      
      poi_export_b <- poi_export_base[, c("osm_id", "name", "category", attr(poi_export_base, "sf_column"))]
      poi_export_b$purpose <- purpose
      poi_export_b$attractiveness <- attractiveness_vals
      poi_export_list_all_poi_attractiveness[[length(poi_export_list_all_poi_attractiveness) + 1L]] <- poi_export_b[, c("osm_id", "name", "category", "purpose", "attractiveness", attr(poi_export_b, "sf_column"))]
      
      msg("Category '", category, "', purpose '", purpose, "': ", nrow(tmp), " POIs processed.")
    }
  }
  
  assert_true(length(agg_list) > 0L, "No aggregation results were generated.")
  all_agg <- rbindlist(agg_list, use.names = TRUE)
  final_long <- all_agg[, list(attractiveness = sum(.SD[[1L]], na.rm = TRUE)), by = c("zoneId", "purpose"), .SDcols = "attractiveness"]
  
  final_wide <- dcast(final_long, zoneId ~ purpose, value.var = "attractiveness", fill = 0)
  all_zones_dt <- data.table(zoneId = zone_ids_all)
  setkeyv(all_zones_dt, "zoneId")
  setkeyv(final_wide, "zoneId")
  final_wide <- final_wide[all_zones_dt]
  
  for (col in setdiff(names(final_wide), "zoneId")) {
    set(final_wide, i = which(is.na(final_wide[[col]])), j = col, value = 0)
  }
  
  details <- rbindlist(detail_list, use.names = TRUE)
  all_pois <- do.call(rbind, poi_export_list_all_pois)
  all_poi_attractiveness <- do.call(rbind, poi_export_list_all_poi_attractiveness)
  
  if (!is.null(output_csv)) {
    fwrite(final_wide, output_csv)
    msg("Result written: ", output_csv)
  }
  
  if (!is.null(output_gpkg_all_pois)) {
    write_gpkg(all_pois, output_gpkg_all_pois, layer_name = "all_pois")
  }
  
  if (!is.null(output_gpkg_all_poi_attractiveness)) {
    write_gpkg(all_poi_attractiveness, output_gpkg_all_poi_attractiveness, layer_name = "all_poi_attractiveness")
  }
  
  msg("Attractiveness calculation completed.")
  msg("Duration: ", format(round(difftime(Sys.time(), start, units = "mins"), 2), nsmall = 2), " minutes.")
  list(
    zone_attractiveness = final_wide,
    zone_attractiveness_long = final_long,
    poi_details = details,
    all_pois = all_pois,
    all_poi_attractiveness = all_poi_attractiveness
  )
}

# ---------------------------------------------------------------------------
# JSON config loading
# ---------------------------------------------------------------------------

# Translation table: JSON metric type names -> internal R metric type names
.json_metric_map <- c(
  Area      = "Area",
  FloorArea = "FloorArea",
  Count    = "Count"
)

#' Load a \code{pois} list from a JSON configuration file.
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
load_pois_from_json <- function(json_path) {
  assert_true(file.exists(json_path), sprintf("JSON configuration file not found: %s", json_path))
  raw <- jsonlite::fromJSON(json_path, simplifyVector = FALSE)
  assert_true(is.list(raw) && length(raw) > 0L, "JSON configuration file is empty or not an object.")

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
    purposes <- vector("list", length(cat_spec))
    names(purposes) <- names(cat_spec)
    for (purpose in names(cat_spec)) {
      purposes[[purpose]] <- parse_purpose_spec(cat_spec[[purpose]], cat_name, purpose)
    }
    pois[[cat_name]] <- purposes
  }

  msg("load_pois_from_json: loaded ", length(pois), " categories from '", basename(json_path), "'.")
  pois
}

example_pois <- list(
  Friseur = list(PrivateBusiness = list("Count", 21)),
  Post = list(PrivateBusiness = list("Count", 372)),
  Kirche = list(PrivateBusiness = list("Area", 0.05)),
  Krankenhaus = list(PrivateVisit = list("FloorArea", 0.024))
)

cleaning_rules <- list(
  Krankenhaus = list(
    require_any = list(
      building = "hospital",
      amenity = "hospital"
    )
  )
)
