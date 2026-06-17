library(data.table)
library(sf)

linear_size_multiplier <- function(size_values, size_min, size_max, mult_at_min, mult_at_max, size_cap = NULL) {
  sz <- as.numeric(size_values)
  
  assert_true(is.finite(size_min) && is.finite(size_max) && size_max > size_min, "Invalid bounds for linear interpolation.")
  assert_true(is.finite(mult_at_min) && is.finite(mult_at_max), "Invalid multiplier values for linear interpolation.")
  if (!is.null(size_cap)) {
    assert_true(is.finite(size_cap) && size_cap > size_max,
      "size_cap must be finite and greater than size_max (the end of the linear ramp).")
  }

  result <- rep(NA_real_, length(sz))
  valid <- is.finite(sz)
  if (!any(valid)) return(rep(1, length(sz)))

  sz_v <- sz[valid]
  t <- (sz_v - size_min) / (size_max - size_min)
  t <- pmax(0, pmin(1, t))
  result[valid] <- mult_at_min + t * (mult_at_max - mult_at_min)

  # Cap the adjusted metric above the active upper bound:
  # - 4-param form: bound is size_max
  # - 5-param form: bound is size_cap (with a plateau at mult_at_max on (size_max, size_cap])
  upper_bound <- if (is.null(size_cap)) size_max else size_cap
  cap_idx <- valid & sz > upper_bound
  if (any(cap_idx)) {
    result[cap_idx] <- mult_at_max * upper_bound / sz[cap_idx]
  }

  # treat missing/invalid size values neutrally
  result[!valid] <- 1
  result
}

# Build a numeric adjuster function from a string specification
# The string is expected to be a call to a supported adjuster function (currently only linear_size_multiplier) with numeric arguments, either all positional or all named. 
# The returned function takes (dt, metric_values) and applies the specified adjustment.
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
  required_names_4 <- c("size_min", "size_max", "mult_at_min", "mult_at_max")
  required_names_5 <- c("size_min", "size_max", "size_cap", "mult_at_min", "mult_at_max")

  eval_num <- function(a_expr, a_label) {
    v <- suppressWarnings(as.numeric(tryCatch(eval(a_expr, envir = baseenv()), error = function(e) NA_real_)))
    assert_true(length(v) == 1L && is.finite(v), sprintf(
      "Category '%s', purpose '%s': adjuster argument '%s' must evaluate to one finite numeric value.",
      category, purpose, a_label
    ))
    as.numeric(v)
  }

  
  size_cap <- NULL
  
  if (named) {
    
    assert_true(all(nzchar(arg_names)), sprintf(
      "Category '%s', purpose '%s': if one adjuster argument is named, all must be named.",
      category, purpose
    ))
    assert_true(setequal(arg_names, required_names_4) || setequal(arg_names, required_names_5), sprintf(
      "Category '%s', purpose '%s': named adjuster arguments must be exactly: %s (or include size_cap for the 5-argument form).",
      category, purpose, paste(required_names_4, collapse = ", ")
    ))

    size_min <- eval_num(args[["size_min"]], "size_min")
    size_max <- eval_num(args[["size_max"]], "size_max")
    mult_at_min <- eval_num(args[["mult_at_min"]], "mult_at_min")
    mult_at_max <- eval_num(args[["mult_at_max"]], "mult_at_max")
    if ("size_cap" %in% arg_names) size_cap <- eval_num(args[["size_cap"]], "size_cap")
    
  } else {
    
    assert_true(length(args) %in% c(4L, 5L), sprintf(
      "Category '%s', purpose '%s': positional adjuster call must have 4 or 5 numeric arguments.",
      category, purpose
    ))
    
    size_min <- eval_num(args[[1L]], "size_min")
    size_max <- eval_num(args[[2L]], "size_max")
    if (length(args) == 5L) {
      size_cap   <- eval_num(args[[3L]], "size_cap")
      mult_at_min <- eval_num(args[[4L]], "mult_at_min")
      mult_at_max <- eval_num(args[[5L]], "mult_at_max")
    } else {
      mult_at_min <- eval_num(args[[3L]], "mult_at_min")
      mult_at_max <- eval_num(args[[4L]], "mult_at_max")
    }
    
  }

  assert_true(size_max > size_min, sprintf(
    "Category '%s', purpose '%s': adjuster requires size_max > size_min.", category, purpose
  ))
  
  print(sprintf(
    "Category '%s', purpose '%s': parsed adjuster with size_min=%.0f, size_max=%.0f, mult_at_min=%.5f, mult_at_max=%.5f%s.",
    category, purpose, size_min, size_max, mult_at_min, mult_at_max,
    if (!is.null(size_cap)) sprintf(", size_cap=%.0f", size_cap) else ""
  ))

  # Here is the final function, created with the parsed parameters
  function(dt, metric_values) {
    linear_size_multiplier(
      size_values = metric_values,
      size_min = size_min,
      size_max = size_max,
      mult_at_min = mult_at_min,
      mult_at_max = mult_at_max,
      size_cap = size_cap
    )
  }
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

extract_category_purpose_specs <- function(category_spec) {
  category_spec[setdiff(names(category_spec), c("filter", "percentile_imputation"))]
}

resolve_category_percentile_imputation <- function(category_spec, default_percentile, category) {
  p <- category_spec$percentile_imputation %||% default_percentile
  p <- as.numeric(p)
  assert_true(length(p) == 1L && is.finite(p) && p > 0 && p < 1,
              sprintf("Category '%s': percentile_imputation must be a finite value in (0, 1).", category))
  p
}

apply_category_filter <- function(dt, category_spec, category, defaults) {
  filter_cfg <- category_spec$filter
  if (is.null(filter_cfg)) return(dt)

  out <- dt
  if (!is.null(filter_cfg$min_area)) {
    min_area <- as.numeric(filter_cfg$min_area)
    area_field <- as.character(filter_cfg$area_field %||% defaults$area_field)

    assert_true(length(min_area) == 1L && is.finite(min_area) && min_area >= 0,
                sprintf("Category '%s': filter.min_area must be a finite numeric value >= 0.", category))
    assert_true(length(area_field) == 1L && nzchar(area_field) && area_field %in% names(out),
                sprintf("Category '%s': filter area field '%s' is missing.", category, area_field))

    area_vals <- suppressWarnings(as.numeric(out[[area_field]]))
    keep <- is.na(area_vals) | area_vals >= min_area
    n_before <- nrow(out)
    out <- out[keep]
    msg("Category '", category, "': category filter min_area>=", format(round(min_area, 6), nsmall = 6),
        " on field '", area_field, "' reduced records from ", n_before, " to ", nrow(out), ".")
  }

  out
}

impute_with_percentile <- function(x, label, percentile = 0.15) {
  x <- as.numeric(x)
  idx_missing <- which(is.na(x))
  if (!length(idx_missing)) return(list(values = x, imputed = 0L, p_n = NA_real_))
  
  valid <- x[!is.na(x) & is.finite(x) & x > 0]
  assert_true(length(valid) > 0L, sprintf("No valid values available to impute '%s' via the %sth percentile.", label, format(round(percentile * 100, 0), nsmall = 0)))
  
  p_n <- as.numeric(stats::quantile(valid, probs = percentile, na.rm = TRUE, names = FALSE, type = 7))
  assert_true(is.finite(p_n) && p_n > 0, sprintf("Invalid %sth percentile for '%s'.", format(round(percentile * 100, 0), nsmall = 0), label))
  
  x[idx_missing] <- p_n
  list(values = x, imputed = length(idx_missing), p_n = p_n)
}

ensure_numeric_field <- function(dt, field_name, category, purpose) {
  assert_true(field_name %in% names(dt), sprintf("Category '%s', purpose '%s': required field '%s' is missing.", category, purpose, field_name))
  suppressWarnings(v <- as.numeric(dt[[field_name]]))
  assert_true(any(!is.na(v)), sprintf("Category '%s', purpose '%s': field '%s' contains no numerically usable values.", category, purpose, field_name))
  v
}

calc_metric_vector <- function(dt, category, purpose, spec, defaults, percentile_imputation) {
  metric <- spec$metric
  area_field <- if (!is.na(spec$area_field)) spec$area_field else defaults$area_field
  levels_field <- if (!is.na(spec$levels_field)) spec$levels_field else defaults$levels_field
  floor_field <- if (!is.na(spec$floor_field)) spec$floor_field else defaults$floor_field

  if (metric == "Count") {
    return(rep(1, nrow(dt)))
  }
  
  if (metric == "Area") {
    area <- ensure_numeric_field(dt, area_field, category, purpose)
    imp <- impute_with_percentile(area, sprintf("%s/%s/%s", category, purpose, area_field), percentile_imputation)
    if (imp$imputed > 0L) {
      msg("Category '", category, "', purpose '", purpose, "': area imputation: ", imp$imputed, " features imputed; value used (P", format(round(percentile_imputation * 100, 0), nsmall = 0), ")=", format(round(imp$p_n, 6), nsmall = 6), ".")
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
    area_imp <- impute_with_percentile(area, sprintf("%s/%s/%s", category, purpose, area_field, percentile_imputation))
    area2 <- area_imp$values
    if (area_imp$imputed > 0L) {
      msg("Category '", category, "', purpose '", purpose, "': area imputation (for floor area): ", area_imp$imputed, " features imputed; value used (P", format(round(percentile_imputation * 100, 0), nsmall = 0), ")=", format(round(area_imp$p_n, 6), nsmall = 6), ".")
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



cleaning_rules <- list(
  Krankenhaus = list(
    require_any = list(
      building = "hospital",
      amenity = "hospital"
    )
  )
)
