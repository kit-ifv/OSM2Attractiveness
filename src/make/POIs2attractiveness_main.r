calculate_attractiveness <- function(
    poi_dir,
    zones_file,
    attractiveness_factors,
    poi_file_prefix = "",
    poi_file_suffix = ".geojson",
    zone_id_field = "zoneId",
    cleaning_rules = list(),
    percentile_imputation = 0.15,
    defaults = list(area_field = "Area", levels_field = "Level_number", floor_field = "FloorArea", level_default = 1),
    output_csv = NULL,
    output_gpkg_all_pois = NULL,
    output_gpkg_all_poi_attractiveness = NULL
) {
  msg("Starting attractiveness calculation.")
  validate_categories_map(attractiveness_factors)
  
  assert_true(dir.exists(poi_dir), sprintf("POI folder does not exist: %s", poi_dir))
  assert_true(file.exists(zones_file), sprintf("Zone file does not exist: %s", zones_file))
  report_poi_coverage(poi_dir, attractiveness_factors, file_prefix = poi_file_prefix, file_suffix = poi_file_suffix)
  
  zones_sf <- sf::st_read(zones_file, quiet = TRUE)
  assert_true(nrow(zones_sf) > 0L, "Zone file contains no features.")
  assert_true(zone_id_field %in% names(zones_sf), sprintf("Zone field '%s' is missing in the zone file.", zone_id_field))
  zones_sf[[zone_id_field]] <- as.character(zones_sf[[zone_id_field]])
  
  zone_ids_all <- unique(as.character(zones_sf[[zone_id_field]]))
  agg_list <- list()
  poi_export_list_all_pois <- list()
  poi_export_list_by_purpose <- list()

  # get file size of all POI files for reporting
  poi_filenames <- paste0(poi_dir, "/", poi_file_prefix, names(attractiveness_factors), poi_file_suffix)
  poi_sizes <- sapply(poi_filenames, function(f) file.size(f))
  total_size_mb <- sum(poi_sizes) / (1024 * 1024)
  size_done <- 0
  n_files <- length(attractiveness_factors)
  msg("Processing ", n_files, " POI files with a total size of ", format(round(total_size_mb, 2), nsmall = 2), " MB.")
  
  # start processing loop
  start <- Sys.time()
  msg("Processing start: ", format(start, "%Y-%m-%d %H:%M:%S"))
  
  for (i in seq_along(names(attractiveness_factors))) {
    
    category <- names(attractiveness_factors)[i]
    
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
    
    this_category_pois_dt <- as.data.table(sf::st_drop_geometry(poi_sf))
    this_category_pois_dt[, source_row__ := .I]
    this_category_pois_dt <- apply_cleaning_rules(this_category_pois_dt, category, cleaning_rules)
    assert_true(nrow(this_category_pois_dt) > 0L, sprintf("Category '%s': no records remain after cleaning.", category))
    
    poi_sf <- poi_sf[this_category_pois_dt$source_row__, ]
    this_category_pois_dt[, source_row__ := NULL]
    
    zone_ids <- assign_zones(poi_sf, zones_sf, zone_id_field, category)
    this_category_pois_dt[, zoneId := zone_ids]
    keep_idx <- !is.na(zone_ids)
    this_category_pois_dt <- this_category_pois_dt[keep_idx]
    poi_sf <- poi_sf[keep_idx, ]
    assert_true(nrow(this_category_pois_dt) > 0L, sprintf("Category '%s': no POIs within zones.", category))

    # Generate deterministic internal IDs using a hashed category prefix.
    n_pois_category <- nrow(this_category_pois_dt)
    category_prefix <- hash_category_prefix(category)
    poi_ids <- sprintf("%s_%06d", category_prefix, seq_len(n_pois_category))
    this_category_pois_dt[, poi_id := poi_ids]
    this_category_pois_dt[, category := category]

    poi_export_base <- sf::st_as_sf(
      cbind(as.data.frame(this_category_pois_dt), geometry = sf::st_geometry(poi_sf)),
      crs = sf::st_crs(poi_sf)
    )
    poi_export_list_all_pois[[length(poi_export_list_all_pois) + 1L]] <- poi_export_base
    
    cat_specs <- attractiveness_factors[[category]]
    
    # Apply category-specific overrides for filtering and imputation
    this_category_pois_dt <- apply_category_filter(this_category_pois_dt, cat_specs, category, defaults)
    percentile_imputation_this_category <- resolve_category_percentile_imputation(cat_specs, percentile_imputation, category)

    # Loop through all trip purposes defined for this category
    # Calculate attractiveness values in this step for each POI
    for (purpose in setdiff(names(cat_specs), c("filter", "percentile_imputation"))) {
      spec <- extract_spec(cat_specs[[purpose]])
      
      metric_vals <- calc_metric_vector(this_category_pois_dt, category, purpose, spec, defaults, percentile_imputation_this_category)
      metric_vals_adjusted <- apply_adjuster(metric_vals, this_category_pois_dt, spec, category, purpose)
      attractiveness_vals <- metric_vals_adjusted * spec$coefficient
      
      tmp <- data.table(
        poi_id = this_category_pois_dt$poi_id,
        zoneId = this_category_pois_dt$zoneId,
        category = category,
        purpose = purpose,
        attractiveness = attractiveness_vals
      )
      
      tmp_agg <- tmp[, list(attractiveness = sum(.SD[[1L]], na.rm = TRUE)), by = c("zoneId", "purpose"), .SDcols = "attractiveness"]
      agg_list[[length(agg_list) + 1L]] <- tmp_agg
      
      poi_export_list_by_purpose[[length(poi_export_list_by_purpose) + 1L]] <- tmp[, .SD, .SDcols = c("poi_id", "zoneId", "category", "purpose", "attractiveness")]
      
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
    final_wide[is.na(get(col)), (col) := 0]
  }
  
  
  # Make datasets with all POIs (for analysis and debugging)
  all_pois <- rbindlist(poi_export_list_all_pois, use.names = TRUE, fill = TRUE)

  all_poi_attractiveness <- rbindlist(poi_export_list_by_purpose, use.names = TRUE)
  all_poi_attractiveness_extended <- merge(all_poi_attractiveness, all_pois[, .(poi_id, osm_id = id, osm_name = name, origin, Area)], by = "poi_id", all.x = T, suffixes = c("", "_poi"))
  
  
  # Attach geometry
  poi_ids_all <- unlist(lapply(poi_export_list_all_pois, function(x) x$poi_id), use.names = FALSE)
  geoms_all   <- do.call(c, lapply(poi_export_list_all_pois, sf::st_geometry))
  
  geom_idx <- match(all_poi_attractiveness_extended$poi_id, poi_ids_all)
  assert_true(!anyNA(geom_idx), "Some poi_ids in all_poi_attractiveness_extended have no matching geometry.")
  
  all_poi_attractiveness_extended_sf <- sf::st_as_sf(
    cbind(as.data.frame(all_poi_attractiveness_extended),
          geometry = geoms_all[geom_idx]),
    crs = sf::st_crs(geoms_all)
  )  
  
  
  # Export results
  if (!is.null(output_csv)) {
    fwrite(final_wide, output_csv)
    msg("Result written: ", output_csv)
  }
  
  if (!is.null(output_gpkg_all_pois)) {
    write_gpkg(all_pois, output_gpkg_all_pois, layer_name = "all_pois")
  }
  
  if (!is.null(output_gpkg_all_poi_attractiveness)) {
    write_gpkg(all_poi_attractiveness_extended_sf, output_gpkg_all_poi_attractiveness, layer_name = "all_poi_attractiveness")
  }
  
  msg("Attractiveness calculation completed.")
  msg("Duration: ", format(round(difftime(Sys.time(), start, units = "mins"), 2), nsmall = 2), ".")
  
  # Return
  list(
    zone_attractiveness = final_wide,
    zone_attractiveness_long = final_long,
    all_pois = all_pois,
    all_poi_attractiveness = all_poi_attractiveness_extended
  )
}

