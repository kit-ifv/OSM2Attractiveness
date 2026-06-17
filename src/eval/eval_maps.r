# eval_maps

# This script visualizes calculated attractiveness results by zone type and activity type.
# It generates static PNG maps (and possibly interactive HTML maps) for zones and regular grids.
# Input data is read from the attractiveness output directory produced by POIs2attractiveness.

library(this.path)
library(yaml)
library(leaflet)
library(ggplot2)
library(data.table)

source(normalizePath(path.join(this.dir(), "eval_maps_funcs.r")))

# Which config is used? Retrieve from command line
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0L) {
  config_name <- args[1]
} else {
  config_name <- "config_rastatt_example"
}
run_subdir_arg <- if (length(args) > 1L) args[2] else NULL

config_file <- normalizePath(path.join(this.dir(), "..", "..", "config", paste0(config_name, ".yaml")), mustWork = TRUE)
area_cfg <- yaml.load_file(config_file)

area_name <- area_cfg$area_name %||% "unknown_area"
zones_file <- normalizePath(path.join(area_cfg$paths$zones_file), mustWork = FALSE)
path_output_base <- file.path(
  area_cfg$paths$attractiveness_output_root,
  area_cfg$paths$attractiveness_output_sub_attractiveness
)

eval_cfg <- area_cfg$eval$attractiveness_maps %||% list()
zone_types_cfg <- as_char_vec(eval_cfg$zone_types %||% character())
grid_cellsize_m_default <- suppressWarnings(as.numeric(eval_cfg$grid_cellsize_m_default %||% 1000))
if (!is.finite(grid_cellsize_m_default) || grid_cellsize_m_default <= 0) {
  grid_cellsize_m_default <- 1000
}
grid_cellsize_m_by_type <- as_named_numeric(
  eval_cfg$grid_cellsize_m_by_type %||% list("1" = 1000, "2" = 2000, "3" = 5000)
)

# Understand where results are saved
if (!is.null(run_subdir_arg) && nzchar(run_subdir_arg)) {
  path_output_dir <- file.path(path_output_base, run_subdir_arg)
} else {
  latest_run_dir <- find_latest_run_dir(path_output_base)
  path_output_dir <- if (!is.null(latest_run_dir)) latest_run_dir else path_output_base
}

path_output_dir_maps <- file.path(path_output_dir, "Maps")
if (!dir.exists(path_output_dir_maps)) {
  dir.create(path_output_dir_maps, recursive = TRUE)
}

timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M")
path_attractiveness <- file.path(path_output_dir, "attractiveness.csv")
path_all_poi_attractiveness <- file.path(path_output_dir, "all_poi_attractiveness.gpkg")
details_csv <- file.path(path_output_dir, "attractiveness_detailed_by_category_purpose.csv")


# Show config
cat(paste(rep("=", 80), collapse = ""), "\n", sep = "")
cat("evaluate_attractivities\n")
cat("Create attractiveness maps for zones and regular grids\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n", sep = "")
cat("Configuration:\n")
cat("Area: ", area_name, "\n", sep = "")
cat("Config file: ", config_file, "\n", sep = "")
cat("Zones file: ", zones_file, "\n", sep = "")
cat("Output base directory: ", path_output_base, "\n", sep = "")
cat("Selected output directory: ", path_output_dir, "\n", sep = "")
cat("Maps output directory: ", path_output_dir_maps, "\n", sep = "")
cat("Run subdir argument: ", run_subdir_arg %||% "<auto>", "\n", sep = "")
cat("Zone types from config: ", if (length(zone_types_cfg) > 0L) paste(zone_types_cfg, collapse = ", ") else "<auto>", "\n", sep = "")
cat("Grid cellsize default (m): ", grid_cellsize_m_default, "\n", sep = "")
cat("Grid cellsize by type: ", if (length(grid_cellsize_m_by_type) > 0L) paste(paste(names(grid_cellsize_m_by_type), grid_cellsize_m_by_type, sep = "="), collapse = ", ") else "<none>", "\n", sep = "")
cat("Attractiveness CSV: ", path_attractiveness, "\n", sep = "")
cat("POI attractiveness GPKG: ", path_all_poi_attractiveness, "\n", sep = "")
cat("Detailed CSV: ", details_csv, "\n", sep = "")
cat("Start: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n", sep = "")


# Validate required inputs
if (!file.exists(zones_file)) {
  stop(paste0("Zones file not found: ", zones_file), call. = FALSE)
}
if (!file.exists(path_attractiveness)) {
  stop(paste0("Attractiveness file not found: ", path_attractiveness), call. = FALSE)
}
if (!file.exists(path_all_poi_attractiveness)) {
  stop(paste0("POI attractiveness file not found: ", path_all_poi_attractiveness), call. = FALSE)
}
if (!file.exists(details_csv)) {
  stop(paste0("Detailed attractiveness file not found: ", details_csv), call. = FALSE)
}


# Load results
attractiveness <- fread(path_attractiveness)
if (!("zoneId" %in% names(attractiveness))) {
  stop("In attractiveness.csv the required column 'zoneId' is missing.", call. = FALSE)
}
purposes <- setdiff(names(attractiveness), "zoneId")
if (length(purposes) == 0L) {
  stop("No purpose columns found in attractiveness.csv.", call. = FALSE)
}

zones_sf <- sf::st_read(zones_file, quiet = TRUE)
zones_sf$NO <- as.character(zones_sf$NO)
zones_sf$typ <- as.character(zones_sf$typ)
attractiveness[, zoneId := as.character(zoneId)]
zones_sf <- merge(zones_sf, attractiveness, by.x = "NO", by.y = "zoneId", all.x = TRUE)

poi_attractiveness_sf <- sf::st_read(path_all_poi_attractiveness, quiet = TRUE)
if (nrow(poi_attractiveness_sf) == 0L) {
  stop("The file all_poi_attractiveness.gpkg contains no features.", call. = FALSE)
}

required_poi_cols <- c("purpose", "attractiveness")
missing_poi_cols <- setdiff(required_poi_cols, names(poi_attractiveness_sf))
if (length(missing_poi_cols) > 0L) {
  stop(
    paste0("In all_poi_attractiveness.gpkg required columns are missing: ", paste(missing_poi_cols, collapse = ", ")),
    call. = FALSE
  )
}

if (sf::st_crs(poi_attractiveness_sf) != sf::st_crs(zones_sf)) {
  poi_attractiveness_sf <- sf::st_transform(poi_attractiveness_sf, sf::st_crs(zones_sf))
}

# Attach zone type to each POI point so regular grids can be created per zone type.
zone_typ_sf <- zones_sf[, c("NO", "typ")]
poi_zone_join <- suppressWarnings(sf::st_join(poi_attractiveness_sf, zone_typ_sf, join = sf::st_within, left = FALSE))

details_dt <- data.table::fread(details_csv)
if (!("zoneId" %in% names(details_dt))) {
  stop("In attractiveness_detailed_by_category_purpose.csv the required column 'zoneId' is missing.", call. = FALSE)
}
details_dt[, zoneId := as.character(zoneId)]
zones_sf <- merge(zones_sf, details_dt, by.x = "NO", by.y = "zoneId", all.x = TRUE)

zones_sf_leaflet <- sf::st_transform(zones_sf, 4326)


# Determine zone types to process
available_zone_types <- sort(unique(zones_sf$typ))
if (length(zone_types_cfg) > 0L) {
  zone_types <- intersect(zone_types_cfg, available_zone_types)
  missing_zone_types <- setdiff(zone_types_cfg, available_zone_types)
  if (length(missing_zone_types) > 0L) {
    msg("Skipping configured zone types not present in data: ", paste(missing_zone_types, collapse = ", "))
  }
} else {
  zone_types <- available_zone_types
}
if (length(zone_types) == 0L) {
  stop("No zone types selected for map creation.", call. = FALSE)
}


# Build maps per zone type and purpose
for (zone_type in zone_types) {
  msg("Create maps for zone type: ", zone_type)

  zones_subset <- zones_sf[zones_sf$typ == zone_type, ]
  if (nrow(zones_subset) == 0L) {
    msg("Skip zone type ", zone_type, " because it has no zones.")
    next
  }

  poi_subset <- poi_zone_join[as.character(poi_zone_join$typ) == zone_type, ]
  grid_cellsize_zone <- grid_cellsize_m_default
  if (zone_type %in% names(grid_cellsize_m_by_type)) {
    grid_cellsize_zone <- as.numeric(grid_cellsize_m_by_type[[zone_type]])
  }
  msg("Regular grid resolution (m): ", grid_cellsize_zone)

  zones_grid_subset <- build_regular_grid_attractiveness(
    poi_subset = poi_subset,
    purposes = purposes,
    cellsize_m = grid_cellsize_zone
  )

  for (purpose in purposes) {
    make_image_map(zones_subset, zone_type, purpose, path_output_dir_maps)
    if (!is.null(zones_grid_subset) && nrow(zones_grid_subset) > 0L) {
      make_image_map_grid(zones_grid_subset, zone_type, purpose, path_output_dir_maps)
    }
  }

  make_overview_image_map(zones_subset, zone_type, purposes, path_output_dir_maps)
  if (!is.null(zones_grid_subset) && nrow(zones_grid_subset) > 0L) {
    make_overview_image_map_grid(zones_grid_subset, zone_type, purposes, path_output_dir_maps)
  }

  if (eval_cfg$make_html_maps == TRUE) {

    zones_sf_leaflet_subset <- zones_sf_leaflet[zones_sf_leaflet$typ == zone_type, ]
    zones_grid_leaflet_subset <- NULL
    if (!is.null(zones_grid_subset) && nrow(zones_grid_subset) > 0L) {
      zones_grid_leaflet_subset <- sf::st_transform(zones_grid_subset, 4326)
    }

    make_html_map(
      zones_sf_leaflet_subset = zones_sf_leaflet_subset,
      subset_name = as.character(zone_type),
      purposes = purposes,
      output_dir = path_output_dir_maps,
      timestamp_string = timestamp,
      grid_sf_leaflet_subset = zones_grid_leaflet_subset
    )

  }

}

msg("Finished map generation.")
