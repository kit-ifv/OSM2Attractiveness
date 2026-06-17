# POIs2attractiveness

# This script calculates attractiveness for zones based on POIs
# Input:
# - POI data in GeoJSON format (one file per category/purpose, e.g. "long_term_shopping.geojson")
# - Zone geometries (e.g. as GeoJSON or shapefile)
# - Configuration of how to calculate attractiveness from POI metrics (e.g. size, levels, etc.) in a JSON file
# Output:
# - CSV file with calculated attractiveness per zone
# - GeoPackage files with all POIs and their calculated attractiveness 
# - CSV file with attractiveness per POI and purpose (recommended to use as base for futher calculations)
# Note: Output data should be validated afterwards as in every region there are specific peculiarities in the POI data that may require adjustments to cleaning rules or attractiveness calculation.

library(this.path)
library(yaml)
source(normalizePath(path.join(this.dir(), "POIs2attractiveness_helpers.r")))
source(normalizePath(path.join(this.dir(), "POIs2attractiveness_funcs.r")))
source(normalizePath(path.join(this.dir(), "POIs2attractiveness_main.r")))

# Which config is used?
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  config_name <- args[1]
} else {
  config_name <- "config_rastatt_example"
}
config_file <- normalizePath(path.join(this.dir(), "..", "..", "config", paste0(config_name, ".yaml")))

# Set-up all paths etc.
area_cfg <- yaml.load_file(config_file)

area_name <- area_cfg$area_name
pois_root <- normalizePath(path.join(area_cfg$paths$pois_root_dir))
poi_dir <- normalizePath(path.join(pois_root, area_cfg$paths$pois_dir_name))

zones_file <- area_cfg$paths$zones_file
percentile_imputation <- area_cfg$percentile_imputation %||% 0.15

timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M")
path_output_dir <- paste(
	area_cfg$paths$attractiveness_output_root,
	area_cfg$paths$attractiveness_output_sub_attractiveness,
	paste("run", timestamp, sep = "_"),
  sep = "/"
)
path_output_csv <- paste(path_output_dir, "attractiveness.csv", sep = "/")
path_output_gpkg_all_pois <- paste(path_output_dir, "all_pois.gpkg", sep = "/")
path_output_gpkg_all_poi_attractiveness <- paste(path_output_dir, "all_poi_attractiveness.gpkg", sep = "/")
poi_file_prefix <- paste0(area_name, "_")
poi_file_suffix <- ".geojson"
zone_id_field <- "NO"
calc_defaults <- list(
	area_field = "Area",
	levels_field = "Level_number",
	floor_field = "FloorArea",
	level_default = 1
)

attractiveness_factors_json <- normalizePath(path.join(area_cfg$paths$attractiveness_factors_json))



# Show config

cat(paste(rep("=", 80), collapse = ""), "\n", sep = "")
cat("POIs2attractiveness\n")
cat("Calculate zone attractiveness from POI files\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n", sep = "")
cat("Configuration:\n")
cat("Area: ", area_name, "\n", sep = "")
cat("Config file: ", config_file, "\n", sep = "")
cat("POIs root: ", pois_root, "\n", sep = "")
cat("POI input folder: ", poi_dir, "\n", sep = "")
cat("Zones file: ", zones_file, "\n", sep = "")
cat("Attractiveness factors JSON: ", attractiveness_factors_json, "\n", sep = "")
cat("POI file prefix: ", poi_file_prefix, "\n", sep = "")
cat("POI file suffix: ", poi_file_suffix, "\n", sep = "")
cat("Zone ID field: ", zone_id_field, "\n", sep = "")
cat("Percentile for area imputation: ", percentile_imputation, "\n", sep = "")
cat("Defaults - area field name: ", calc_defaults$area_field, "\n", sep = "")
cat("Defaults - levels field name: ", calc_defaults$levels_field, "\n", sep = "")
cat("Defaults - floor field name: ", calc_defaults$floor_field, "\n", sep = "")
cat("Defaults - level default value: ", calc_defaults$level_default, "\n", sep = "")
cat("Cleaning rules categories: ", paste(names(cleaning_rules), collapse = ", "), "\n", sep = "")
cat("Output directory: ", path_output_dir, "\n", sep = "")
cat("Output CSV: ", path_output_csv, "\n", sep = "")
cat("Output GPKG all POIs: ", path_output_gpkg_all_pois, "\n", sep = "")
cat("Output GPKG all POI attractiveness: ", path_output_gpkg_all_poi_attractiveness, "\n", sep = "")
cat("Start: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n", sep = "")


if (!dir.exists(path_output_dir)) {
  dir.create(path_output_dir, recursive = TRUE)
	cat("Created output directory: ", path_output_dir, "\n", sep = "")
} else {
	cat("Output directory already exists: ", path_output_dir, "\n", sep = "")
}

attractiveness_factors <- load_attractiveness_factors_from_json(attractiveness_factors_json)


# Do actual processing

res <- calculate_attractiveness(
	poi_dir = poi_dir,
	zones_file = zones_file,
	attractiveness_factors = attractiveness_factors,
	poi_file_prefix = poi_file_prefix,
	poi_file_suffix = poi_file_suffix,
	zone_id_field = zone_id_field,
	cleaning_rules = cleaning_rules,
	percentile_imputation = percentile_imputation,
	defaults = calc_defaults,
	output_csv = path_output_csv,
	output_gpkg_all_pois = path_output_gpkg_all_pois,
	output_gpkg_all_poi_attractiveness = path_output_gpkg_all_poi_attractiveness
)


print(res$zone_attractiveness)


results_detailed_wide <- dcast(res$all_poi_attractiveness, zoneId ~ category + purpose, value.var = "attractiveness", fill = 0, fun.aggregate = sum)
fwrite(results_detailed_wide, paste(path_output_dir, "attractiveness_detailed_by_category_purpose.csv", sep = "/"))


# Create CSV file with attractiveness per POI, with which we can perform further calculation for final attractiveness values by zone
# (allow POI-ID-based adjustments, add other attractiveness data sources, ...)
fwrite(res$all_poi_attractiveness, paste(path_output_dir, "attractiveness_detailed_all_pois.csv", sep = "/"))
