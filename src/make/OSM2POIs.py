# %%
# -*- coding: utf-8 -*-
"""
Create the data required for attractiveness calculation
Input: OSM XML files per category (e.g. FHH_Pharmacy.osm) and an OSM buildings file
Output: GeoJSON files for POIs, aggregated for downstream processing (no distinction between points and polygons etc.)
"""


# -----------------------------
# Configuration
# -----------------------------

import os
import time
from datetime import datetime
import yaml

# Project-specific configuration - read from config file
script_dir = os.path.dirname(os.path.abspath(__file__))

# get config file path from command line argument, if provided, otherwise use default
if len(os.sys.argv) > 1:
    config_name = os.sys.argv[1]
    print("Using config file from command line argument:", config_name)
else:
    config_name = "config_rastatt_example"
    print("Using default config file:", config_name)

config_file = os.path.normpath(os.path.join(script_dir, "..", "..", "config", config_name + ".yaml"))

# check if config file exists
if not os.path.exists(config_file):
    print("Error: Config file not found at path:", config_file)
    exit(1)


def resolve_path(path_value):
    if os.path.isabs(path_value) or path_value.startswith("\\\\") or path_value.startswith("//"):
        return os.path.normpath(path_value)
    return os.path.abspath(os.path.join(path_value))

with open(config_file, "r", encoding="utf-8") as f:
    area_cfg = yaml.safe_load(f)

paths_cfg = area_cfg["paths"]
pois_config_file = resolve_path(paths_cfg["pois_config"])

with open(pois_config_file, "r", encoding="utf-8") as f:
    pois_cfg = yaml.safe_load(f)



area_name = area_cfg["area_name"]  # name of the area of interested, is used as prefix for input and output file names, e.g. Region_Buildings
path_in = resolve_path(paths_cfg["osm_filtered_dir"])  # path where category-specific OSM XML files are located; these are created beforehand using FilterOSM.py
path_in_buildings = resolve_path(paths_cfg["buildings_input"])

name_dir_out = paths_cfg["pois_dir_name"]
pois_root_dir = resolve_path(paths_cfg["pois_root_dir"])
path_out = os.path.join(pois_root_dir, name_dir_out)
path_out_raw = os.path.join(path_out, "raw")

crs_proj_out = area_cfg["crs_proj_out"]
exclude_osm_ids = {entry["id"] for entry in area_cfg.get("exclude_osm_ids", [])}

default_supermarket_size_m2 = pois_cfg["default_supermarket_size_m2"]
name_based_pattern_hypermarket = pois_cfg["name_based_pattern_hypermarket"]
supermarket_brands = pois_cfg["supermarket_brands"]
supermarket_area_values = pois_cfg["supermarket_area_values"]


# general configuration (normally no need to change)
filename_buildings_in_out = "Buildings" # name of the generated buildings file
export_fileext = "geojson"
export_fileext_largefiles = "gpkg" # for buildings, because GeoJSON is too slow
extract_tags_fields = ['building:levels', 'level', 'amenity', 'leisure', 'sport', 'brand', 'natural', 'building', 'tourism', 'shop'] # these fields are read from "other_tags"


# -----------------------------
# Imports
# -----------------------------

from OSM2POIs_funcs import (
    area,
    area_by_name,
    create_buildings_file,
    floor_area,
    floor_area_buildings_only,
    floor_area_point_in_polygon,
    only_point,
)
from OSM2POIs_helpers import run_categories, run_category



# -----------------------------
# MAIN SCRIPT
# -----------------------------

path_in_absolute = os.path.abspath(path_in)
path_in_buildings_absolute = os.path.abspath(path_in_buildings)
path_out_absolute = os.path.abspath(path_out)
path_out_raw_absolute = os.path.abspath(path_out_raw)

processing_cfg = {
    "area_name": area_name,
    "path_in": path_in_absolute,
    "path_out": path_out_absolute,
    "path_out_raw": path_out_raw_absolute,
    "filename_buildings_in_out": filename_buildings_in_out,
    "crs_proj_out": crs_proj_out,
    "export_fileext": export_fileext,
    "export_fileext_largefiles": export_fileext_largefiles,
    "extract_tags_fields": extract_tags_fields,
    "exclude_ids": exclude_osm_ids,
    "supermarket_brands": supermarket_brands,
    "supermarket_area_values": supermarket_area_values,
    "default_supermarket_size_m2": default_supermarket_size_m2,
    "name_based_pattern_hypermarket": name_based_pattern_hypermarket,
}


overall_start_time = time.time()
print("=" * 80)
print("OSM2POI")
print("Create point objects in aggregated categories based on OSM data")
print("=" * 80)
print("")
print("Configuration:")
print("Area:", area_name)
print("Input folder:", path_in_absolute)
print("Output folder:", path_out_absolute)
print("Raw output folder:", path_out_raw_absolute)
print("\nStarting processing...\n")
print("Start: ", datetime.now().strftime('%Y-%m-%d %H:%M:%S'))
print("\n")

# Create output directories if they do not exist yet.
for directory in [path_out_absolute, path_out_raw_absolute]:
    if not os.path.exists(directory):
        os.makedirs(directory)
        print(f"Create directory: {directory}")
    else:
        print(f"Directory already exists: {directory}")

# Create buildings file first. Only is done when the file does not exist yet.
if not os.path.exists(os.path.join(path_out_absolute, f"{area_name}_Buildings.{export_fileext_largefiles}")):
    print("Create file: Buildings")
    create_buildings_file(
        f"{area_name}_Buildings",
        path_in_buildings_absolute,
        path_out_absolute,
        extract_tags_fields,
        crs_proj_out,
        export_fileext_largefiles,
    )
else:
    print(f"Skip: {os.path.join(path_out_absolute, area_name + '_Buildings.' + export_fileext_largefiles)} already exists.")

# Point categories
run_categories([
    'PostOffice','Mailbox','RegionalRail',
    'Pharmacy','Doctor',
    'Bank','Authority','DailyShopping_BakeryButcherKiosk',
    'EV_ChargingStation','Hairdresser','Universities'
], only_point, area_name, processing_cfg)

# Area categories
run_categories([
    'Playground','FitnessCenter','SportsHall','SportsField','SmallSportsField','SwimmingPool','SwimmingPoolOutdoor','Beach',
    'Restaurant','Church','Park','Cemetery','Zoo','AllotmentGardens', 'DailyShopping_Drugstore', 'MuseumsOutdoor'
], area, area_name, processing_cfg)

# Floor area
run_categories([
    'Library', 'LongTermShopping_DepartmentClothingElectronics', 'Hotel', 'Cinema', 'Museums', 'Theater',
    'LongTermShopping_Other', 'DailyShopping_Other', 'Kindergarten',
], floor_area, area_name, processing_cfg)

# Floor area using buildings only
run_categories([
    'Hospital'
], floor_area_buildings_only, area_name, processing_cfg)


# Additional special cases
run_categories([
    'LongTermShopping_FurnitureStore', 'LongTermShopping_DIYGardenCenter'
], floor_area_point_in_polygon, area_name, processing_cfg)

run_category('DailyShopping_Supermarket', area_by_name, area_name, processing_cfg)

print("")
print("FINISHED - Runtime: {:.2f}h".format((time.time() - overall_start_time) / 3600))

