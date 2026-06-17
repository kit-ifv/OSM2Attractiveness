# %%
# -*- coding: utf-8 -*-
"""
Filter OSM data for the area of interest and categories of interest using Osmosis
Input: OSM data; filter definitions for categories of interest
Output: OSM data for the area of interest, filtered by categories of interest
"""

# Libraries #
import os
import subprocess
import time
from datetime import datetime

import yaml

# Config #
script_dir = os.path.dirname(os.path.abspath(__file__))

print("=" * 80)
print("FilterOSM")
print("Filter OSM data with categories required for the model")
print("=" * 80)
print("")
print("Configuration:")


# get config file path from command line argument, if provided, otherwise use default
config_file = None
if len(os.sys.argv) > 1:
    config_name = os.sys.argv[1]
    print("Using config file from command line argument:", config_file)
else:
    config_name = "config_rastatt_example"
    print("Using default config file:", config_name)

config_file = os.path.normpath(os.path.join(script_dir, "..", "..", "config", config_name + ".yaml"))

# check if config file exists
if not os.path.exists(config_file):
    print("Error: Config file not found at path:", config_file)
    exit(1)


with open(config_file, "r", encoding="utf-8") as f:
    area_cfg = yaml.safe_load(f)


def resolve_path(path_value):
    if os.path.isabs(path_value) or path_value.startswith("\\\\") or path_value.startswith("//"):
        return os.path.normpath(path_value)
    return os.path.abspath(os.path.join(path_value))


paths_cfg = area_cfg["paths"]
path_osmosis = resolve_path(paths_cfg["osmosis_bin"])
path_osm = resolve_path(paths_cfg["osm_raw_base"])  # .osm.pbf is attached, download from e.g. Geofabrik
path_filter = resolve_path(paths_cfg["filter_dir"])
path_export = resolve_path(paths_cfg["osm_filtered_dir"])

# other
verbose = False # whether to print the Osmosis command output even if commands are successful (can be very long, so optional)

# area of interest
area_name = area_cfg["area_name"]  # name for the area of interest, used as prefix for input and output file names, e.g. FHH_Pharmacy.osm
bbox_left = area_cfg["bbox_wgs84"]["left"]
bbox_right = area_cfg["bbox_wgs84"]["right"]
bbox_top = area_cfg["bbox_wgs84"]["top"]
bbox_bottom = area_cfg["bbox_wgs84"]["bottom"]


# Main script #
path_osm_absolute = os.path.abspath(path_osm + ".osm.pbf")
path_osm_extract_absolute = os.path.abspath(path_osm + "_extract-" + area_name + ".xml")
path_export_absolute = os.path.abspath(path_export)
path_filter_absolute = os.path.abspath(path_filter)

overall_start_time = time.time()
print("Config file:", config_file)
print("Area:", area_name)
print("Filter folder:", path_filter_absolute)
print("Input data:", path_osm_extract_absolute)
print("Output data folder:", path_export_absolute)
print("Verbose (show osmosis commands):", verbose)

# Check if Osmosis is installed and accessible
if not os.path.exists(path_osmosis):
    print("Error: Osmosis not found at path:", path_osmosis)
    exit(1)
else:
    print("Osmosis directory found at path:", path_osmosis)


# Do once: make extract of OSM data for the area of interest
if os.path.exists(path_osm_extract_absolute):
    print("OSM extract exists already, skip making extract")
else:
    print("OSM extract does not exist yet, create extract for area of interest using Osmosis")
    print("Full OSM file:", path_osm_absolute)
    # check if OSM file exists
    if not os.path.exists(path_osm_absolute):
        print("Error: OSM file not found at path:", path_osm_absolute)
        exit(1)
    print("Extract file:", path_osm_extract_absolute)
    print("Bounding box: left={}, right={}, top={}, bottom={}".format(bbox_left, bbox_right, bbox_top, bbox_bottom))

    print("\nStart creating OSM extract for area of interest using Osmosis, this may take some time...\n")

    os.chdir(path_osmosis)
    command_extract = (
        "osmosis --read-pbf " + path_osm_absolute + " outPipe.0=1 "
        + f"--bounding-box left={bbox_left} right={bbox_right} top={bbox_top} bottom={bbox_bottom} inPipe.0=1 outPipe.0=2 "
        + "--write-xml file=" + path_osm_extract_absolute + " inPipe.0=2"
    )
    if verbose:
        print("Running command:")
        print(command_extract)

    result_extract = subprocess.run(
        command_extract,
        shell=True,
        capture_output=True,
        text=True
    )
    if result_extract.returncode != 0:
        print("Error creating OSM extract. Return code:", result_extract.returncode)
        if not verbose: print("Command was:", command_extract)
        print("STDOUT:\n", result_extract.stdout)
        print("STDERR:\n", result_extract.stderr)
        # terminate script, since the extract is required for the following steps
        exit(1)
    else:
        print("OSM extract created successfully")

    if verbose or result_extract.returncode != 0:
        print("Returncode:", result_extract.returncode)
        print("STDOUT:\n", result_extract.stdout)
        print("STDERR:\n", result_extract.stderr)

if not os.path.exists(path_export_absolute):
    os.makedirs(path_export_absolute)
    print(f"Created export directory")
else:
    print(f"Export directory already exists. Note: Existing files will be overwritten")
 
# %%
def read_filter_steps(path_to_file):
    """Read filter steps from a file. One non-empty line equals one tag-filter step."""
    steps = []
    with open(path_to_file, "r", encoding="utf-8") as filter_file:
        for raw_line in filter_file:
            # Allow comments and empty lines in filter files.
            line = raw_line.split("#", 1)[0].strip()
            if line:
                steps.append(line)
    return steps


def append_tag_filter_steps(command_parts, action, object_type, steps, in_pipe, next_pipe):
    """Append sequential tag-filter commands and return the last out-pipe and next free pipe id."""
    current_pipe = in_pipe
    for step in steps:
        command_parts.append(
            f"--tag-filter {action}-{object_type} {step} inPipe.0={current_pipe} outPipe.0={next_pipe}"
        )
        current_pipe = next_pipe
        next_pipe += 1
    return current_pipe, next_pipe


def start_osmosis_filter(category, path_osm_extract, path_osmosis, path_export, path_filter, area_name, verbose=False):
    filter_white_path = os.path.join(path_filter, f"filter_{category}.txt")
    filter_black_path = os.path.join(path_filter, f"reject_{category}.txt")
    filter_white_steps = read_filter_steps(filter_white_path)
    filter_black_steps = read_filter_steps(filter_black_path)

    os.chdir(path_osmosis)

    command_parts = [
        f"--read-xml {path_osm_extract} outPipe.0=1",
        f"--read-xml {path_osm_extract} outPipe.0=2",
        f"--read-xml {path_osm_extract} outPipe.0=3",
    ]

    next_pipe = 4

    # Relations stream
    relations_pipe = 1
    relations_pipe, next_pipe = append_tag_filter_steps(
        command_parts, "accept", "relations", filter_white_steps, relations_pipe, next_pipe
    )
    relations_pipe, next_pipe = append_tag_filter_steps(
        command_parts, "reject", "relations", filter_black_steps, relations_pipe, next_pipe
    )
    relations_used_way_pipe = next_pipe
    command_parts.append(f"--used-way inPipe.0={relations_pipe} outPipe.0={relations_used_way_pipe}")
    next_pipe += 1
    relations_used_node_pipe = next_pipe
    command_parts.append(f"--used-node inPipe.0={relations_used_way_pipe} outPipe.0={relations_used_node_pipe}")
    next_pipe += 1

    # Ways stream
    ways_pipe = 2
    ways_pipe, next_pipe = append_tag_filter_steps(
        command_parts, "accept", "ways", filter_white_steps, ways_pipe, next_pipe
    )
    ways_pipe, next_pipe = append_tag_filter_steps(
        command_parts, "reject", "ways", filter_black_steps, ways_pipe, next_pipe
    )
    ways_no_relations_pipe = next_pipe
    command_parts.append(f"--tag-filter reject-relations inPipe.0={ways_pipe} outPipe.0={ways_no_relations_pipe}")
    next_pipe += 1
    ways_used_node_pipe = next_pipe
    command_parts.append(f"--used-node inPipe.0={ways_no_relations_pipe} outPipe.0={ways_used_node_pipe}")
    next_pipe += 1

    # Nodes stream
    nodes_pipe = 3
    nodes_pipe, next_pipe = append_tag_filter_steps(
        command_parts, "accept", "nodes", filter_white_steps, nodes_pipe, next_pipe
    )
    nodes_pipe, next_pipe = append_tag_filter_steps(
        command_parts, "reject", "nodes", filter_black_steps, nodes_pipe, next_pipe
    )
    nodes_no_ways_pipe = next_pipe
    command_parts.append(f"--tag-filter reject-ways inPipe.0={nodes_pipe} outPipe.0={nodes_no_ways_pipe}")
    next_pipe += 1
    nodes_no_relations_pipe = next_pipe
    command_parts.append(f"--tag-filter reject-relations inPipe.0={nodes_no_ways_pipe} outPipe.0={nodes_no_relations_pipe}")
    next_pipe += 1

    # Sorting and merging
    sorted_nodes_pipe = next_pipe
    command_parts.append(f"--sort inPipe.0={nodes_no_relations_pipe} outPipe.0={sorted_nodes_pipe}")
    next_pipe += 1
    sorted_relation_nodes_pipe = next_pipe
    command_parts.append(f"--sort inPipe.0={relations_used_node_pipe} outPipe.0={sorted_relation_nodes_pipe}")
    next_pipe += 1
    sorted_way_nodes_pipe = next_pipe
    command_parts.append(f"--sort inPipe.0={ways_used_node_pipe} outPipe.0={sorted_way_nodes_pipe}")
    next_pipe += 1

    first_merge_pipe = next_pipe
    command_parts.append(
        f"--merge inPipe.0={sorted_nodes_pipe} inPipe.1={sorted_relation_nodes_pipe} outPipe.0={first_merge_pipe}"
    )
    next_pipe += 1
    second_merge_pipe = next_pipe
    command_parts.append(
        f"--merge inPipe.0={sorted_way_nodes_pipe} inPipe.1={first_merge_pipe} outPipe.0={second_merge_pipe}"
    )

    output_file = os.path.join(path_export, f"{area_name}_{category}.osm")
    command = "osmosis " + " ".join(command_parts) + f" --write-xml {output_file} inPipe.0={second_merge_pipe}"

    if verbose:
        print("Running command:")
        print(command)

    result = subprocess.run(
        command,
        shell=True,
        capture_output=True,
        text=True
    )

    if result.returncode != 0:
        print(f"Error processing category {category}. Return code: {result.returncode}")
        if not verbose: print("Command was:", command)
    else:
        print(f"Finished processing category {category} successfully.")

    if verbose or result.returncode != 0:
        print("Returncode:", result.returncode)
        print("STDOUT:\n", result.stdout)
        print("STDERR:\n", result.stderr)

print("\nStart processing...\n")
print("Start: ", datetime.now().strftime('%Y-%m-%d %H:%M:%S'))
print("")


# %%
categories = [
    "Districts",
    "Pharmacy",
    "Doctor",
    "Bank",
    "Authority",
    "Library",
    "LongTermShopping_DIYGardenCenter",
    "LongTermShopping_FurnitureStore",
    "LongTermShopping_Other",
    "LongTermShopping_DepartmentClothingElectronics",
    "DailyShopping_BakeryButcherKiosk",
    "DailyShopping_Drugstore",
    "DailyShopping_Other",
    "DailyShopping_Supermarket",
    "EV_ChargingStation",
    "Hairdresser",
    "SwimmingPool",
    "SwimmingPoolOutdoor",
    "Beach",
    "Universities",
    "Hotel",
    "Kindergarten",
    "Cinema",
    "Museums",
    "MuseumsOutdoor",
    "Theater",
    "Restaurant",
    "Church",
    "Hospital",
    "Park",
    "Cemetery",
    "Zoo",
    "AllotmentGardens",
    "PostOffice",
    "Playground",
    "FitnessCenter",
    "SportsHall",
    "SportsField",
    "SmallSportsField",
    "Mailbox",
    "Schools",
    "RegionalRail",
    "Buildings"
]

total_categories = len(categories)

for index, category in enumerate(categories, start=1):
    print(f"Start processing category {index} of {total_categories}: {category}")
    category_start_time = time.time()
    start_osmosis_filter(
        category,
        path_osm_extract_absolute,
        path_osmosis,
        path_export_absolute,
        path_filter_absolute,
        area_name,
        verbose
    )
    category_runtime = time.time() - category_start_time
    elapsed_total = time.time() - overall_start_time
    avg_time_per_category = elapsed_total / index
    remaining = total_categories - index
    eta_seconds = avg_time_per_category * remaining
    eta_timestamp = datetime.fromtimestamp(time.time() + eta_seconds).strftime('%Y-%m-%d %H:%M:%S')
    print(f"Runtime: {category_runtime:.2f}s | Elapsed: {elapsed_total:.0f}s | Remaining: {eta_seconds:.0f}s ({remaining} categories left) | ETC: {eta_timestamp}")
    print("")

print("DONE - Runtime: {:.2f}h".format((time.time() - overall_start_time) / 3600))
