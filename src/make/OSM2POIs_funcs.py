import os
import ast
import pandas as pd
import geopandas as gpd
from shapely.ops import unary_union
from shapely.validation import make_valid
import re
import time
from datetime import datetime

from OSM2POIs_helpers import (
    build_supermarket_pattern_specs,
    clean_polygons,
    export_raw_and_reduced,
    extract_tags,
    filter_points_outside_polygons,
    filter_supporting_elements,
    get_n_levels,
    get_union_geometry,
    load_buildings_aligned,
    load_osm_layers,
    normalize_brand_text,
    reduce_columns,
    remove_joint_suffix,
    simplify_fields_after_join,
    to_centroids,
)


# -----------------------------
# Functions
# -----------------------------
def create_buildings_file(category, path_in_buildings, path_out, extract_tags_fields, crs_proj_out, export_fileext_largefiles):
    """Filter buildings (based on .osm-file or repaired OSM-file (gpkg)), compute Level_number, export."""
    osm_file = os.path.join(path_in_buildings)

    # Read original buildings file
    if osm_file.lower().endswith('.osm'):
        print(f"Reading OSM file '{osm_file}' for buildings. Please be aware there might be invalid geometries, which will be skipped. Consider inspecting the buildings file in a GIS software like QGIS and export a tidy buildings file in GeoPackage format. You can then reference the GeoPackage file in this processing step.")
        read_kwargs = {'layer': 'multipolygons'}
    else:
        read_kwargs = {}

    gdf = gpd.read_file(osm_file, on_invalid='ignore', **read_kwargs)
    n_invalid = gdf.geometry.isna().sum()
    n_valid = gdf.geometry.notna().sum()
    print(f"Geometries read: {n_valid} valid, {n_invalid} invalid/unreadable.")
    if n_invalid > 0:
        print(f"Warning: {n_invalid} feature(s) with invalid/unreadable geometries, they were skipped. "
              f"Consider inspecting '{osm_file}' in GIS software for details.")
    gdf = gdf[gdf.geometry.notna()].set_crs('EPSG:4326', allow_override=True).to_crs(crs_proj_out)

    # Filter: only rows with 'building'
    if 'building' in gdf.columns:
        gdf = gdf[gdf['building'].notna() & (gdf['building'] != '')]


    gdf_poly = gdf[gdf.geometry.type.isin(['Polygon', 'MultiPolygon'])].copy()

    # Compute area
    gdf_poly['Area'] = gdf_poly.geometry.area

    # Extract relevant tags
    gdf_poly = extract_tags(gdf_poly, extract_tags_fields)

    # Determine Level_number
    gdf_poly['Level_number'] = gdf_poly.apply(get_n_levels, axis=1)

    # Export
    out_path = os.path.join(path_out, f"{category}.{export_fileext_largefiles}") 
    # GeoPackage was around 60x faster than GeoJSON in building tests, so it is used here
    gdf_poly.to_file(out_path)

def only_point(category, cfg):
    """For point categories."""

    # Define input file path
    osm_file = os.path.join(cfg["path_in"], f"{category}.osm")

    gdf_pts, gdf_poly = load_osm_layers(
        osm_file,
        crs_proj_out=cfg["crs_proj_out"],
        exclude_ids=cfg.get("exclude_ids"),
        include_points=True,
        include_polygons=True,
        allow_missing_polygons=True,
    )
    gdf_poly = clean_polygons(gdf_poly)
    
    # Filter supporting-element when present
    gdf_poly = filter_supporting_elements(gdf_poly)
    gdf_pts = filter_supporting_elements(gdf_pts)
    print("After filter 'supporting-element' = 'no': Polygons: ", len(gdf_poly), ", Points: ", len(gdf_pts))

    gdf_pts = extract_tags(gdf_pts, cfg["extract_tags_fields"])
    gdf_poly = extract_tags(gdf_poly, cfg["extract_tags_fields"])

    # Merge: points + centroids
    # Approach: keep all points outside polygons; create centroids for polygons
    if not gdf_poly.empty:
        gdf_cent = to_centroids(gdf_poly, origin="polygon")

        poly_union = get_union_geometry(gdf_poly)
        gdf_pts_out = filter_points_outside_polygons(gdf_pts, poly_union)
        gdf_pts_out["origin"] = "point"
        gdf_out = pd.concat([gdf_pts_out, gdf_cent], ignore_index=True)
    else:
        gdf_out = gdf_pts.copy()
        gdf_out["origin"] = "point"

    export_raw_and_reduced(
        gdf_out,
        category,
        path_out_raw=cfg["path_out_raw"],
        path_out=cfg["path_out"],
        export_fileext=cfg["export_fileext"],
        columns_to_keep_add=['natural', 'sport', 'leisure', 'amenity', 'brand', 'shop'],
    )

    

def area(category, cfg):
    """For area-based types. Use their polygons and add matching point objects."""
    # Define input file path
    osm_file = os.path.join(cfg["path_in"], f"{category}.osm")

    gdf_pts, gdf_poly = load_osm_layers(
        osm_file,
        crs_proj_out=cfg["crs_proj_out"],
        exclude_ids=cfg.get("exclude_ids"),
        include_points=True,
        include_polygons=True,
    )
    gdf_poly = clean_polygons(gdf_poly)

    # Filter: only elements that are not "supporting-element"
    gdf_poly = filter_supporting_elements(gdf_poly)
    gdf_pts = filter_supporting_elements(gdf_pts)
    print("After filter 'supporting-element' = 'no': Polygons: ", len(gdf_poly), ", Points: ", len(gdf_pts))

    ## Polygon handling ##
    # Extract additional available attributes for potential later use
    gdf_poly = extract_tags(gdf_poly, cfg["extract_tags_fields"])

    merged_geom = get_union_geometry(gdf_poly)
    # A code block that merged and split polygons again was removed here.
    # TODO: Clarify what its original purpose was.

    # Area calculation
    gdf_poly_merged = gdf_poly.copy()
    gdf_poly_merged['Area'] = gdf_poly_merged.geometry.area

    # Create centroids of merged areas
    gdf_cent = to_centroids(gdf_poly_merged, origin="polygon")

    ## Point handling ##
    # Determine points outside polygons
    gdf_pts_out = filter_points_outside_polygons(gdf_pts, merged_geom)
    gdf_pts_out = extract_tags(gdf_pts_out, cfg["extract_tags_fields"])
    gdf_pts_out["origin"] = "point"

    ## Merge and export ##
    gdf_out = pd.concat([gdf_pts_out, gdf_cent], ignore_index=True)
    export_raw_and_reduced(
        gdf_out,
        category,
        path_out_raw=cfg["path_out_raw"],
        path_out=cfg["path_out"],
        export_fileext=cfg["export_fileext"],
        columns_to_keep_add=['amenity', 'leisure', 'sport', 'brand', 'natural', 'shop'],
    )



def floor_area(category, cfg):
    """For POI categories with floor area as target metric. Use polygons (including Level_number) and also add points."""
    # Why include points here? Some POIs are mapped as points, not areas.
    # Example: a district library inside a larger complex such as a shopping center.
    # A known limitation is that these points have no direct area information.
    # Possible future improvement: intersect with buildings, count POIs per building,
    # and split building area evenly across POIs.

    # Define input file path
    osm_file = os.path.join(cfg["path_in"], f"{category}.osm")

    gdf_pts, gdf_poly = load_osm_layers(
        osm_file,
        crs_proj_out=cfg["crs_proj_out"],
        exclude_ids=cfg.get("exclude_ids"),
        include_points=True,
        include_polygons=True,
    )


    ## Polygon handling ##
    # Clean geometries
    gdf_poly = clean_polygons(gdf_poly)

    # Compute area
    gdf_poly['Area'] = gdf_poly.geometry.area

    # Extract additional attributes from "other_tags"
    gdf_poly = extract_tags(gdf_poly, cfg["extract_tags_fields"])

    # Determine Level_number
    gdf_poly['Level_number'] = gdf_poly.apply(get_n_levels, axis=1)

    # Create polygon centroids
    gdf_cent = to_centroids(gdf_poly, origin="polygon")


    ## Point handling ##
    # Determine points outside the polygons above
    poly_union = get_union_geometry(gdf_poly)
    gdf_pts = extract_tags(gdf_pts, cfg["extract_tags_fields"])
    gdf_pts_out = filter_points_outside_polygons(gdf_pts, poly_union)
    gdf_pts_out["origin"] = "point" 

    ## Merge and export ##
    # Merge
    gdf_out = pd.concat([gdf_pts_out, gdf_cent], ignore_index=True)
    export_raw_and_reduced(
        gdf_out,
        category,
        path_out_raw=cfg["path_out_raw"],
        path_out=cfg["path_out"],
        export_fileext=cfg["export_fileext"],
        columns_to_keep_add=['building', 'amenity', 'shop'],
    )


def floor_area_buildings_only(category, cfg):
    """Like floor_area, but based on building polygons."""
    # Background: e.g. hospitals are often mapped as one large polygon in OSM.
    # These polygons include much more than the actual buildings,
    # while buildings are the relevant objects for attractiveness.
    # Therefore, building polygons inside relevant areas are used via spatial join.

    # Define input file path
    osm_file = os.path.join(cfg["path_in"], f"{category}.osm")

    _, gdf_poly = load_osm_layers(
        osm_file,
        crs_proj_out=cfg["crs_proj_out"],
        exclude_ids=cfg.get("exclude_ids"),
        include_points=False,
        include_polygons=True,
    )

    # Polygon handling: clean geometries, compute area, extract relevant tags
    gdf_poly = clean_polygons(gdf_poly)
    gdf_poly = extract_tags(gdf_poly, cfg["extract_tags_fields"])
    gdf_buildings = load_buildings_aligned(
        target_crs=gdf_poly.crs,
        path_out=cfg["path_out"],
        area_name=cfg["area_name"],
        filename_buildings_in_out=cfg["filename_buildings_in_out"],
        export_fileext_largefiles=cfg["export_fileext_largefiles"],
    )

    # Spatial join: INNER / WITHIN corresponds to JOIN_ONE_TO_ONE + KEEP_COMMON + WITHIN
    # If one building falls into multiple polygons, duplicates are created.
    # Keep only the first match per building in the next step.
    joined = gpd.sjoin(
        gdf_buildings,
        gdf_poly,
        how="inner",             # KEEP_COMMON corresponds to inner join
        predicate="within"       # WITHIN match
    )

    print(f"Join result count polygons x buildings: {len(joined)}")

    # Deduplicate: keep only the first polygon per building
    # This assumes the gdf_buildings index is unique.
    joined = joined[~joined.index.duplicated(keep="first")]
    print(f"Count after deduplication: {len(joined)}")

    joined = extract_tags(joined, cfg["extract_tags_fields"], col_with_tags="other_tags_left")

    # Create centroids of merged areas
    gdf_cent = to_centroids(joined, origin="polygon")

    # Extract building level
    # Usually already in "building:levels", but keep the function call for consistency
    gdf_cent['Level_number'] = gdf_cent.apply(get_n_levels, axis=1)

    # Create simpler fields: name, id_left, id_right, building, amenity
    gdf_cent = simplify_fields_after_join(gdf_cent)

    # Export full dataset
    gdf_cent.to_file(os.path.join(cfg["path_out_raw"], f"{category}_raw.{cfg['export_fileext']}"))

    # Create reduced dataset
    gdf_out = reduce_columns(gdf_cent, ["id_left","id_right","name_joint","building_joint","amenity_joint"])

    # Remove "_joint" suffix for the final dataset
    gdf_reduced = remove_joint_suffix(gdf_out)

    # Export reduced dataset
    gdf_reduced.to_file(os.path.join(cfg["path_out"], f"{category}.{cfg['export_fileext']}"))




def floor_area_point_in_polygon(category, cfg):
    """Combine points and polygon-derived floor area points."""
    # Approach:
    # - For point objects: intersect with surrounding building areas
    # - For polygons: use polygons directly
    # - Combine both into one dataset


    # Input file path
    osm_file = os.path.join(cfg["path_in"], f"{category}.osm")
    
    gdf_pts, gdf_poly = load_osm_layers(
        osm_file,
        crs_proj_out=cfg["crs_proj_out"],
        exclude_ids=cfg.get("exclude_ids"),
        include_points=True,
        include_polygons=True,
    )

    # Polygon handling: clean geometries, compute area, extract relevant tags
    gdf_poly = clean_polygons(gdf_poly)
    gdf_poly = extract_tags(gdf_poly, cfg["extract_tags_fields"])


    ## Prepare points: combine points with buildings ##
    # Use only points outside polygons of this category
    gdf_pts = extract_tags(gdf_pts, cfg["extract_tags_fields"])
    gdf_buildings = load_buildings_aligned(
        target_crs=gdf_poly.crs,
        path_out=cfg["path_out"],
        area_name=cfg["area_name"],
        filename_buildings_in_out=cfg["filename_buildings_in_out"],
        export_fileext_largefiles=cfg["export_fileext_largefiles"],
    )
    poly_union = get_union_geometry(gdf_poly)
    gdf_pts_filtered = filter_points_outside_polygons(gdf_pts, poly_union)

    # Spatial join: ArcGIS command equivalent: INNER (KEEP_COMMON) + INTERSECT + ONE_TO_MANY
    # GeoPandas naturally returns one-to-many when several points are in one building,
    # equivalent to ArcGIS JOIN_ONE_TO_MANY.
    gdf_pts_buildings_joined = gpd.sjoin(
        gdf_buildings,
        gdf_pts_filtered,
        how="inner",         # ArcGIS equivalent: KEEP_COMMON, keep intersections only
        predicate="intersects"  # ArcGIS equivalent: INTERSECT
    )
    print(f"Join result count buildings x points: {len(gdf_pts_buildings_joined)}")

    # Clean columns and create full dataset
    gdf_pts_buildings_joined['origin'] = 'point'
    if 'osm_way_id_left' not in gdf_pts_buildings_joined.columns:
        if 'osm_way_id' in gdf_pts_buildings_joined.columns:
            gdf_pts_buildings_joined['osm_way_id_left'] = gdf_pts_buildings_joined['osm_way_id']
        elif 'osm_way_id_right' in gdf_pts_buildings_joined.columns:
            gdf_pts_buildings_joined['osm_way_id_left'] = gdf_pts_buildings_joined['osm_way_id_right']
    gdf_pts_buildings_joined = simplify_fields_after_join(gdf_pts_buildings_joined)
    gdf_pts_buildings_joined.to_file(os.path.join(cfg["path_out_raw"], f"{category}_pts_raw.{cfg['export_fileext']}"))

    # Create reduced dataset - keep only required columns
    # and remove "_joint" suffix for downstream dataset
    gdf_pts_buildings_reduced = reduce_columns(gdf_pts_buildings_joined, ['id_left', 'id_right', 'name_joint', 'building_joint', 'amenity_joint'])
    gdf_pts_final = remove_joint_suffix(gdf_pts_buildings_reduced)

    ## Prepare polygons ##
    gdf_poly['Area'] = gdf_poly.geometry.area
    gdf_poly = extract_tags(gdf_poly, cfg["extract_tags_fields"])
    gdf_poly['Level_number'] = gdf_poly.apply(get_n_levels, axis=1)
    gdf_poly["origin"] = "polygon"

    ## Merge, clean up, and export ##
    # Merge all
    gdf_out = pd.concat([gdf_pts_final, gdf_poly], ignore_index=True)

    # Compute centroids of merged areas
    gdf_out_cent = to_centroids(gdf_out)

    # Export
    export_raw_and_reduced(
        gdf_out_cent,
        category,
        path_out_raw=cfg["path_out_raw"],
        path_out=cfg["path_out"],
        export_fileext=cfg["export_fileext"],
        columns_to_keep_add=['amenity', 'brand', 'shop'],
    )


def area_by_name(category, cfg):
    """Special case for supermarkets with brand-based areas, split by name."""
    # Approach: if polygons exist, use polygons. If points exist, use
    # default area values based on brand names.

    # Define input filename
    osm_file = os.path.join(cfg["path_in"], f"{category}.osm")

    ## Polygon handling ##
    gdf_pts, gdf_poly = load_osm_layers(
        osm_file,
        crs_proj_out=cfg["crs_proj_out"],
        exclude_ids=cfg.get("exclude_ids"),
        include_points=True,
        include_polygons=True,
    )

    # Clean geometries, compute area, create centroids
    gdf_poly = clean_polygons(gdf_poly)
    gdf_poly = extract_tags(gdf_poly, cfg["extract_tags_fields"])

    gdf_poly['Area'] = gdf_poly.geometry.area * 0.7 # reduction factor to approximate sales area, not full building area

    gdf_cent = to_centroids(gdf_poly, origin="polygon")

    # Remove points within existing polygons, then extract relevant tags
    poly_union = get_union_geometry(gdf_poly)
    gdf_pts_out = filter_points_outside_polygons(gdf_pts, poly_union)
    gdf_pts_out = extract_tags(gdf_pts_out, cfg["extract_tags_fields"])
    gdf_pts_out["origin"] = "point"

    # Classify supermarkets
    result_rows = []
    matched_indices = pd.Series(False, index=gdf_pts_out.index)
    gdf_pts_out["osm_name_norm"] = gdf_pts_out["name"].apply(normalize_brand_text)

    pattern_specs = build_supermarket_pattern_specs(cfg["supermarket_brands"])
    for brand, pattern_norm in pattern_specs:
        mask = (~matched_indices) & gdf_pts_out["osm_name_norm"].str.contains(pattern_norm, regex=False, na=False)
        if not mask.any():
            continue

        matched_indices |= mask
        temp = gdf_pts_out[mask].copy()
        temp["Marke"] = brand
        temp["Area_brand_based"] = cfg["supermarket_area_values"][brand]
        result_rows.append(temp)
    
    # Unassigned supermarkets
    others = gdf_pts_out[~matched_indices].copy()
    others["Marke"] = "weitere_sup"
    others["Area_brand_based"] = cfg["default_supermarket_size_m2"]
    result_rows.append(others)

    final_gdf = pd.concat(result_rows)
    final_gdf["Area"] = final_gdf["Area_brand_based"]


    ## Merge and export ##
    # Merge: points + polygon centroids
    gdf = pd.concat([final_gdf, gdf_cent], ignore_index=True)

    # Extract "wholesale" attribute
    gdf = extract_tags(gdf, cfg["extract_tags_fields"])
    gdf = extract_tags(gdf, ['wholesale'])

    # Helper column for easier comparison
    gdf["osm_name_lower"] = gdf["name"].str.lower()


    # Split into types
    # 1. Hypermarket
    hypermarket_mask = (
        (gdf["Area"] > 9000) |
        (gdf["osm_name_lower"].str.contains(cfg["name_based_pattern_hypermarket"], na=False)) |
        (gdf["wholesale"].notna())
    )
    gdf.loc[hypermarket_mask, "type"] = "hypermarket"
    print(f"Count hypermarket: {hypermarket_mask.sum()}")
    
    # 2. Supermarket
    supermarket_mask_general = (
        ((gdf["Area"] < 9000) | gdf["Area"].isna()) &
        ~gdf["osm_name_lower"].str.contains(cfg["name_based_pattern_hypermarket"], na=False)
    )
    gdf.loc[supermarket_mask_general, "type"] = "supermarket"
    print(f"Count supermarket: {supermarket_mask_general.sum()}")

    # 2a Aldi/Lidl
    # Aldi/Lidl defined as: type = supermarket AND name contains Aldi or Lidl
    aldi_lidl_mask = supermarket_mask_general & gdf["osm_name_lower"].str.contains("aldi|lidl", na=False)
    gdf.loc[aldi_lidl_mask, "type"] = "aldi_lidl"
    print(f"Count Aldi/Lidl: {aldi_lidl_mask.sum()}")

    # 2b Other supermarkets: type = supermarket, but not Aldi/Lidl
    other_supermarket_mask = supermarket_mask_general & ~aldi_lidl_mask
    print(f"Count other supermarkets: {other_supermarket_mask.sum()}")

    # Export full dataset
    gdf.to_file(os.path.join(cfg["path_out_raw"], f"{category}_raw.{cfg['export_fileext']}"))

    # Export as three separate datasets (hypermarket, supermarket, Aldi/Lidl)
    export_raw_and_reduced(
        gdf[hypermarket_mask],
        f"{category}_Hypermarket",
        path_out_raw=cfg["path_out_raw"],
        path_out=cfg["path_out"],
        export_fileext=cfg["export_fileext"],
        columns_to_keep_add=['type', 'brand', 'wholesale', 'shop'],
    )
    export_raw_and_reduced(
        gdf[other_supermarket_mask],
        f"{category}_Supermarket",
        path_out_raw=cfg["path_out_raw"],
        path_out=cfg["path_out"],
        export_fileext=cfg["export_fileext"],
        columns_to_keep_add=['type', 'brand', 'wholesale', 'shop'],
    )
    export_raw_and_reduced(
        gdf[aldi_lidl_mask],
        f"{category}_AldiLidl",
        path_out_raw=cfg["path_out_raw"],
        path_out=cfg["path_out"],
        export_fileext=cfg["export_fileext"],
        columns_to_keep_add=['type', 'brand', 'wholesale', 'shop'],
    )

