import os
import ast
import pandas as pd
import geopandas as gpd
from shapely.ops import unary_union
from shapely.validation import make_valid
import re
import time
from datetime import datetime



# -----------------------------
# Parser for building levels
# -----------------------------
def parse_other_tags(ot):
    """
    Converts other_tags (dict or string) into a Python dict.
    Supports HSTORE strings such as '"building:levels"=>"3","levels"=>"5"' or Python literals.
    """
    if ot is None or pd.isna(ot):
        return {}
    if isinstance(ot, dict):
        return ot
    if not isinstance(ot, str):
        return {}
    # Attempt: Python literal
    try:
        parsed = ast.literal_eval(ot)
        if isinstance(parsed, dict):
            return parsed
    except Exception:
        pass
    # Fallback: regex for HSTORE format
    return dict(re.findall(r'"([^\"]+)"=>"(.*?)"', ot))

def get_n_levels(row):
    """
    Computes Level_number using the following logic:
    1. If 'levels' has a value: count entries separated by ';' or ','.
    2. Otherwise: if 'building:levels' exists and 'building' != 'residential': parse first element.
    3. Else: None
    """
    lv = row.get('level')
    if lv:
        try:
            # Split by semicolon or comma and count non-empty parts
            parts = re.split(r'[;,]', str(lv))
            return float(len([p for p in parts if p]))
        except Exception:
            return None
    bl = row.get('building:levels')
    bldg = row.get('building')
    if bl and (bldg is None or str(bldg).lower() != 'residential'):
        try:
            # First entry, split by ; or ,
            first = re.split(r'[;,]', str(bl))[0]
            return float(first)
        except Exception:
            return None
    return None


def coalesce_cols(df: pd.DataFrame, cols: list[str]) -> pd.Series:
    """
    For each row, takes the first value from cols that is not NA/None and not "".
    Missing columns are ignored.
    """
    out = pd.Series([pd.NA] * len(df), index=df.index, dtype="object")
    for c in cols:
        if c not in df.columns:
            continue
        v = df[c]
        if v.dtype == "object":
            v = v.replace("", pd.NA)
        out = out.fillna(v)
    return out


def concat_fieldvalues(df, col_right="name_right", col_left="name_left", sep = " | "):
    """
    Concatenates col_right and col_left with sep (e.g. ' | ')
    if both are available.
    """
    def _combine(row):
        parts = []
        for col in [col_right, col_left]:
            if col in df.columns:
                val = row[col]
                if pd.notna(val) and str(val).strip() != "":
                    parts.append(str(val).strip())
        return sep.join(parts) if parts else pd.NA

    return df.apply(_combine, axis=1)


def simplify_fields_after_join(gdf):
    """After join: create best-effort unified fields for name, id, building, and amenity from both datasets."""
    gdf["name_joint"] = concat_fieldvalues(gdf, "name_right", "name_left")
    gdf["id_left"] = coalesce_cols(gdf, ["osm_id_left", "osm_way_id_left"])
    gdf["id_right"] = coalesce_cols(gdf, ["osm_id_right", "osm_way_id_right"])
    gdf["id_joint"] = concat_fieldvalues(gdf, "id_right", "id_left", sep="_")
    gdf["building_joint"] = coalesce_cols(gdf, ["building_left", "building_right"])
    gdf["amenity_joint"] = coalesce_cols(gdf, ["amenity_left", "amenity_right"])
    return gdf

def extract_tags(gdf, tags_to_extract, col_with_tags="other_tags"):
    """Extracts selected tags from column 'other_tags' into dedicated columns."""
    # It is still to be verified whether this function is meaningful for polygons.
    # Polygons seem to already contain most information at top level.

    gdf = gdf.copy()
    if col_with_tags in gdf.columns:
        gdf['tags_dict'] = gdf[col_with_tags].apply(parse_other_tags)
    else:
        gdf['tags_dict'] = [{} for _ in range(len(gdf))]

    for tag in tags_to_extract:
        if tag not in gdf.columns:
            gdf[tag] = gdf['tags_dict'].apply(lambda d: d.get(tag))
    # Drop helper column
    gdf = gdf.drop(columns=['tags_dict'])
    return gdf

def reduce_columns(gdf, columns_to_keep_add=None):
    """Helper function to keep selected columns and drop the rest."""
    # Columns that should always be kept
    columns_to_keep_default = [
        "id",
        "name",
        "origin",
        "Area",
        "Level_number",
        "geometry",
    ]

    if columns_to_keep_add is None:
        columns_to_keep_add = []

    columns_to_keep = columns_to_keep_default + columns_to_keep_add

    # Remove fields if they are null/NA in the whole dataset.
    # Keep geometry whenever present so the result can stay a GeoDataFrame,
    # even for empty categories.
    columns_to_keep = [
        c for c in columns_to_keep
        if c in gdf.columns and (c == "geometry" or gdf[c].notna().any())
    ]

    out = gdf[columns_to_keep].copy()
    if "geometry" in out.columns:
        crs = getattr(gdf, "crs", None)
        return gpd.GeoDataFrame(out, geometry="geometry", crs=crs)
    return out


def normalize_osm_id(value):
    """Normalizes OSM ID values to comparable strings (e.g. 123.0 -> '123')."""
    if pd.isna(value):
        return None

    value_txt = str(value).strip()
    if value_txt == "" or value_txt.lower() == "nan":
        return None

    try:
        value_num = float(value_txt)
        if value_num.is_integer():
            return str(int(value_num))
    except Exception:
        pass

    return value_txt


def filter_excluded_osm_ids(gdf, exclude_ids=None, context=""):
    """Removes rows listed in exclude_ids via osm_id/osm_way_id and logs matches."""
    if exclude_ids is None:
        exclude_ids = set()

    if gdf is None or gdf.empty or not exclude_ids:
        return gdf

    id_cols = [col for col in ["osm_id", "osm_way_id"] if col in gdf.columns]
    if not id_cols:
        return gdf

    exclude_ids_norm = {normalize_osm_id(v) for v in exclude_ids}
    exclude_ids_norm = {v for v in exclude_ids_norm if v is not None}
    if not exclude_ids_norm:
        return gdf

    hit_mask = pd.Series(False, index=gdf.index)
    hit_ids = set()

    for col in id_cols:
        norm_vals = gdf[col].map(normalize_osm_id)
        col_hits = norm_vals.isin(exclude_ids_norm)
        if col_hits.any():
            hit_ids.update(norm_vals[col_hits].dropna().tolist())
        hit_mask = hit_mask | col_hits

    n_hits = int(hit_mask.sum())
    if n_hits > 0:
        context_txt = f" ({context})" if context else ""
        print(
            f"exclude_ids{context_txt}: {n_hits} entries removed. "
            f"Matched IDs: {', '.join(sorted(hit_ids))}"
        )

    return gdf.loc[~hit_mask].copy()


def load_osm_layer(osm_file, layer, crs_proj_out, exclude_ids=None, on_invalid=None):
    """Loads one OSM layer, sets CRS, and projects to target CRS."""
    kwargs = {"layer": layer}
    if on_invalid is not None:
        kwargs["on_invalid"] = on_invalid
    gdf = gpd.read_file(osm_file, **kwargs).set_crs('EPSG:4326', allow_override=True).to_crs(crs_proj_out)
    context = f"{os.path.basename(osm_file)}:{layer}"
    gdf = filter_excluded_osm_ids(gdf, exclude_ids=exclude_ids, context=context)
    return gdf


def load_osm_layers(
    osm_file,
    crs_proj_out,
    exclude_ids=None,
    include_points=True,
    include_polygons=True,
    allow_missing_polygons=False,
):
    """Loads points and/or polygons from an OSM file."""
    gdf_pts = None
    gdf_poly = None

    if include_points:
        gdf_pts = load_osm_layer(osm_file, layer='points', crs_proj_out=crs_proj_out, exclude_ids=exclude_ids)
        print("Points loaded, count: ", len(gdf_pts))

    if include_polygons:
        try:
            gdf_poly = load_osm_layer(
                osm_file,
                layer='multipolygons',
                crs_proj_out=crs_proj_out,
                exclude_ids=exclude_ids,
                on_invalid="fix",
            )
            print("Polygons loaded, count: ", len(gdf_poly))
        except Exception:
            if not allow_missing_polygons:
                raise
            base_columns = gdf_pts.columns if gdf_pts is not None else []
            gdf_poly = gpd.GeoDataFrame(columns=base_columns, crs=crs_proj_out)
            print("No polygons found, continuing with points only.")

    return gdf_pts, gdf_poly


def clean_polygons(gdf_poly):
    """Cleans polygon geometries (remove nulls, apply make_valid)."""
    if gdf_poly is None or gdf_poly.empty:
        return gdf_poly
    gdf_poly = gdf_poly[gdf_poly["geometry"].notnull()].copy()
    gdf_poly["geometry"] = gdf_poly["geometry"].apply(make_valid)
    return gdf_poly


def filter_supporting_elements(gdf):
    """Filters OSM supporting elements if the field exists."""
    if gdf is None:
        return gdf
    if 'osm_supporting_element' in gdf.columns:
        return gdf[gdf['osm_supporting_element'] == 'no'].copy()
    return gdf


def to_centroids(gdf, origin=None):
    """Converts geometries to centroids; optionally sets origin."""
    out = gdf.copy()
    out.geometry = out.centroid
    if origin is not None:
        out["origin"] = origin
    return out


def get_union_geometry(gdf_poly):
    """Returns the union geometry of polygons."""
    if gdf_poly is None or gdf_poly.empty:
        return None
    return unary_union(gdf_poly.geometry.values)


def filter_points_outside_polygons(gdf_pts, poly_union):
    """Keeps only points that are not covered by polygons."""
    if gdf_pts is None:
        return gdf_pts
    if poly_union is None:
        return gdf_pts.copy()
    return gdf_pts[~gdf_pts.geometry.apply(lambda pt: poly_union.covers(pt))].copy()


def normalize_brand_text(value):
    """Normalizes names/patterns for robust matching (e.g. Rewe-Center == Rewe Center == ReweCenter)."""
    if pd.isna(value):
        return ""
    txt = str(value).lower().strip()
    return re.sub(r"[^a-z0-9]+", "", txt)


def build_supermarket_pattern_specs(brands_dict):
    """Builds pattern list with priority: longer (more specific) patterns first."""
    pattern_specs = []
    for brand, patterns in brands_dict.items():
        for pattern in patterns:
            p_norm = normalize_brand_text(pattern)
            if p_norm:
                pattern_specs.append((brand, p_norm))

    pattern_specs.sort(key=lambda x: len(x[1]), reverse=True)
    return pattern_specs


def export_raw_and_reduced(gdf_out, category, path_out_raw, path_out, export_fileext, columns_to_keep_add=None):
    """Exports raw and reduced datasets consistently."""
    if columns_to_keep_add is None:
        columns_to_keep_add = []

    gdf_out = gdf_out.copy()
    if "geometry" in gdf_out.columns and not isinstance(gdf_out, gpd.GeoDataFrame):
        gdf_out = gpd.GeoDataFrame(gdf_out, geometry="geometry")
    gdf_out["id"] = coalesce_cols(gdf_out, ["osm_id", "osm_way_id"])
    gdf_out.to_file(os.path.join(path_out_raw, f"{category}_raw.{export_fileext}"))

    gdf_out_reduced = reduce_columns(gdf_out, columns_to_keep_add)
    gdf_out_reduced.to_file(os.path.join(path_out, f"{category}.{export_fileext}"))


def load_buildings_aligned(target_crs, path_out, area_name, filename_buildings_in_out, export_fileext_largefiles):
    """Loads buildings file and aligns CRS to target_crs."""
    path_buildings = os.path.join(path_out, f"{area_name}_{filename_buildings_in_out}.{export_fileext_largefiles}")
    gdf_buildings = gpd.read_file(path_buildings)
    print("Buildings loaded, count: ", len(gdf_buildings))
    if gdf_buildings.crs != target_crs:
        gdf_buildings = gdf_buildings.to_crs(target_crs)
    return gdf_buildings


def remove_joint_suffix(gdf):
    """Removes _joint suffix from column names."""
    out = gdf.copy()
    out.columns = [c.replace("_joint", "") if c.endswith("_joint") else c for c in out.columns]
    return out


def run_category(category_name, func, area_name, *args, **kwargs):
    """Uniform wrapper for timing and logging per category."""
    start_time = time.time()
    print(f"Processing category: {category_name} - {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
    func(f"{area_name}_{category_name}", *args, **kwargs)
    elapsed = time.time() - start_time
    print(f"Done - Runtime: {elapsed:.2f}s\n")


def run_categories(categories, func, area_name, *args, **kwargs):
    """Runs multiple categories with consistent logging."""
    for category_name in categories:
        run_category(category_name, func, area_name, *args, **kwargs)

