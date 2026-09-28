# Evaluate generated POI files quantitatively
#
# Evaluate number of POIs per category, number and share of POIs with floor area and levels (where needed)
#
# The list of POIs does not yet reference the attractiveness_factors_*.json file, instead the categories and the required
# indicators are set directly in this file. (TODO: Switch to reference to attractiveness_factors_*.json file.)

# ----------------------------
# Settings
# ----------------------------
# Folder with GeoJSON outputs
out_dir <- r"(data\pois\Export_Points_NAME)"

files <- list.files(out_dir, pattern = "\\.geojson$", full.names = TRUE)

prefix <- "ALIKE_"

# Mapping: relevant variable by category
# Allowed values: "Count", "Area", "FloorArea"
relevant_by_category <- list(
  Pharmacy = "Count",
  PostOffice = "Count",
  
  DailyShopping_Supermarket_AldiLidl = "Area",
  DailyShopping_Supermarket_Supermarket = "Area",
  DailyShopping_Supermarket_Hypermarket = "Area",
  DailyShopping_Drugstore = "Area",
  DailyShopping_BakeryButcherKiosk = "Count",
  DailyShopping_Other = "Area",
  
  LongTermShopping_DIYGardenCenter = "Area",
  LongTermShopping_FurnitureStore = "FloorArea",
  LongTermShopping_DepartmentClothingElectronics = "FloorArea",
  LongTermShopping_Other = "FloorArea",
  
  Doctor = "Count",
  Bank = "Count",
  Authority = "Count",
  Hairdresser = "Count",
  Church = "Area",
  
  Hospital = "FloorArea",
  
  Library = "FloorArea",
  FitnessCenter = "Area",
  Cinema = "FloorArea",
  Museums = "FloorArea",
  MuseumsOutdoor = "Area",
  Theater = "Area",
  Restaurant = "Count",
  Zoo = "Area",
  
  SwimmingPool = "Area",
  SwimmingPoolOutdoor = "Area",
  
  Playground = "Area",
  Beach = "Area",
  Park = "Area",
  Cemetery = "Area",
  AllotmentGardens = "Area",
  
  SportsHall = "Area",
  SmallSportsField = "Area",
  SportsField = "Area",
  
  Kindergarten = "Count",
  RegionalRail = "Count",
  
  # Existing category from the previous script; not included in the provided mapping
  Hotel = "Count"
)

# ----------------------------
# Helper
# ----------------------------
category_from_filename <- function(path) {
  nm <- basename(path)
  nm <- sub("\\.geojson$", "", nm, ignore.case = TRUE)
  nm <- sub(paste0("^", prefix), "", nm, ignore.case = TRUE)
  nm
}

to_numeric_safe <- function(x) {
  if (inherits(x, "units")) x <- as.numeric(x)
  if (is.factor(x)) x <- as.character(x)
  if (is.character(x)) {
    x <- gsub("\\s+", "", x)
    # Remove German thousands separator "."; if you have true decimal points, comment this line out
    x <- gsub("\\.", "", x)
    x <- gsub(",", ".", x)
  }
  suppressWarnings(as.numeric(x))
}

share_non_na_numeric <- function(dt, field) {
  if (!(field %in% names(dt))) return(NA_real_)
  v <- dt[[field]]
  v_num <- to_numeric_safe(v)
  if (length(v_num) == 0) return(NA_real_)
  mean(!is.na(v_num))
}

numeric_stat <- function(dt, field, fun, ...) {
  if (!(field %in% names(dt))) return(NA_real_)
  v <- to_numeric_safe(dt[[field]])
  if (all(is.na(v))) return(NA_real_)
  fun(v, na.rm = TRUE, ...)
}

count_origin <- function(dt) {
  # origin: "point" vs "polygon"
  if (!("origin" %in% names(dt))) {
    return(list(n_point = 0L, n_polygon = 0L))
  }
  
  o <- dt[["origin"]]
  if (is.factor(o)) o <- as.character(o)
  o <- tolower(trimws(o))
  
  list(
    n_point = sum(o == "point", na.rm = TRUE),
    n_polygon = sum(o == "polygon", na.rm = TRUE)
  )
}


# ----------------------------
# Read each file and calculate file statistics
# ----------------------------
if (length(files) == 0) stop("No GeoJSON files found in: ", out_dir)

stats_by_file <- rbindlist(lapply(files, function(f) {
  cat("Reading:", f, "\n")
  cat_name <- category_from_filename(f)
  
  # Retrieve relevant variable beforehand
  relevant_variable <- unname(relevant_by_category[[cat_name]])
  if (is.null(relevant_variable)) relevant_variable <- NA_character_
  
  sf_obj <- tryCatch(
    st_read(f, quiet = TRUE, stringsAsFactors = FALSE),
    error = function(e) NULL
  )
  
  if (is.null(sf_obj)) {
    return(data.table(
      category = cat_name,
      file = basename(f),
      ok = FALSE,
      relevant_variable = relevant_variable,
      category_in_mapping = !is.na(relevant_variable),
      
      n_features = NA_integer_,
      n_point = NA_integer_,
      n_polygon = NA_integer_,
      
      share_Area = NA_real_,
      share_Level_number = NA_real_,
      share_FloorArea = NA_real_,
      
      Area_mean = NA_real_,
      Area_10p = NA_real_,
      Area_15p = NA_real_,
      Area_20p = NA_real_,
      Area_25p = NA_real_,
      Area_33p = NA_real_,
      Area_50p = NA_real_,
      Area_75p = NA_real_,
      
      Level_number_mean = NA_real_,
      Level_number_25p = NA_real_,
      Level_number_50p = NA_real_,
      
      adj_level_number_mean = NA_real_,
      adj_level_number_25p = NA_real_,
      adj_level_number_50p = NA_real_,
      
      FloorArea_mean = NA_real_,
      FloorArea_10p = NA_real_,
      FloorArea_15p = NA_real_,
      FloorArea_20p = NA_real_,
      FloorArea_25p = NA_real_,
      FloorArea_33p = NA_real_,
      FloorArea_50p = NA_real_,
      FloorArea_75p = NA_real_
    ))
  }
  
  n_feat <- nrow(sf_obj)
  dt_attr <- as.data.table(st_drop_geometry(sf_obj))
  
  # Calculate adjusted level number and FloorArea.
  # FloorArea is calculated from Area * level_number_adj.
  # If Level_number is missing or NA, assume one level.
  if ("Area" %in% names(dt_attr)) {
    area_num <- to_numeric_safe(dt_attr[["Area"]])
    
    if ("Level_number" %in% names(dt_attr)) {
      level_num <- to_numeric_safe(dt_attr[["Level_number"]])
      level_number_adj <- fifelse(!is.na(level_num), level_num, 1)
    } else {
      level_number_adj <- rep(1, nrow(dt_attr))
    }
    
    dt_attr[, level_number_adj := level_number_adj]
    dt_attr[, FloorArea := area_num * level_number_adj]
  }
  
  oc <- count_origin(dt_attr)
  
  use_floorarea <- identical(relevant_variable, "FloorArea")
  
  area_quantiles <- if ("Area" %in% names(dt_attr)) {
    quantile(
      to_numeric_safe(dt_attr[["Area"]]),
      probs = c(0.10, 0.15, 0.20, 0.25, 0.33, 0.50, 0.75),
      na.rm = TRUE,
      names = FALSE
    )
  } else {
    rep(NA_real_, 7)
  }
  
  floorarea_quantiles <- if (use_floorarea && "FloorArea" %in% names(dt_attr)) {
    quantile(
      to_numeric_safe(dt_attr[["FloorArea"]]),
      probs = c(0.10, 0.15, 0.20, 0.25, 0.33, 0.50, 0.75),
      na.rm = TRUE,
      names = FALSE
    )
  } else {
    rep(NA_real_, 7)
  }
  
  as.data.table(list(
    category = cat_name,
    file = basename(f),
    ok = TRUE,
    relevant_variable = relevant_variable,
    category_in_mapping = !is.na(relevant_variable),
    
    n_features = n_feat,
    n_point = as.integer(oc$n_point),
    n_polygon = as.integer(oc$n_polygon),
    
    share_point = if (n_feat > 0) oc$n_point / n_feat else NA_real_,
    share_polygon = if (n_feat > 0) oc$n_polygon / n_feat else NA_real_,
    
    share_Area = share_non_na_numeric(dt_attr, "Area"),
    share_Level_number = share_non_na_numeric(dt_attr, "Level_number"),
    
    share_FloorArea = if (use_floorarea) {
      share_non_na_numeric(dt_attr, "FloorArea")
    } else {
      NA_real_
    },
    
    Area_mean = numeric_stat(dt_attr, "Area", mean),
    Area_10p = area_quantiles[1],
    Area_15p = area_quantiles[2],
    Area_20p = area_quantiles[3],
    Area_25p = area_quantiles[4],
    Area_33p = area_quantiles[5],
    Area_50p = area_quantiles[6],
    Area_75p = area_quantiles[7],
    
    Level_number_mean = numeric_stat(dt_attr, "Level_number", mean),
    Level_number_25p = numeric_stat(dt_attr, "Level_number", quantile, probs = 0.25),
    Level_number_50p = numeric_stat(dt_attr, "Level_number", quantile, probs = 0.50),
    
    adj_level_number_mean = numeric_stat(dt_attr, "level_number_adj", mean),
    adj_level_number_25p = numeric_stat(dt_attr, "level_number_adj", quantile, probs = 0.25),
    adj_level_number_50p = numeric_stat(dt_attr, "level_number_adj", quantile, probs = 0.50),
    
    FloorArea_mean = if (use_floorarea) {
      numeric_stat(dt_attr, "FloorArea", mean)
    } else {
      NA_real_
    },
    FloorArea_10p = floorarea_quantiles[1],
    FloorArea_15p = floorarea_quantiles[2],
    FloorArea_20p = floorarea_quantiles[3],
    FloorArea_25p = floorarea_quantiles[4],
    FloorArea_33p = floorarea_quantiles[5],
    FloorArea_50p = floorarea_quantiles[6],
    FloorArea_75p = floorarea_quantiles[7]
  ))
  
}), fill = TRUE)


# ----------------------------
# Finalize summary
# ----------------------------

qc_cat <- stats_by_file

# Replace Inf with NA if all values were missing
qc_cat[is.infinite(share_Area), share_Area := NA_real_]
qc_cat[is.infinite(share_Level_number), share_Level_number := NA_real_]
qc_cat[is.infinite(share_FloorArea), share_FloorArea := NA_real_]

# Sort
setorder(qc_cat, category)

# Output
out_csv <- file.path(out_dir, "QC_categories_summary.csv")
fwrite(qc_cat, out_csv, sep = ";")

cat("\nSummary written:\n", out_csv, "\n", sep = "")