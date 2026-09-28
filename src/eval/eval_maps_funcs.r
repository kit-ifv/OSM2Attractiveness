`%||%` <- function(x, y) if (is.null(x)) y else x

msg <- function(...) {
  message(sprintf("[%s] %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), paste0(..., collapse = "")))
}

as_char_vec <- function(x) {
  if (is.null(x)) return(character())
  as.character(unlist(x, use.names = FALSE))
}

as_named_numeric <- function(x) {
  if (is.null(x)) return(setNames(numeric(), character()))
  x_vec <- unlist(x, use.names = TRUE)
  out <- suppressWarnings(as.numeric(x_vec))
  names(out) <- names(x_vec)
  out[is.finite(out)]
}

find_latest_run_dir <- function(path_output_base) {
  if (!dir.exists(path_output_base)) return(NULL)
  sub_dirs <- list.dirs(path_output_base, recursive = FALSE, full.names = TRUE)
  run_dirs <- sub_dirs[grepl("^run_", basename(sub_dirs))]
  if (length(run_dirs) == 0L) return(NULL)
  run_dirs[order(basename(run_dirs), decreasing = TRUE)][1L]
}

build_purpose_details_html <- function(data_sf, purpose) {
  detail_cols <- grep(paste0("_", purpose, "$"), names(data_sf), value = TRUE)
  if (length(detail_cols) == 0L) {
    return(rep("<em>No detailed values for this purpose available.</em>", nrow(data_sf)))
  }

  vals_mat <- as.data.frame(sf::st_drop_geometry(data_sf[, detail_cols, drop = FALSE]))
  labels <- sub(paste0("_", purpose, "$"), "", detail_cols)

  vapply(seq_len(nrow(vals_mat)), function(i) {
    row_vals <- as.numeric(vals_mat[i, ])
    row_vals[is.na(row_vals)] <- 0

    line_items <- paste0(
      "<li><strong>", labels, ":</strong> ",
      format(round(row_vals, 2), nsmall = 2),
      "</li>"
    )

    paste0(
      "<strong>Details (Category):</strong><br>",
      "<ul style='margin:4px 0 0 16px; padding:0;'>",
      paste(line_items, collapse = ""),
      "</ul>"
    )
  }, character(1))
}

build_regular_grid_attractiveness <- function(poi_subset, purposes, cellsize_m = 1000) {
  if (is.null(poi_subset) || nrow(poi_subset) == 0L) return(NULL)

  required_cols <- c("purpose", "attractiveness")
  missing_cols <- setdiff(required_cols, names(poi_subset))
  if (length(missing_cols) > 0L) {
    stop(
      paste0("Missing required POI columns for grid aggregation: ", paste(missing_cols, collapse = ", ")),
      call. = FALSE
    )
  }

  poi_work <- poi_subset[, c("purpose", "attractiveness")]
  poi_work <- poi_work[!sf::st_is_empty(poi_work), ]
  if (nrow(poi_work) == 0L) return(NULL)

  grid <- sf::st_make_grid(poi_work, cellsize = cellsize_m, square = TRUE, what = "polygons")
  if (length(grid) == 0L) return(NULL)

  grid_sf <- sf::st_sf(grid_id = as.character(seq_along(grid)), geometry = grid)
  poi_grid <- suppressWarnings(sf::st_join(poi_work, grid_sf, join = sf::st_within, left = FALSE))
  if (nrow(poi_grid) == 0L) return(NULL)

  poi_grid_dt <- as.data.table(sf::st_drop_geometry(poi_grid))
  poi_grid_dt <- poi_grid_dt[!is.na(purpose) & purpose %in% purposes & !is.na(attractiveness)]
  if (nrow(poi_grid_dt) == 0L) return(NULL)

  agg_long <- poi_grid_dt[, list(attractiveness = sum(attractiveness, na.rm = TRUE)), by = c("grid_id", "purpose")]
  agg_wide <- dcast(agg_long, grid_id ~ purpose, value.var = "attractiveness", fill = NA_real_)
  grid_sf <- merge(grid_sf, agg_wide, by = "grid_id", all.x = TRUE)

  for (purpose in purposes) {
    if (!(purpose %in% names(grid_sf))) {
      grid_sf[[purpose]] <- NA_real_
    }
  }

  row_has_data <- rowSums(!is.na(as.data.frame(sf::st_drop_geometry(grid_sf[, purposes, drop = FALSE])))) > 0
  for (purpose in purposes) {
    grid_sf[[purpose]][is.na(grid_sf[[purpose]])] <- 0
  }

  grid_sf[row_has_data, ]
}

mapgl_style_url <- "https://tiles.openfreemap.org/styles/liberty"

safe_mapgl_id <- function(...) {
  parts <- as.character(c(...))
  raw_id <- paste(parts[nzchar(parts)], collapse = "_")
  clean_id <- gsub("[^A-Za-z0-9_-]+", "_", raw_id)
  clean_id <- gsub("_+", "_", clean_id)
  clean_id <- gsub("^_|_$", "", clean_id)
  if (!nzchar(clean_id)) "layer" else clean_id
}

make_mapgl_color_stops <- function(values, n = 6L) {
  finite_values <- values[is.finite(values)]
  colors <- grDevices::hcl.colors(n, "Viridis")

  if (length(finite_values) == 0L) {
    return(list(values = seq(0, 1, length.out = n), colors = colors))
  }

  min_value <- min(finite_values, na.rm = TRUE)
  max_value <- max(finite_values, na.rm = TRUE)

  if (isTRUE(all.equal(min_value, max_value))) {
    delta <- if (min_value == 0) 1 else abs(min_value) * 0.05
    min_value <- min_value - delta
    max_value <- max_value + delta
  }

  list(values = seq(min_value, max_value, length.out = n), colors = colors)
}

make_html_map <- function(zones_sf_mapgl_subset, subset_name, purposes, output_dir, timestamp_string, grid_sf_mapgl_subset = NULL) {
  path_output_html <- file.path(output_dir, paste0("interactive_zones_", subset_name, ".html"))
  has_grid <- !is.null(grid_sf_mapgl_subset) && nrow(grid_sf_mapgl_subset) > 0
  layer_control <- list()
  has_legend <- FALSE

  map <- maplibre(
    style = mapgl_style_url,
    bounds = zones_sf_mapgl_subset,
    projection = "mercator",
    height = 700
  ) |>
    add_navigation_control(position = "top-right") |>
    add_scale_control(position = "bottom-left", unit = "metric")

  for (purpose in purposes) {
    palette_values <- zones_sf_mapgl_subset[[purpose]]
    if (has_grid && purpose %in% names(grid_sf_mapgl_subset)) {
      palette_values <- c(palette_values, grid_sf_mapgl_subset[[purpose]])
    }
    stops <- make_mapgl_color_stops(palette_values)
    fill_color <- interpolate(
      column = purpose,
      values = stops$values,
      stops = stops$colors,
      na_color = "#808080"
    )

    detail_html <- build_purpose_details_html(zones_sf_mapgl_subset, purpose)
    zone_popup_col <- safe_mapgl_id("popup", purpose)
    zone_tooltip_col <- safe_mapgl_id("tooltip", purpose)
    zone_layer_id <- safe_mapgl_id("zones", subset_name, purpose)
    zone_group <- paste0(purpose, " (zones)")
    zones_layer <- zones_sf_mapgl_subset
    zones_layer[[zone_popup_col]] <- paste0(
      "<strong>Zone ID:</strong> ", zones_layer$NO, "<br>",
      "<strong>Type:</strong> ", zones_layer$typ, "<br>",
      "<strong>Name:</strong> ", zones_layer$NAME, "<br>",
      "<strong>", purpose, ":</strong> ", format(round(zones_layer[[purpose]], 2), nsmall = 2), "<br><br>",
      detail_html
    )
    zones_layer[[zone_tooltip_col]] <- paste0(
      "Zone: ", zones_layer$NO, "<br>",
      purpose, ": ", format(round(zones_layer[[purpose]], 2), nsmall = 2)
    )

    map <- map |>
      add_fill_layer(
        id = zone_layer_id,
        source = zones_layer,
        fill_color = fill_color,
        fill_opacity = 0.7,
        fill_outline_color = "#ffffff",
        visibility = if (purpose == purposes[1]) "visible" else "none",
        popup = zone_popup_col,
        tooltip = zone_tooltip_col,
        hover_options = list(fill_outline_color = "#ff3333", fill_opacity = 0.85)
      ) |>
      add_continuous_legend(
        legend_title = zone_group,
        values = stops$values,
        colors = stops$colors,
        position = "bottom-right",
        layer_id = zone_layer_id,
        unique_id = paste0(zone_layer_id, "_legend"),
        add = has_legend
      )
    has_legend <- TRUE
    layer_control[[zone_group]] <- zone_layer_id

    if (has_grid) {
      grid_popup_col <- safe_mapgl_id("popup_grid", purpose)
      grid_tooltip_col <- safe_mapgl_id("tooltip_grid", purpose)
      grid_layer_id <- safe_mapgl_id("grid", subset_name, purpose)
      grid_group <- paste0(purpose, " (grid)")
      grid_layer <- grid_sf_mapgl_subset
      grid_layer[[grid_popup_col]] <- paste0(
        "<strong>Grid cell:</strong> ", grid_layer$grid_id, "<br>",
        "<strong>", purpose, ":</strong> ", format(round(grid_layer[[purpose]], 2), nsmall = 2)
      )
      grid_layer[[grid_tooltip_col]] <- paste0(
        "Grid cell: ", grid_layer$grid_id, "<br>",
        purpose, ": ", format(round(grid_layer[[purpose]], 2), nsmall = 2)
      )

      map <- map |>
        add_fill_layer(
          id = grid_layer_id,
          source = grid_layer,
          fill_color = fill_color,
          fill_opacity = 0.7,
          fill_outline_color = "#666666",
          visibility = "none",
          popup = grid_popup_col,
          tooltip = grid_tooltip_col,
          hover_options = list(fill_outline_color = "#ff3333", fill_opacity = 0.85)
        ) |>
        add_continuous_legend(
          legend_title = grid_group,
          values = stops$values,
          colors = stops$colors,
          position = "bottom-right",
          layer_id = grid_layer_id,
          unique_id = paste0(grid_layer_id, "_legend"),
          add = TRUE
        )
      layer_control[[grid_group]] <- grid_layer_id
    }
  }

  map <- map |>
    add_layers_control(
      position = "top-left",
      layers = layer_control,
      collapsible = FALSE,
      background_color = "#ffffff",
      active_color = "#3578c6"
    )

  htmlwidgets::saveWidget(map, file = path_output_html, selfcontained = TRUE)
  msg("Interactive map written: ", path_output_html)
}
make_overview_image_map <- function(zones_subset, zone_type, purposes, output_dir) {
  zones_long <- as.data.table(sf::st_drop_geometry(zones_subset))
  zones_long <- melt(
    zones_long,
    id.vars = setdiff(names(zones_long), purposes),
    measure.vars = purposes,
    variable.name = "purpose",
    value.name = "attractiveness"
  )

  zones_long <- sf::st_as_sf(
    zones_long,
    geometry = sf::st_geometry(zones_subset)[match(zones_long$NO, zones_subset$NO)]
  )

  min_val <- min(zones_long$attractiveness, na.rm = TRUE)
  max_val <- max(zones_long$attractiveness, na.rm = TRUE)

  p <- ggplot(zones_long) +
    geom_sf(aes_string(fill = "attractiveness")) +
    scale_fill_viridis_c(option = "plasma", na.value = "grey90", limits = c(min_val, max_val)) +
    facet_wrap(~purpose, ncol = 4) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom") +
    labs(title = paste("Attractiveness Overview - Zone Type", zone_type), fill = "Attractiveness")

  ggsave(
    filename = file.path(output_dir, paste0("attractiveness_overview_zones_", zone_type, ".png")),
    plot = p,
    width = 60,
    height = 30,
    units = "cm",
    dpi = 300
  )
}

make_image_map <- function(zones_subset, zone_type, purpose, output_dir) {
  p <- ggplot(zones_subset) +
    geom_sf(aes_string(fill = purpose)) +
    scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
    theme_minimal() +
    labs(title = paste("Attractiveness for", purpose, "in Zone Type", zone_type), fill = "Attractiveness")

  ggsave(
    filename = file.path(output_dir, paste0("attractiveness_zones_", zone_type, "_", purpose, ".png")),
    plot = p,
    width = 15,
    height = 15,
    units = "cm",
    dpi = 300
  )
}

make_overview_image_map_grid <- function(grid_subset, zone_type, purposes, output_dir) {
  grid_long <- as.data.table(sf::st_drop_geometry(grid_subset))
  grid_long <- melt(
    grid_long,
    id.vars = setdiff(names(grid_long), purposes),
    measure.vars = purposes,
    variable.name = "purpose",
    value.name = "attractiveness"
  )

  grid_long <- sf::st_as_sf(
    grid_long,
    geometry = sf::st_geometry(grid_subset)[match(grid_long$grid_id, grid_subset$grid_id)]
  )

  min_val <- min(grid_long$attractiveness, na.rm = TRUE)
  max_val <- max(grid_long$attractiveness, na.rm = TRUE)

  p <- ggplot(grid_long) +
    geom_sf(aes_string(fill = "attractiveness")) +
    scale_fill_viridis_c(option = "plasma", na.value = "grey90", limits = c(min_val, max_val)) +
    facet_wrap(~purpose, ncol = 4) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom") +
    labs(title = paste("Attractiveness Overview (Regular Grid) - Zone Type", zone_type), fill = "Attractiveness")

  ggsave(
    filename = file.path(output_dir, paste0("attractiveness_overview_grid_zones_", zone_type, ".png")),
    plot = p,
    width = 60,
    height = 30,
    units = "cm",
    dpi = 300
  )
}

make_image_map_grid <- function(grid_subset, zone_type, purpose, output_dir) {
  p <- ggplot(grid_subset) +
    geom_sf(aes_string(fill = purpose)) +
    scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
    theme_minimal() +
    labs(title = paste("Attractiveness (Regular Grid) for", purpose, "in Zone Type", zone_type), fill = "Attractiveness")

  ggsave(
    filename = file.path(output_dir, paste0("attractiveness_grid_zones_", zone_type, "_", purpose, ".png")),
    plot = p,
    width = 15,
    height = 15,
    units = "cm",
    dpi = 300
  )
}
