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

make_html_map <- function(zones_sf_leaflet_subset, subset_name, purposes, output_dir, timestamp_string, grid_sf_leaflet_subset = NULL) {
  path_output_html <- file.path(output_dir, paste0("attractiveness_interactive_map_zones_", subset_name, "_", timestamp_string, ".html"))
  has_grid <- !is.null(grid_sf_leaflet_subset) && nrow(grid_sf_leaflet_subset) > 0
  zone_groups <- paste0(purposes, " (zones)")
  grid_groups <- paste0(purposes, " (grid)")

  map <- leaflet(zones_sf_leaflet_subset) %>%
    addTiles(group = "OpenStreetMap") %>%
    addProviderTiles(providers$CartoDB.Positron, group = "CartoDB") %>%
    setView(
      lng = mean(sf::st_bbox(zones_sf_leaflet_subset)[c(1, 3)]),
      lat = mean(sf::st_bbox(zones_sf_leaflet_subset)[c(2, 4)]),
      zoom = 10
    )

  palettes <- setNames(
    lapply(purposes, function(purpose) {
      palette_values <- zones_sf_leaflet_subset[[purpose]]
      if (has_grid && purpose %in% names(grid_sf_leaflet_subset)) {
        palette_values <- c(palette_values, grid_sf_leaflet_subset[[purpose]])
      }
      colorNumeric(palette = "viridis", domain = palette_values, na.color = "#808080")
    }),
    purposes
  )

  for (purpose in purposes) {
    pal <- palettes[[purpose]]
    zone_group <- paste0(purpose, " (zones)")
    grid_group <- paste0(purpose, " (grid)")
    detail_html <- build_purpose_details_html(zones_sf_leaflet_subset, purpose)

    popup_content <- paste0(
      "<strong>Zone ID:</strong> ", zones_sf_leaflet_subset$NO, "<br>",
      "<strong>Type:</strong> ", zones_sf_leaflet_subset$typ, "<br>",
      "<strong>Name:</strong> ", zones_sf_leaflet_subset$NAME, "<br>",
      "<strong>", purpose, ":</strong> ", format(round(zones_sf_leaflet_subset[[purpose]], 2), nsmall = 2), "<br><br>",
      detail_html
    )

    map <- map %>%
      addPolygons(
        data = zones_sf_leaflet_subset,
        fillColor = ~pal(zones_sf_leaflet_subset[[purpose]]),
        fillOpacity = 0.7,
        weight = 1,
        color = "white",
        group = zone_group,
        label = ~paste0(
          "Zone: ", NO, "<br>",
          purpose, ": ", format(round(zones_sf_leaflet_subset[[purpose]], 2), nsmall = 2)
        ),
        labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px")),
        popup = popup_content,
        highlightOptions = highlightOptions(weight = 3, color = "red", bringToFront = TRUE)
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = zones_sf_leaflet_subset[[purpose]],
        title = purpose,
        group = zone_group,
        opacity = 0.7
      )

    if (has_grid) {
      map <- map %>%
        addPolygons(
          data = grid_sf_leaflet_subset,
          fillColor = ~pal(grid_sf_leaflet_subset[[purpose]]),
          fillOpacity = 0.7,
          weight = 0.4,
          color = "#666666",
          group = grid_group,
          label = ~paste0(
            "Grid cell: ", grid_id, "<br>",
            purpose, ": ", format(round(grid_sf_leaflet_subset[[purpose]], 2), nsmall = 2)
          ),
          labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px")),
          popup = ~paste0(
            "<strong>Grid cell:</strong> ", grid_id, "<br>",
            "<strong>", purpose, ":</strong> ", format(round(grid_sf_leaflet_subset[[purpose]], 2), nsmall = 2)
          ),
          highlightOptions = highlightOptions(weight = 2, color = "red", bringToFront = TRUE)
        ) %>%
        addLegend(
          position = "bottomright",
          pal = pal,
          values = grid_sf_leaflet_subset[[purpose]],
          title = paste0(purpose, " (grid)"),
          group = grid_group,
          opacity = 0.7
        )
    }
  }

  overlay_groups <- zone_groups
  if (has_grid) overlay_groups <- c(overlay_groups, grid_groups)

  hide_groups <- zone_groups[-1]
  if (has_grid) hide_groups <- c(hide_groups, grid_groups)

  map <- map %>%
    addLayersControl(
      baseGroups = c("OpenStreetMap", "CartoDB"),
      overlayGroups = overlay_groups,
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
    hideGroup(hide_groups)

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
