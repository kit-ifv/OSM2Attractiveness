# Build maps for each purpose to check spatial distribution of attractiveness and identify potential issues (e.g. zones with very high/low values, spatial patterns, etc.).
# Build maps in four different spatial zones: based on field "type" in zones file (values: 1, 2, 3, 4), as the values will be very different in all four types.
# Export the maps in an output directory in png file format. make one overview file for each zone type and furthermore one plot for each purpose.

library(leaflet)
library(ggplot2)
library(data.table)

timestamp <- "2026-05-01_1200"
name <- "attractiveness"
general_output_path <- "attractiveness_results"
zones_file <- "zellen_25832.geojson"
zone_types <- c("1")
grid_cellsize_m <- 1000
grid_cellsize_m_by_type <- c(
	"1" = 1000,
	"2" = 2000,
	"3" = 5000,
	"4" = 5000
)





build_purpose_details_html <- function(data_sf, purpose) {
	detail_cols <- grep(paste0("_", purpose, "$"), names(data_sf), value = TRUE)
	if (length(detail_cols) == 0L) {
		return(rep("<em>No detailed values for this purpose available.</em>", nrow(data_sf)))
	}

	vals_mat <- as.data.frame(sf::st_drop_geometry(data_sf[, detail_cols, drop = FALSE]))
	labels <- sub(paste0("_", purpose, "$"), "", detail_cols)

	details_vec <- vapply(seq_len(nrow(vals_mat)), function(i) {
		row_vals <- as.numeric(vals_mat[i, ])
		row_vals[is.na(row_vals)] <- 0

		line_items <- paste0(
			"<li><strong>", labels, ":</strong> ",
			format(round(row_vals, 2), nsmall = 2),
			"</li>"
		)

		paste0(
			"<strong>Details (Kategorie):</strong><br>",
			"<ul style='margin:4px 0 0 16px; padding:0;'>",
			paste(line_items, collapse = ""),
			"</ul>"
		)
	}, character(1))

	details_vec
}


build_regular_grid_attractiveness <- function(zones_subset, purposes, cellsize_m = 1000) {
	if (nrow(zones_subset) == 0L) {
		return(NULL)
	}

	zones_work <- zones_subset[, c("NO", purposes)]
	zones_work <- zones_work[!sf::st_is_empty(zones_work), ]
	if (nrow(zones_work) == 0L) {
		return(NULL)
	}

	zones_work <- sf::st_make_valid(zones_work)

	grid <- sf::st_make_grid(zones_work, cellsize = cellsize_m, square = TRUE, what = "polygons")
	grid_sf <- sf::st_sf(grid_id = as.character(seq_along(grid)), geometry = grid)

	intersections <- suppressWarnings(sf::st_intersection(zones_work, grid_sf))
	if (nrow(intersections) == 0L) {
		return(NULL)
	}

	intersections$intersection_area <- as.numeric(sf::st_area(intersections))
	intersections_dt <- as.data.table(sf::st_drop_geometry(intersections))

	for (purpose in purposes) {
		tmp <- as.data.frame(intersections_dt[, c("grid_id", "intersection_area", purpose), with = FALSE])
		tmp <- tmp[!is.na(tmp[[purpose]]) & tmp[["intersection_area"]] > 0, , drop = FALSE]

		if (nrow(tmp) == 0L) {
			grid_sf[[purpose]] <- NA_real_
			next
		}

		agg <- aggregate(
			x = data.frame(
				weighted_value = tmp[[purpose]] * tmp[["intersection_area"]],
				weight = tmp[["intersection_area"]]
			),
			by = list(grid_id = tmp[["grid_id"]]),
			FUN = sum
		)
		agg[[purpose]] <- agg$weighted_value / agg$weight
		agg_dt <- agg[, c("grid_id", purpose), drop = FALSE]
		grid_sf <- merge(grid_sf, agg_dt, by = "grid_id", all.x = TRUE)
	}

	row_has_data <- rowSums(!is.na(as.data.frame(sf::st_drop_geometry(grid_sf[, purposes, drop = FALSE])))) > 0
	grid_sf[row_has_data, ]
}


make_html_map <- function(zones_sf_leaflet_subset, subset_name, purposes, output_dir, timestamp_string, grid_sf_leaflet_subset = NULL) {
	path_output_html <- file.path(output_dir, paste0("attractiveness_interactive_map_zones_", subset_name, "_", timestamp_string, ".html"))
	has_grid <- !is.null(grid_sf_leaflet_subset) && nrow(grid_sf_leaflet_subset) > 0
	zone_groups <- paste0(purposes, " (zones)")
	grid_groups <- paste0(purposes, " (grid)")

  # Create base map
  map <- leaflet(zones_sf_leaflet_subset) %>%
  	addTiles(group = "OpenStreetMap") %>%
  	addProviderTiles(providers$CartoDB.Positron, group = "CartoDB") %>%
		setView(lng = mean(sf::st_bbox(zones_sf_leaflet_subset)[c(1, 3)]),
					lat = mean(sf::st_bbox(zones_sf_leaflet_subset)[c(2, 4)]),
  					zoom = 10)
  
  # Create color palettes for each purpose
  palettes <- setNames(
  	lapply(purposes, function(purpose) {
  		colorNumeric(
  			palette = "viridis",
  			domain = zones_sf_leaflet_subset[[purpose]],
  			na.color = "#808080"
  		)
  	}),
  	purposes
  )
  
  # Add layers for each purpose
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
  
  # Add layer control
	overlay_groups <- zone_groups
	if (has_grid) {
		overlay_groups <- c(overlay_groups, grid_groups)
	}
	hide_groups <- zone_groups[-1]
	if (has_grid) {
		hide_groups <- c(hide_groups, grid_groups)
	}

  map <- map %>%
  	addLayersControl(
  		baseGroups = c("OpenStreetMap", "CartoDB"),
		overlayGroups = overlay_groups,
  		options = layersControlOptions(collapsed = FALSE)
  	) %>%
		hideGroup(hide_groups)  # Hide all but first zone-purpose by default
  
  # Save map
  htmlwidgets::saveWidget(map, file = path_output_html, selfcontained = TRUE)
	message("Interaktive Karte erstellt: ", path_output_html)	
}

make_overview_image_map <- function(zones_subset, zone_type, purposes, output_dir) {
	# Pivot data to long format for faceting
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
	
	# Calculate common scale across all purposes
	min_val <- min(zones_long$attractiveness, na.rm = TRUE)
	max_val <- max(zones_long$attractiveness, na.rm = TRUE)
	
	p <- ggplot(zones_long) +
		geom_sf(aes_string(fill = "attractiveness")) +
		scale_fill_viridis_c(option = "plasma", na.value = "grey90", limits = c(min_val, max_val)) +
		facet_wrap(~purpose, ncol = 4) +
		theme_minimal() +
		theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom") +
		labs(title = paste("Attractiveness Overview - Zone Type", zone_type), fill = "Attractiveness")
	  
	
	ggsave(filename = file.path(output_dir, paste0("attractiveness_overview_zones_", zone_type, ".png")), plot = p, width = 60, height = 30, units = "cm", dpi = 300)
}

make_image_map <- function(zones_subset, zone_type, purpose, output_dir) {
	p <- ggplot(zones_subset) +
      geom_sf(aes_string(fill = purpose)) +
      scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
      theme_minimal() +
      labs(title = paste("Attractiveness for", purpose, "in Zone Type", zone_type), fill = "Attractiveness")
    
    ggsave(filename = file.path(output_dir, paste0("attractiveness_zones_", zone_type, "_", purpose, ".png")), plot = p, width = 15, height = 15, units = "cm", dpi = 300)
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


path_output_dir <- paste(general_output_path, name, sep = "/")
path_output_dir_maps <- paste(path_output_dir, "Maps", sep = "/")

if (!dir.exists(path_output_dir_maps)) {
 dir.create(path_output_dir_maps, recursive = TRUE)
}

if (!exists("path_output_dir")) {
  stop("Variable 'path_output_dir' fehlt. Sie wird benötigt, um Ergebnisdaten zu laden.")
}

path_attractiveness <- file.path(path_output_dir, "attractiveness.csv")
if (!file.exists(path_attractiveness)) {
  stop(paste0("Datei für Attraktivitäten nicht gefunden: ", path_attractiveness))
}

attractiveness <- fread(path_attractiveness)

purposes <- colnames(attractiveness)[-1]  # assuming first column is zoneId

zones_sf <- sf::st_read(zones_file, quiet = TRUE)
zones_sf <- merge(zones_sf, attractiveness, by.x = "NO", by.y = "zoneId", all.x = TRUE)

details_csv <- file.path(path_output_dir, "attractiveness_detailed_by_category_purpose.csv")
if (!file.exists(details_csv)) {
	stop(paste0("Detail-Datei nicht gefunden: ", details_csv))
}

details_dt <- data.table::fread(details_csv)
if (!("zoneId" %in% names(details_dt))) {
	stop("In attractiveness_detailed_by_category_purpose.csv fehlt die Spalte 'zoneId'.")
}

zones_sf$NO <- as.character(zones_sf$NO)
details_dt[, zoneId := as.character(zoneId)]
zones_sf <- merge(zones_sf, details_dt, by.x = "NO", by.y = "zoneId", all.x = TRUE)

zones_sf_leaflet <- zones_sf %>%
	sf::st_transform(4326)  # Convert to WGS84 for leaflet

for (zone_type in zone_types) {
  print(paste("Erstelle Karten für Zonentyp:", zone_type))
  zones_subset <- zones_sf[zones_sf$typ == zone_type, ]
	grid_cellsize_zone <- grid_cellsize_m
	if (zone_type %in% names(grid_cellsize_m_by_type)) {
		grid_cellsize_zone <- as.numeric(grid_cellsize_m_by_type[[zone_type]])
	}
	print(paste("Reguläre Rasterauflösung (m):", grid_cellsize_zone))
	zones_grid_subset <- build_regular_grid_attractiveness(zones_subset, purposes, cellsize_m = grid_cellsize_zone)
  
  for (purpose in purposes) {
	  make_image_map(zones_subset, zone_type, purpose, path_output_dir_maps)
	  if (!is.null(zones_grid_subset) && nrow(zones_grid_subset) > 0) {
		  make_image_map_grid(zones_grid_subset, zone_type, purpose, path_output_dir_maps)
	  }
  }
  make_overview_image_map(zones_subset, zone_type, purposes, path_output_dir_maps)
	if (!is.null(zones_grid_subset) && nrow(zones_grid_subset) > 0) {
		make_overview_image_map_grid(zones_grid_subset, zone_type, purposes, path_output_dir_maps)
	}

	zones_sf_leaflet_subset <- zones_sf_leaflet[zones_sf_leaflet$typ == zone_type, ]
	zones_grid_leaflet_subset <- NULL
	if (!is.null(zones_grid_subset) && nrow(zones_grid_subset) > 0) {
		zones_grid_leaflet_subset <- sf::st_transform(zones_grid_subset, 4326)
	}
	make_html_map(
		zones_sf_leaflet_subset,
		subset_name = as.character(zone_type),
		purposes,
		path_output_dir_maps,
		timestamp,
		grid_sf_leaflet_subset = zones_grid_leaflet_subset
	)

}
