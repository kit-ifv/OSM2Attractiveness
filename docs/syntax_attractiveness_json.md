# Syntax of `attractiveness_factors_*.json`

Overall idea: The file defines how each POI contributes to trip-purpose attractiveness (i.e., it defines the attractiveness factors).

## Required JSON structure

Top level: JSON object

- Key = POI category name (must match the POI filename category part)
- Value = object with one or more trip purposes, additionally (facultative) category-wide settings

Purpose level:

- Key = trip purpose name (e.g. `ShoppingDaily`, `PrivateBusiness`, `LeisureOutdoor`)
- Value = either array form or object form

### Array form

```json
"<Purpose>": ["<Metric>", <Coefficient>]
```

Optional cap:

```json
"<Purpose>": ["<Metric>", <Coefficient>, <max_size>]
```

### Object form

```json
"<Purpose>": {
	"metric": "<Metric>",
	"coefficient": <Coefficient>,
	"max_size": <max_size>,
	"adjuster": "linear_size_multiplier(size_min, size_max, mult_at_min, mult_at_max)"
}
```

`max_size` and `adjuster` are optional.


## Allowed metric values

These strings are valid for `<Metric>`:

- `Count`: each POI contributes `1`.
- `Area`: uses POI field `Area`.
	- Missing values (originating from POIs not tagged as polygons but as nodes) are imputed with the 15th percentile of valid positive values in that category/purpose.
- `FloorArea`: prefers POI field `FloorArea`; if missing/non-positive, computes `Area * Level_number`.
	- `Level_number` defaults to `1` if missing/non-positive.
	- `Area` imputation follows the same 15th-percentile rule.

## Semantics of each field

- `coefficient`: scalar multiplier applied to each POI metric value.
- `max_size`: optional upper cap applied before multiplying by `coefficient`. Must be finite and `> 0`.
- `adjuster`: optional multiplier function (object form only).

Attractiveness per POI is:

`attractiveness = adjusted_metric_value * coefficient`

where `adjusted_metric_value` is metric value after optional `max_size` capping and optional `adjuster` multiplication.


## `adjuster` syntax and restrictions

Currently, only one adjuster function is accepted:

`linear_size_multiplier(size_min, size_max, mult_at_min, mult_at_max)`

or (optional 5-argument form)

`linear_size_multiplier(size_min, size_max, size_cap, mult_at_min, mult_at_max)`

Behavior:

- Linear interpolation from `mult_at_min` (at `size_min`) to `mult_at_max` (at `size_max`)
- Values below `size_min` use `mult_at_min`
- In the 4-argument form, values above `size_max` are capped so that `adjusted_metric_value` does not grow beyond `size_max * mult_at_max`
- In the 5-argument form, values in `(size_max, size_cap]` use `mult_at_max`, and values above `size_cap` are capped so that `adjusted_metric_value` does not grow beyond `size_cap * mult_at_max`
- Missing/non-finite size values get neutral multiplier `1`

## Category-wide settings
`filter`: Object filter, currently implemented: `min_area` (only include objects with area of at least n; caution: objects with no area at all are included as well)
`percentile_imputation`: Category-specific override for global imputation value


## Filename matching requirement

Each top-level category must map to an existing POI GeoJSON input file (generated from OSM2POIs.py) by:

`<poi_file_prefix>_<Category><poi_file_suffix>`

In the example config, this corresponds to files like `Rastatt_<Category>.geojson`.

## Minimal example

```json
{
	"Doctor": {
		"PrivateBusiness": ["Count", 20.0]
	},
	"Park": {
		"LeisureOutdoor": {
			"metric": "Area",
			"coefficient": 0.06,
			"max_size": 1000000
		}, 
		"filter": { min_area: 100 }
	},
	"LongTermShopping_Other": {
		"ShoppingOther": {
			"metric": "FloorArea",
			"coefficient": 1.0,
			"adjuster": "linear_size_multiplier(200, 800, 0.7, 0.2)"
		}, 
		"percentile_imputation": 0.4
	}
}
```
