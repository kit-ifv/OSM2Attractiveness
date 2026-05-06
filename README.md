# OSM2Attractiveness

OSM2Attractiveness is a geodata processing workflow that converts OpenStreetMap (OSM) data into zone-level attractiveness indicators. It is originally developed by the [Karlsruhe Institute of Technology (KIT), Institute for Transport Studies](https://www.ifv.kit.edu/english/index.php).

The pipeline is intended for transport and spatial analysis use cases where destinations (e.g., shopping, services, leisure, health) need to be represented as comparable attractiveness values per zone.

At a high level, the workflow contains three stages:

1. Filter raw OSM data into category-specific OSM files.
2. Convert those category files into standardized POI geodata.
3. Aggregate POI information to zone-level attractiveness.

The repository currently includes an example configuration for German attractiveness factors and a German example area of interest (Rastatt town). It is easily configurable to be used for the specific factors of other areas of interest.

For methodological background see, e.g.:
* Klinkhardt, C.; Wörle, T.; Briem, L.; Heilig, M.; Kagerbauer, M.; Vortisch, P. (2021). Using OpenStreetMap as a Data Source for Attractiveness in Travel Demand Models. Transportation research record, 2675 (8), 294–303. [doi:10.1177/0361198121997415](https://doi.org/10.1177/0361198121997415)  

## Getting Started

This section explains how to run the project on a local machine.

### Prerequisites

Required software:

- Windows (current scripts are written and configured for Windows paths/commands)
- Python 3.10+ (recommended)
- R 4.2+ (for POI-to-attractiveness aggregation and evaluation scripts)
- [Osmosis](https://wiki.openstreetmap.org/wiki/Osmosis/Quick_Install_(Windows)) (required for OSM filtering)
- QGIS or other GIS software to repair buildings geometries

Python packages (see [requirements.txt](requirements.txt)): geopandas, pandas, shapely, pyyaml.

R packages used by the make scripts: this.path, yaml, data.table, sf

Data prerequisites:

- A raw OSM PBF file, available e.g. by [Geofabrik](https://download.geofabrik.de/)
- attractiveness-factors
- A zones layer (GeoJSON or similar)


### Installation

1. Clone the repository.

```sh
git clone https://github.com/kit-ifv/osm2attractiveness.git
cd osm2attractivites
```

2. (Recommended) Create and activate a virtual environment.

```sh
python -m venv .venv
.venv\Scripts\activate
```

3. Install Python dependencies.

```sh
pip install -r requirements.txt
```

4. Install R dependencies (example).

```r
install.packages(c("this.path", "yaml", "data.table", "sf"))
```

5. Adapt the config file in [config](config) or create your own by copying an example: [config/config_rastatt_example.yaml](config/config_rastatt_example.yaml)


## Usage

Run the workflow from repository root in the following order.

### 1) Filter OSM by Categories

Creates category-specific `.osm` files in `data/osm-filtered`.

```sh
python src/make/FilterOSM.py config_rastatt_example
```

Notes:

- The script first creates an area extract from the raw `.osm.pbf` using the configured bounding box.
- If no config argument is passed, all scripts default to `config_rastatt_example`.
- Filters use the syntax described in [Osmosis docs](https://wiki.openstreetmap.org/wiki/Osmosis/Detailed_Usage_0.48#--tag-filter_(--tf)). Filters are logical `OR`, i.e., only one conditions needs to be fulfilled.
- Filter files support multi-step filters: each non-empty line in `filter_<Category>.txt` or `reject_<Category>.txt` is applied as an additional `--tag-filter` step. In this way, logical `AND` conditions are can be achieved.

Example (`filter_SportsHall.txt`):

```txt
leisure=sports_centre
building=* building:part=yes
```

### 2) Convert Filtered OSM Data to POIs

Creates standardized points-of-interest files (GeoJSON) and a processed buildings file.

```sh
python src/make/OSM2POIs.py config_rastatt_example
```

### 3) Calculate Zone Attractiveness

Aggregates POIs to zone-level attractiveness numbers (CSV + GPKG).

```sh
Rscript src/make/POIs2attractiveness.r config_rastatt_example
```


### Optional: Evaluation Scripts

Generate maps with attractiveness value per activity (zone- and grid-based)

```sh
Rscript src/eval/eval_maps.r config_rastatt_example
```


## Folder Structure

### [config](config)

Area-specific configuration files and filter definitions:

- Main run configs (bounding box, paths, CRS, exclusions)
- Locale/config support files (e.g., POI parameters, attractiveness factors)
- OSM filter rule files in [config/filters](config/filters)

### [data](data)

Input and output data folders:

- `osm-raw`: raw OSM PBF sources
- `osm-filtered`: category-level OSM extracts
- `pois`: generated POI files
- `attractiveness*`: attractiveness calculation outputs

### [src/make](src/make)

Core processing scripts:

- `FilterOSM.py`
- `OSM2POIs.py`
- `POIs2attractiveness.r`

### [src/eval](src/eval)

Evaluation and QA scripts for generated POIs/attractiveness. They are in draft status.

### [docs](docs)

Project documentation and supporting notes. To be extended.

## Notes and Limitations

- Only dummy attractiveness factors are included. Replace them with validated attractiveness factors. For Germany, we recommend Ver_Bau  as a suitable reference: https://bbwsoftware.de/
- Buildings input often requires cleaning (repairing geometries) in a GIS before use (open the `multipolygons` layer in QGIS and run the tool `fixgeometries` with the following parameters: `INPUT='Region_Buildings.osm|layername=multipolygons' METHOD=1 OUTPUT='Region_Buildings_repaired.gpkg'`).
- Some filter categories exist that are currently not used in the attractiveness value generation (e.g., parking, hotels, tourism, EV charging, mailbox). They may be useful for other analyses.
- Workplaces are currently not generated by this workflow and should be integrated from other data/methods.
- Output should always be validated for local plausibility because OSM tagging quality and semantics vary by region.
- Generally, the attractiveness values are based on the average of similar places. Places that - for any reason - are in some way "special" may have attractiveness values that are orders of magnitude larger. Especially, care needs to be taken with respect to how tourist sites should be considered (depends on the aim of the travel demand model).

## Roadmap

The following are possible improvements for this workflow. We welcome contributions.

### Methodological

- Extend support to additional countries with reusable locale/config packages.
- Improve automatic detection/handling of unusually large specific-purpose areas (outliers in OSM tagging).
- Improve floor-area estimation logic, especially for multi-level buildings and partial-building occupancy.
- Improve building-height and vertical-structure assumptions.
- Add more transparent intermediate components/summands for easier plausibility checks.
- Create validated open-source data for attractiveness factors.

### Technical

- Simplify code.
- Use config in evaluation scripts.
- Add automated tests.
- Improve multi-OS support (current implementation is Windows-focused concerning Osmosis run commands).
- Evaluate migration from Osmosis (legacy) to Osmium or other maintained OSM-tooling.

## Contributing

Contributions are welcome. Please open an issue for bugs, data-quality edge cases, or methodological suggestions before major changes.

When contributing code, include:

- A short rationale for the change
- Any config/data assumptions
- Validation notes (what was tested and on which area)

## License

This project is licensed as described in [LICENSE.md](LICENSE.md).

## Acknowledgments

Thanks to the OpenStreetMap community for providing the data.
