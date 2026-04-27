# Pipeline Variable Overview

This document summarizes the category variables used across the OSM to POIs to attractiveness pipeline.

Metric format:
- `Count`, `Area`, or `Floor Area`
- Metric = - means the category is filtered/exported but not scored in the sample attractiveness configuration.

| Name | Name translated (German) | Categories included | Metric (by default) | Remarks |
|---|---|---|---|---|
| AllotmentGardens | Kleingaerten | Allotment garden areas and garden colonies. | Area |  |
| Authority | Behoerde | Town halls, government offices, and public administration offices. | Count | Broad public-administration category. |
| Bank | Bank | Banks and ATMs, including bank buildings. | Count |  |
| Beach | Strand | Beach areas. | Area |  |
| Cemetery | Friedhof | Cemeteries and graveyards. | Area |  |
| Church | Kirche | Churches, mosques, synagogues, temples, chapels, cathedrals, and other places of worship. | Area | |
| Cinema | Kino | Cinemas and planetariums. | Floor Area | |
| DailyShopping_BakeryButcherKiosk | Einkauf_taeglich_BaeckerMetzgerKiosk | Bakeries, butcher shops, kiosks, and similar small daily-shopping outlets. | Count |  |
| DailyShopping_Drugstore | Einkauf_taeglich_Drogerie | Drugstores and chemists. | Area |  |
| DailyShopping_Other | Einkauf_taeglich_Sonstiges | Beverage shops, dairies, pastry shops, convenience stores, greengrocers, and organic food shops. | Area | Catch-all for smaller daily shopping uses. |
| DailyShopping_Supermarket | Einkauf_taeglich_Supermarkt | Supermarkets and food wholesale supermarkets before the pipeline splits them into subtypes. | No factor | Umbrella filter only; the pipeline scores the derived subcategories below. |
| DailyShopping_Supermarket_AldiLidl | Einkauf_taeglich_Supermarkt_AldiLidl | Discount supermarkets such as as Aldi or Lidl. | Area |  |
| DailyShopping_Supermarket_Hypermarket | Einkauf_taeglich_Supermarkt_Verbrauchermarkt | Large-format supermarkets and hypermarkets. | Area |  |
| DailyShopping_Supermarket_Supermarket | Einkauf_taeglich_Supermarkt_Supermarkt | Standard supermarkets that are neither discount discount stores nor hypermarkets. | Area |  |
| Doctor | Arzt | Medical practices and healthcare facilities such as doctors, dentists, physiotherapists, clinics, psychotherapists, podiatrists, and veterinary services. | Count | |
| EV_ChargingStation | E_Ladestation | Electric vehicle charging stations. | - | Exported infrastructure layer only. |
| FitnessCenter | Fitnesscenter | Fitness centres, gyms, and similar fitness buildings. | Area |  |
| Hairdresser | Friseur | Hairdressers, beauty services, massage, and opticians. | Count |  |
| Hospital | Krankenhaus | Hospitals and hospital buildings. | Floor Area | Floor area based; buildings are especially relevant in processing. |
| Hotel | Hotel | Hotels, motels, hostels, guest houses, chalets, and apartment-style accommodation. | - |  |
| Kindergarten | Kindergarten | Kindergartens, childcare, and preschool facilities. | Count | Broad early-childhood services bucket. |
| Library | Bibliothek | Libraries and library facilities. | Floor Area |  |
| LongTermShopping | Einkauf_langfrist | General long-term shopping umbrella category. | | Umbrella filter exists, but the sample scoring config uses the dedicated subcategories below instead. |
| LongTermShopping_DepartmentStore | Einkauf_langfrist_WarenKaufhaus | Department stores, electronics stores, and clothing stores. |  |  |
| LongTermShopping_DIYGardenCenter | Einkauf_langfrist_BauGartenmarkt | DIY stores, hardware stores, garden centres, and garden-furniture stores. | Area |  |
| LongTermShopping_FurnitureStore | Einkauf_langfrist_Moebelmarkt | Furniture, bed, kitchen, and carpet stores. | Floor Area | Floor area based. |
| LongTermShopping_Other | Einkauf_langfrist_Sonstiges | Other larger shopping destinations and fuel-station retail. | Floor Area |  |
| Mailbox | Briefkasten | Post boxes and mailboxes. | - |  |
| Museums | Museen | Museums, galleries, and arts centres. | Floor Area |  |
| Park | Park | Parks and recreation grounds. | Area |  |
| Parking | Parken | Parking areas, parking spaces, parking buildings, and parking aisles. | - | Exported infrastructure layer only. |
| Pharmacy | Apotheke | Pharmacies and pharmacy buildings. | Count |  |
| Playground | Spielplatz | Playgrounds. | Area |  |
| PostOffice | Post | Post offices. | Count |  |
| RegionalRail | Regionalbahn | Regional rail stations and halts. | Count |  |
| Restaurant | Restaurant | Restaurants, cafes, bars, biergartens, fast-food outlets, food courts, pubs, and ice-cream venues. | Count |  |
| Schools | Schulen | Schools and school buildings. | - |  |
| SmallSportsField | Sportplatz_klein | Smaller (high-density) sport and recreation facilities such as basketball, climbing, table tennis, skating, shooting, bowling, and similar uses. | Area |  |
| SportsField | Sportplatz | Outdoor pitches and running tracks. | Area |  |
| SportsHall | Sporthalle | Sports halls, sports centres, and related indoor sports facilities. | Area | Indoor sports bucket; filter partially overlaps with sports-centre style uses. |
| SwimmingPool | Schwimmbad | Swimming pools and water parks. | Area | The translation aliases `Freibad` and `Hallenbad` exist, but the sample pipeline keeps one combined category. |
| Theater | Theater | Theatres and social centres used as cultural venues. | Area | Broader than theatre alone because social centres are included. |
| Tourism | tourism | General tourism attractions such as aquariums, attractions, galleries, museums, theme parks, viewpoints, zoos, and visitor information. | - | Broad tourism layer; overlaps with scored specialist classes such as `Museums` and `Zoo`. |
| Universities | Hochschulen | Universities and university campuses. | - |  |
| Zoo | Zoo | Zoos. | Area |  |

## Notes

- 