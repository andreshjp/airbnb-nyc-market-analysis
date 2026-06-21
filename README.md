# Airbnb NYC Market Analysis

> Exploratory data analysis of the New York City Airbnb market using a real-world dataset with significant data quality issues. The project covers end-to-end data wrangling, multi-dimensional EDA, and visual storytelling across 17 charts — from pricing dynamics to review patterns across all five boroughs.

---

## Overview

The NYC Airbnb Open Data dataset presents a non-trivial ingestion challenge: a portion of records have their columns merged into the `id` field due to malformed CSV parsing, with commas embedded in listing names causing column misalignment. Handling this programmatically — without dropping records — was the first analytical task.

After reconstruction and cleaning, the analysis explores the structural dynamics of the NYC short-term rental market across five dimensions: **geography**, **price**, **room type**, **availability**, and **reviews**.

---

## Key Questions

1. How are listings distributed across NYC's five boroughs?
2. Which boroughs have the most distinct neighborhoods?
3. How does price vary by borough and room type?
4. What is the relationship between price and availability window?
5. How does listing availability differ across boroughs?
6. What are the review trends over time, and how do they differ by borough?
7. What is the average minimum stay requirement per borough?

The analysis then drills into the two dominant boroughs — **Manhattan and Brooklyn** — to compare pricing, room mix, availability, and review trajectories, and separately examines the three secondary markets: **Bronx, Queens, and Staten Island**.

---

## Data Wrangling

The raw dataset had two structural issues requiring programmatic resolution:

- **Malformed rows:** A subset of records had their columns concatenated into the `id` field because listing names contained commas. These were identified via regex, the embedded commas within names were temporarily replaced with `|`, the row was re-split on `,`, and names were restored.
- **Type coercion:** After merging both clean and reconstructed subsets, numeric and factor columns were coerced to their correct types.
- **NA handling:** Rows with 3 or more missing values were dropped. The final dataset retains all recoverable observations.

---

## Visualizations

| Chart | Type | Insight |
|-------|------|---------|
| Listings by borough | Treemap | Manhattan (44.3%) and Brooklyn (41.1%) dominate |
| Price by borough | Boxplot | Manhattan commands a clear premium; wide tails in all boroughs |
| Price by room type | Boxplot | Entire home listings price ~2x private rooms across boroughs |
| Average price by availability window | Bar + error bars | Listings with limited availability (<1 month) price higher on average |
| Availability heatmap | Heatmap | Queens and Bronx have proportionally more year-round availability |
| Reviews by year | Bar | Consistent growth from 2011 with a drop in 2019 (data cutoff effect) |
| Reviews by borough (with NAs) | Stacked bar | Differential review completion rates across boroughs |
| Reviews by borough × year | Grouped bar | Manhattan and Brooklyn drive review volume |
| Room type mix: Manhattan vs. Brooklyn | Stacked bar | Brooklyn has a higher share of private rooms |
| Prices by room type: Manhattan vs. Brooklyn | Grouped bar | Manhattan entire-home listings are ~30% more expensive than Brooklyn |
| ...and more | | Full borough-level drill-downs for availability and review trends |

---

## Tech Stack

| Tool | Role |
|------|------|
| `R` | Data wrangling and analysis |
| `dplyr`, `tidyr`, `stringr` | Data manipulation |
| `ggplot2` | Visualization |
| `treemapify` | Treemap charts |
| `reshape2` | Data reshaping for stacked charts |
| `readr`, `writexl` | I/O |

---

## Project Structure

```
airbnb-nyc-market-analysis/
├── analysis.R                  # Full wrangling and analysis script
├── airbnb_nyc_data.csv         # Source dataset (NYC Airbnb Open Data)
└── plots/                      # Exported visualizations (17 charts)
    ├── total_neighbourhood_group.jpg
    ├── price_by_neighbourhood.jpg
    ├── price_by_roomtype.jpg
    ├── price_vs_availability.jpg
    ├── neighbourhood_group_vs_availability.jpg
    ├── reviews_by_year.jpg
    └── ...
```

---

## Key Findings

- **Manhattan and Brooklyn** account for ~85% of all listings; Staten Island and Bronx are thin markets.
- **Entire home/apartment** listings command roughly double the median price of private rooms across all boroughs.
- Listings with **availability under 30 days** show higher average prices — consistent with revenue management behavior (hosts restrict high-demand periods).
- Price and availability show near-zero correlation (r ≈ 0.002), suggesting pricing is driven by location and room type rather than scarcity.
- **Review volume grew consistently** from 2011 through 2018; Manhattan and Brooklyn account for over 80% of all reviews.

---

## How to Run

```r
# Install required packages
install.packages(c("readr", "dplyr", "stringr", "writexl", "tidyr",
                   "ggplot2", "treemapify", "reshape2"))

# Set your working directory to the project folder, then:
source("analysis.R")
```

All charts are saved as `.jpg` / `.png` files in the working directory.

---

## Data Source

[New York City Airbnb Open Data](https://www.kaggle.com/datasets/dgomonov/new-york-city-airbnb-open-data) — Kaggle, sourced from Inside Airbnb. Contains ~49,000 listings with pricing, availability, location, and review data.

---

## Skills Demonstrated

`Data Wrangling` · `Exploratory Data Analysis` · `Data Visualization` · `ggplot2` · `dplyr` · `String Manipulation` · `Regex` · `R` · `Storytelling with Data`
