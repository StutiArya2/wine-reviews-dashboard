# Wine Reviews Interactive Dashboard

## Overview
An interactive dashboard built with **R Shiny** to analyze ~130,000 wine reviews from the WineEnthusiast dataset. The dashboard provides comprehensive visual analytics for wine ratings, pricing, regional comparisons, and taster behavior.

## Dataset
- **Source:** WineEnthusiast Magazine (winemag.csv)
- **Records:** 129,971 wine reviews
- **Features:** 14 columns including country, variety, winery, points, price, taster info

## Dashboard Features

### 8 Interactive Tabs:
1. **Overview** - KPIs, points distribution, top countries & varieties
2. **Country Analysis** - Country comparisons, province drill-down, bubble charts
3. **Variety Analysis** - Top varieties by rating, price vs points comparison
4. **Distributions** - Histograms and segment analysis for price & points
5. **Price vs Points** - Scatter plots, correlation analysis, trend lines
6. **Taster Analysis** - Reviewer patterns, heatmaps, scoring behavior
7. **Top Wines** - Highest rated, best value, top wineries
8. **Data Explorer** - Filterable data table with download option

### Interactive Features:
- Global filters: Points range, Price range, Country, Variety
- Drill-down: Country to Province level
- Dynamic controls: Sample size, color grouping, log scale
- Tooltips on all charts
- Downloadable filtered data

## Tech Stack
- **R Shiny** - Web framework
- **shinydashboard** - Dashboard layout
- **Plotly** - Interactive visualizations
- **DT** - Interactive data tables
- **dplyr / tidyr** - Data manipulation
- **data.table** - Fast data loading

## Key Insights
- US dominates with highest review count; France and Italy lead in average ratings
- Strong positive correlation between price and points (higher price = higher rating)
- Pinot Noir and Chardonnay are most reviewed varieties
- Significant price variation within same rating brackets
- Tasters show distinct country specializations

## How to Run
1. Clone this repository
2. Download the dataset (winemag.csv) and place in Downloads folder
3. Install required packages:
```R
install.packages(c("shiny", "shinydashboard", "plotly", "DT",
                   "dplyr", "tidyr", "shinycssloaders", "data.table"))
