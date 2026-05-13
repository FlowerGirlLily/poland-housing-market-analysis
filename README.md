# poland-housing-market-analysis
Time-series econometric analysis of Poland’s housing market and the effects of the Ukrainian refugee influx on housing affordability.

The original compiled dataset used in this project is no longer available.

The analysis was conducted using publicly available quarterly data from:
- Narodowy Bank Polski (NBP)
- Główny Urząd Statystyczny (GUS)

Variables included:
- Housing prices
- GDP
- Inflation (CPI)
- Interest rates
- War intervention dummy variable

The repository contains the original analytical workflow and methodology used in the research project.

# The Ukrainian Refugee Influx and Poland’s Housing Market

Independent undergraduate research project examining the impact of the war in Ukraine on housing prices in Poland using ARIMA/ARIMAX modelling in R.

## Research Question

To what extent did the war in Ukraine influence housing market prices in Poland?

## Methodology

The project uses:
- ARIMA / ARIMAX modelling
- Augmented Dickey-Fuller stationarity testing
- Residual diagnostics
- Ljung-Box autocorrelation testing
- Cross-validation and rolling-window forecasting

## Variables

Dependent variable:
- Housing prices per square meter (secondary market)

Independent variables:
- Inflation
- GDP
- Interest rates
- War intervention dummy variable

## Data Sources

- Narodowy Bank Polski (NBP)
- Główny Urząd Statystyczny (GUS)

## Main Findings

The analysis suggests that inflation and interest rates had stronger statistical relationships with housing price growth than the war intervention variable itself.

The refugee influx appears to have intensified pressure on rental markets and public perception of housing affordability, particularly in major Polish cities.

## Tools Used

- R
- forecast
- tseries
- ggplot2
- dplyr

## Repository Contents

- `analysis.R` — original analytical workflow
- `paper.pdf` — research paper
- `figures/` — selected charts and diagnostics
