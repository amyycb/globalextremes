# 🌍 GSDR-IDF: Global Intensity-Duration-Frequency Curves

This repository hosts the R Shiny application for the **GSDR-IDF Data Portal**, an interactive tool for exploring global intensity-duration-frequency (IDF) curves derived from the **Global Sub-Daily Rainfall (GSDR)** dataset. The app supports research and decision-making in flood risk assessment, climate resilience, and hydrology.

## 📊 Dataset Overview

The GSDR-IDF dataset is based on over **24,000 hourly rain gauge records** from the GSDR dataset. It provides:

- IDF curves for durations of 1, 3, 6, and 24 hours  
- Return levels for 10-, 30-, and 100-year events  
- Generalized Extreme Value (GEV) parameters from both Single Gauge Analysis (SGA) and Regional Frequency Analysis (RFA)  
- Interactive maps and plots for global rainfall extremes  

## 🧰 App Features

- Interactive leaflet maps with zoom and search  
- Filters for method (SGA/RFA), duration, return level, and GEV parameters  
- Pop-up metadata for selected gauges  
- Downloadable plots and data (.csv and .png)  
- Warning system for ungauged locations beyond 200 km  

## 🚀 Getting Started

### Prerequisites

Install R and the following packages:

```r
install.packages(c("shiny", "leaflet", "dplyr", "ggplot2", "sf", "DT", "lubridate"))

