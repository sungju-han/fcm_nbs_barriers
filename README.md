# FCM Analysis for NBS Implementation Barriers

This repository contains the R code used in the paper "Beyond Individual Barriers: Feedback Mechanisms and Barrier Systems of Nature-Based Solutions for Reducing Hydro-meteorological Risks" (working). The code implements Fuzzy Cognitive Mapping (FCM) analysis to understand barrier dynamics in Nature-Based Solutions implementation across European river basins.

## Features

The code provides functions for:
- Calculating network metrics (density, complexity)
- Computing centrality measures (indegree, outdegree)
- Global normalization of centrality scores
- Analyzing feedback loops in FCM
- Calculating transformative potential

## Requirements

- R (version 4.0.0 or higher)
- Required packages:
  - igraph: For network analysis
  - here: For file path management

## Installation

```R
# Install required packages
install.packages("igraph")
install.packages("here")
```

## Data Format
The input data should be a CSV file with:

- Row and column names representing FCM components
- Cell values representing relationship weights (-1 to 1)
- First column as row names

## Citation
If you use this code in your research, please cite:
Han, S., Plavsic, J., Kovačević-Majkić, J., Izydorczyk, K., Krauze, K., Kuzior, M., Włodarczyk-Marciniak, R., Kuhlicke, C. (2025). Beyond Individual Barriers: Feedback Mechanisms and Barrier Systems of Nature-Based Solutions for Reducing Hydro-meteorological Risks. Preprint.
