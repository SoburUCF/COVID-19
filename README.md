# COVID-19 Studies




## Installation of subsampler:
Installation instructions are in: https://github.com/andersonbrito/subsampler
Conda did not work for me, so used mamba instead

To create the environment
```
$CONDA_NO_PLUGINS=true mamba env create -f subsampler.yaml 
```

## Flu-COVID-19 

To calculate the flu case rate per 100k population and death rate per 1M population, US state population data 
were collected from US Census Bureau
https://www.census.gov/data/tables/time-series/demo/popest/2020s-state-total.html 

Weekly Flu case data were collected from CDC FluView database
https://www.cdc.gov/fluview/overview/fluview-interactive.html?CDC_AAref_Val=https://www.cdc.gov/flu/weekly/fluviewinteractive.htm 
