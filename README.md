# Toronto_Vehicle_Collision_Frequency_Project
## Background
A repository for code and report files related to our investigation into temporal and geographical trends in vehicle collision data, collected by the Toronto Police Service within the city of Toronto, Ontario, Canada. 

This project was done to fulfill the group project requiremenets for the Summer 2026 INF2167 course at the University of Toronto.

Authored by Stella Gregorski & Yiying Qin

The data used in this project were obtained from https://data.tps.ca/datasets/TorontoPS::traffic-collisions-open-data-asr-t-tbl-001/about, and are provided in this repository to promote reproducibility as allowed under the Open Government License - Ontario. The folder data/01-raw data contains the initial dataset as downloaded, so anyone attempting to re-create these results can do so using the same starting data. All analysis done within script files and within the report was done using the data file in data/02-analysis data, so changes can be seen. 

## Repository Structure
The repo is structured as:

`data/01 - raw data` contains the raw data as downloaded from the Toronto Police Service
`data/02 - analysis data` contains the dataset modified during the project
`models` contains fitted models described in the report
`paper` contains the Quarto files and BibTeX bibliography files used to generate the project proposal and final report documents, as well as rendered PDF versions of both documents
`scripts` contains R script files used to clean data, generate visuals, and fit models
`presentation` contains the Quarto file and BibTeX file used to generate the final presentation slideshow, as well as a rendered HTML version of the slideshow to use when presenting
