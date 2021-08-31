## Measuring Sustainable Tourism with Online Platform Data

Code and data to produce figures and replicate web-crawling and classifier training for the paper 'Measuring Sustainable Tourism with Online Platform Data'. 
(_Link to paper to be added_)

__Please cite as__: _Hoffmann et al. (2021) Measuring Sustainable Tourism with Online Platform Data. Preprint._

### Description

**Last modified**: 31.08.2021

**Authors**: Felix Hoffmann, Fabian Braesemann, Timm Teubner

**Abstract**: Sustainability in tourism is a topic of global relevance, finding multiple mentions in the United Nations Sustainable Development Goals. The complex task of balancing tourism’s economic, environmental and social effects requires detailed and up-to-date data. Expanding upon a pilot project for the Albanian market by Braesemann, this paper investigates whether online platform data from [TripAdvisor](https://www.tripadvisor.com) can be employed as an alternative data source in sustainable tourism statistics. Using a web-scraped dataset of the European market, a sustainability label for accommodations can be predicted reasonably well with supervised learning techniques. 



### Folder Structure

- Data
  - Fig1_data.csv (65,000 accommodations from the Training data)
  - Fig3A_data.csv (ML performance data)
  - Fig3B_inset_data.csv (Random draw data)
  - Fig3C_data.csv (geocoded accommodations)
  - Final_Training_Data.csv (data to train ML models)
  - Shapefiles.zip (shapefiles for map in Fig 3C)

- Crawling (Python)
  - Collecting links to listings in 37 countries (Scraping_Links.ipynb)
  - Scraping content of listings (Scraping_Listings.ipynb)

- Analysis (Python)
  - Unsupervised learning (Unsupervised_Learning.ipynb)
  - Grid search of applicable pre-processing (Grid_Search.ipynb)
  - Training & Application of ML algorithms (TO BE ADDED)
  
- Visualization (R)
  - Fig1.R (descriptive statistics)
  - Fig2.R (unsupervisead learning: PCA and clustering)
  - Fig3AB.R (ML performance and comparison to random draw)
  - Fig3C.R (Europe map)

  __NOTES__:
  - Fig 1 & Fig 2 require the same data set (Fig1_data.csv)
  - Table (Fig 2B) and confusion matrices (Fig 3B) not included
  - Fig 3C requires shapefiles provided in Shapefiles.zip
  - To run the code, download the data from the data folder into the same folder as the R-files

