# Savanna_structure_GEDI

Statistical work about the GEDI data parallel to the savanna-forest project.

# Folders organisation

- Raw data from Le Bien outside of this rpeo
- **archives** : all old codes that I have used, that can contain chunks that are worth copy-paste, potentially linked to the first explorations of the data
- **big_subsampling_300km_pour_hierarchique** : data to crate a 300km grid, once you have the 10km subsampled data, in order to perform a JAGS hierarchical model
- **data_pop_inf_10** : folder to store csv and .geojson files (once you have the 10km subsampled (+300 km grid) data), with R/tentative_intersection.Rmd and QGIS
- **figures** : to store the figures produced with the code + some csv, pdfs and .odp
- **geojson_files** : to store the .geojson files (vectorial layer in QGIS) that do not belong to any other folder
- **glm_outputs** : to store glm outputs
- **JAGS_outputs** :    **R/complete_model_jags.Rmd** (and **R/hierarchical_model_jags.Rmd**) produce folders in JAGS_outputs that contain .txt and .pdf files of the outputs and resimulation. Those folder contain the time at which it was created + some potential info, like 2024-07-10 11:07:41.256236hierarchique for instance.
- **R** : contain all the scripts, in particular :
  - **preprocessing_from_raw_data.Rmd** : that created *complete_corresponding_table_without_duplicate.RDS* and *complete_corresponding_table_without_duplicate_standardized.RDS* in **rawdata_post_preprocessing** from Le Bien's data

  - **sub_sampling_grid.RMD** :  create .RDS files of the 10km subsampling data in **transformed data**, + .geojson files to verify if the subsampling was correlty performed

  - **complete_model_jags.Rmd** : produce folders in JAGS_outputs that contain .txt and .pdf files of the outputs and resimulation. Those folder contain the time at which it was created + some potential info, like 2024-07-09 15:32:59.396984_Kg_pas_borne for instance.

  - **hierarchical_model_jags.Rmd** : produce folders in JAGS_outputs that contain .txt and .pdf files of the outputs of the 300km hierarchical model. Those folder contain the time at which it was created + some potential info, like 2024-07-10 11:07:41.256236hierarchique for instance.

  -  **tentative_intersection.Rmd** : a file that only retains points in sparsely populated areas (<10 inhabitants/km2) in the **data_pop_inf_10** folder

- **rawdata_post_preprocessing** : to store raw data post preprocessing (i.e. fire NA replacement + normalizations) in .RDS

- **simulations_allures_courbes** : R codes to get a grip over the functions of the model. In particular contains **visualisation_20_fonctions_estimations_G.Rmd** that creates a lot of pdf figures in the folder **figures/visualisation_20_fonctions_estimations_G_all_pdf**
- **transformed_data** : contain the 10km subsampled data for each ecoregion or all Africa
- **transformed_data_ilots** : supposed to contain 10km*10km clusters of points distant of 10 km one of each other, in order to perform a glmm or something

For more info, see the readme files inside the folders.