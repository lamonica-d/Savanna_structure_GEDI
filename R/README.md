# Listes des scripts R présents dans ce folder 
                     
- **"paths.R"** : fichier source qu'on charge dans tous les autres fichiers pour remplir automatiquement les chemins vers les données, les figures etc
- **"README.md"**     

## Première partie
- **"visualisation_donnes_manquantes.Rmd"** : visualisation NA selon les variables

 - **"aires_geographiques.Rmd"** : génération des cartes des variables dans l'espace et des zones de NA associées à chaque variable avec ggplot                     
- **"correlations.Rmd"** : génération des matrices de corrélation sur les données totales               
- **"correlations_with_NA_replacement.Rmd"** : génération des matrices de corrélation sur les données avec NA feu transformé en 0
- **"hists_corrplots_fire_rain_canopy_cover_height.Rmd"** : génération des histogrammes de feu, pluie, canopy_cover et canopy_height
- **"premiers_tests_glm.Rmd"** : premiers tests glm 
- **"script01.R"** : premier fichier Dom                                  
- **"tests_lecture_ecoregions.Rmd"** : aggrégation de données écorégions plus précises suite au mail de Le Bien                
- **generate_geojson_files_of_3_ecoregions.Rmd** : génère les fichiers geojson des 3 écorégions dans le folder *geojson_files* pour une lecture qgis       

## Test régressions beta sur des données random

- **"beta_reg_tests.Rmd"** : premiers tests sur la régression beta                               
- **"test_beta_inflated.Rmd"** : tests sur la régression beta inflated   
- **"test_brms1.RDS"** : sauvegarde d'une chaîne MCMC                                
- **"test_brms2.RDS"** : sauvegarde d'une chaîne MCMC     

## Tests brms

- **"tests_brms_3_ecoregions.Rmd"** : premiers essais glm sur les 3 écorégions Guinean forest-savanna, West_Sudanian et Sahelian Acacia   
- **chaines_Guinean_100000_lignes.pdf** : sauvegarde du visuel des chaînes pour les premières 10**5 données de Guinean avec **tests_brms_3_ecoregions.Rmd**          

- **generate_sub_samples_of_3_ecoregions.Rmd** : génère des sous-échantillons randoms 10^4 lignes des 3 écorégions
- **sub_sample_Guinean.RDS** : sous-échantillon random 10^4 lignes de Guinean généré avec **generate_sub_samples_of_3_ecoregions.Rmd**
- **test_car_model.Rmd** : tentative de car() dans brm() sur **sub_sample_10_4_Guinean.RDS**

## A partir du 25 Mars

- **tests_regressions_STAN.Rmd** : tests STAN génériques pas spécifiques à nos données, liés aux fichiers stan dans le folder STAN_models
- **preprocessing_from_raw_data.Rmd** : create *complete_corresponding_table_without_duplicate.RDS* and *complete_corresponding_table_without_duplicate_standardized.RDS* from raw data
- **regressions.Rmd**