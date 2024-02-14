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
- **chaines_Guinean_100000_lignes.pdf** : sauvegarde du visuel des chaînes pour les premières 10**5 données de Guinean avec tests_brms_3_ecoregions.Rmd             
- **test_brms_on_sub_sample_10_4_Guinean** : la suite mais sur 10^4 données au lieu de 10^5, et sur un sample homogène dans l'espace