# Folder qui contient les scripts
                     
- **paths.R** : fichier source qu'on charge dans tous les autres fichiers pour remplir automatiquement les chemins vers les données, les figures etc, selon l'utilisateur

- **preprocessing_from_raw_data.Rmd** : crée *complete_corresponding_table_without_duplicate.RDS* et *complete_corresponding_table_without_duplicate_standardized.RDS* à partir des données de Le Bien

- **sub_sampling_grid.RMD** :  crée les fichiers .RDS pour les régressions et les fichiers .geojson pour vérifier si l'échantillonnage est correct, et pour avoir des visualisations globales. Witold me disait qu'il existe une meilleure méthode pour avoir ce sampling à 10km (un peu compliqué mais moins que ce qu'on a fait avec le code R) Enregistrés dans le folder transformed_data.

- **complete_model_jags.Rmd** : code qui appelle le fichier **model.txt** dans **JAGS_models** et génère dans un fichier dans JAGS_outputs les sorties textuelles, les corrélations, les graphiques des sorties, les graphiques des resimulations etc

- **hierarchical_model_jags.Rmd** : code qui appelle le fichier **modele_hierarchique.txt** dans **JAGS_models** et génère dans un fichier dans JAGS_outputs les sorties textuelles, les corrélations, les graphiques des sorties selon la localisation à 300km

- **tentative_intersection.Rmd** : fichier qui permet de conserver uniquement les points dans les zones peu densément peuplées (<10 hab/km2) en réalisant l'intersection d'un raster et d'une couche vectorielle (il faut passer par QGIS au milieu mais c'est expliqué dedans). En fait on rajoute l'information de la densité de population à chaque point de la couche vectorielle et on réalise la condition <10 hab/km2 avec le code R une fois qu'on recharge le fichier QGIS.

Scripts R appelant des fonctions brms pour faire du glm, que j'avais finalement splittés :

- **script_propre_beta_canopy_cover.R**
- **script_propre_beta_feu_en_fonction_pluie.R**
- **script_propre_gamma_rh98.R**
- **script_propre_glmm.R**

scripts assez simples par rapport aux longues boucles avec resimulation et compagnie que j'ai tout de même laissées dans le le folder **regression** (voir plus bas)

# Autres

**complete_model_STAN** : code qui appelle le code stan du modèle dans le folder **STAN_models**. (pas à jour par rapport à la version finale en JAGS) mais qui peut donner éventuellement une base si on souhaite estimer le modèle avec STAN.

Je l'avais codé à la fois en JAGS et en STAN à un moment dans la continuité des tps d'écotox aussi pour apprendre simultanément les deux langages et appréhender les différences.

# 
il y a d'autres régressions que j'avais faites dans le folder **regression**, avec une boucle sur les écorégions, avec les resimulations...
j'avais finalement splitté le code en différents fichiers afin d'avoir un seul type de régression par fichier

ça peut éventuellement donner une base si on reprend les glms...
ou pas

mais les régressions principales sont dans le folder R :
- R/script_propre_beta_canopy_cover.R
- R/script_propre_beta_feu_en_fonction_pluie.R
- R/script_propre_gamma_rh98.R
- R/script_propre_glmm.R

# 

**generate_ilots_data.R** : code naïf censé générer les îlots dans le folder **transformed_data_ilots** dans le folder principal. Je crois qu'il ne marche pas.

# 

**old_scripts** contient trois vieux scripts que j'ai resortis des **archives**. Je crois que **generate_hists_and_correlations**.Rmd génère les histogrammes et matrices de corrélations dans le folder figures, que **generate_sub_samples_of_3_ecoregions**.Rmd génère des échantillons 10^4 de couches vectorielles afin de pouvoir faire des visualisations QGIS, et que **visualisation_donnes_manquantes.Rmd** donne des summary de NA.

Il est possible que des figures dans le folder figure aient été faites avec des vieux codes que j'ai mis dans le folder archive.

