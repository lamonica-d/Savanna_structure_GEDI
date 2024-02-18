
options(Encoding="latin1")
# encoding en latin1 pour l'accent de Aur?lien

################################ Chemins Aur?lien
if(Sys.info()[['effective_user']] == "Aur?lien"){
  
  path_to_Documents = file.path(
    "C:",
    "Users",
    "Lyz50",
    "Documents"
    )
  
  path_to_Github_folder = file.path(
    path_to_Documents,
    "GitHub"
  )
  
  path_to_Forest_savanna_project_folder = file.path(
    path_to_Github_folder,
    "Forest_savanna_project"
  )

  path_to_Savanna_structure_GEDI_folder = file.path(
    path_to_Github_folder,
    "Savanna_structure_GEDI"
  )
  
  path_to_GEDI_raw_data = file.path(
    path_to_Documents,
    "raw_GEDI_data"
  )

}

############################### Chemins Dominique
if(Sys.info()[['effective_user']] == "lamonica"){

    path_to_Documents = file.path(
      "home",
      "lamonica",
      "Documents"
    )

    path_to_Forest_savanna_project_folder = file.path(
      path_to_Documents,
      "dessfor",
      "stage_aurelien_thiriet",
      "Forest_savanna_project"
    )
    
    path_to_Savanna_structure_GEDI_folder = file.path(
      path_to_Documents,
      "dessfor",
      "gedi_data_lebien",
      "Savanna_structure_GEDI"
    )
    
    path_to_GEDI_raw_data = file.path(
      path_to_Documents,
      "dessfor",
      "gedi_data_lebien",
      "pix_extract"
    )
}