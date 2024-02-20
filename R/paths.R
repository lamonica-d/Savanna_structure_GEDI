
################################ Chemins Aurélien
if(Sys.info()[['effective_user']] == "Aurélien"){
  
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
      "/home",
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
