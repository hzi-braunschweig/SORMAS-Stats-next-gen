# This function takes the event data exported by the eventExport function and compute the 
# the location_category (or collectivité) varaible
compute_eventlocation_category= function(eventData){
  # This function takes the event data exported by the eventExport function
  # Add a varible to this data based on type of place and facility
  # These categories are based on France recommendaton. For another country, you just need to edit the code below
  # main issue: https://github.com/hzi-braunschweig/SORMAS-Stats-next-gen/issues/35
  ret =  eventData %>%	#je ne me rappelle plus du nom exact de ta table - à vérifier
    dplyr::mutate(
      location_category = case_when (
        typeofplace_event =='FACILITY' & facilitytype_location =='ASSOCIATION' ~ 'ASSOCIATION',
        typeofplace_event =='FACILITY' & facilitytype_location =='SWIMMING_POOL' ~ 'SWIMMING_POOL', 
        typeofplace_event == 'FACILITY' & facilitytype_location == 'THEATER' ~ 'THEATER',
        typeofplace_event == 'FACILITY' & facilitytype_location %in% c('SCHOOL', "KINDERGARTEN", "AFTER_SCHOOL") ~ 'SCHOOL',
        typeofplace_event == 'FACILITY' & facilitytype_location %in% c('HOSPITAL',  "DAY_HOSPITAL") ~ 'HOSPITAL',
        typeofplace_event == 'FACILITY' & facilitytype_location %in% c('MOBILE_NURSING_SERVICE', "DISABLED_PERSON_HABITATION", "ELDERLY_DAY_CARE", "ELDERLY_CARE_FACILITY" ) ~ 'MO_DIS_ELDER',
        typeofplace_event == 'FACILITY' & facilitytype_location == 'UNIVERSITY' ~ "UNIVERSITY",
        typeofplace_event == 'FACILITY' & facilitytype_location == 'OTHER_MEDICAL_FACILITY' ~ "OTH_MED_FACILITY", 
        typeofplace_event == 'FACILITY' & facilitytype_location %in% c('BUSINESS', 'OTHER_WORKING_PLACE') ~ 'BUSINESS',
        typeofplace_event == 'FACILITY' & facilitytype_location == 'HOTEL' ~ 'HOTEL',
        typeofplace_event == 'FACILITY' & facilitytype_location == 'CANTINE' ~ 'CANTINE',
        typeofplace_event == 'FACILITY' & facilitytype_location == 'RESTAURANT' ~ 'RESTAURANT',
        typeofplace_event == 'FACILITY' & facilitytype_location == 'CHILDRENS_DAY_CARE' ~ 'CHILDRENS_DAY_CARE',
        typeofplace_event == 'FACILITY' & facilitytype_location == 'BAR' ~ 'BAR',
        typeofplace_event == 'FACILITY' & facilitytype_location == 'NIGHT_CLUB' ~ 'NIGHT_CLUB',   
        typeofplace_event == 'FACILITY' & facilitytype_location %in% c('OTHER_RESIDENCE', "HOMELESS_SHELTER", "RETIREMENT_HOME", "REFUGEE_ACCOMMODATION") ~ 'OTHER_RESIDENCE',
        typeofplace_event == 'FACILITY' & facilitytype_location == 'CORRECTIONAL_FACILITY' ~ 'CORRECTIONAL_FACILITY',
        typeofplace_event == 'FACILITY' & facilitytype_location == 'CHILDRENS_HOME' ~ 'CHILDRENS_HOME',
        typeofplace_event == 'MEANS_OF_TRANSPORT'  ~ 'MEANS_OF_TRANSPORT',
        typeofplace_event == 'HOME'  ~ 'HOME',
        typeofplace_event == 'PUBLIC_PLACE'  ~ 'PUBLIC_PLACE',
        TRUE ~ 'UNDEFINED')) %>%  # renaming french categories
    dplyr::mutate(location_category_fr = case_when(
      location_category %in% c("ASSOCIATION", "SWIMMING_POOL") ~  'ACES-Act/Ev.sport', #  #ACES - Activité ou évènement sportif
      location_category == "THEATER" ~  'ACEC-Act/Ev.cult',   #  'ACEC - Activité ou évènement culturel'
      location_category == "SCHOOL" ~ "SCOL-Milieu.scol" ,  #SCOL - Évènement ou situation en milieu scolaire
      location_category == "HOSPITAL" ~ "ESPP-Etab.sanit.", # ESPP – Etablissements de santé publics ou privés
      location_category == "MO_DIS_ELDER" ~ "EMES-Etab.MS" ,
      location_category == "UNIVERSITY"  ~ "UNIV-Ev/Sit.enseig.sup" , # 'UNIV - Évènement ou situation en établissement d_enseignement supérieur'
      location_category == "BUSINESS" ~ "RAEP-EV.act.prof" ,    # 'RAEP - Rassemblement ou évènement en lien avec une activité professionnelle', Can there not be more options here?
      location_category == "HOTEL" ~ "TOUR-Hébergement.tour", # TOUR- Structure d_hébergement touristique
      location_category == "CANTINE" ~ "RCMP-Rest.col.mil.prof)" ,  # RCMP - Restauration collective en milieu  professionnel
      location_category == "RESTAURANT" ~ "REST-Restaurant" ,
      location_category == "CHILDRENS_DAY_CARE" ~ "CRCH-Struct.acc.jeune.enf" ,  #CRCH  - Structure accueil du jeune enfant
      location_category %in% c("BAR", "NIGHT_CLUB") ~ "BARS" ,
      location_category == "OTHER_RESIDENCE" ~ "FROP-Foyer.résid/pens", # FROP - Foyer de résidence ou pensionnat (jeunes, travailleurs) 
      location_category == "CORRECTIONAL_FACILITY" ~ "EPEN-Étab.péni" , # EPEN - Établissement pénitentiaire
      location_category == "CHILDRENS_HOME" ~ "HPJJ-Struct.relevant.PJJ" ,    #HPJJ  -  Structure accueil et hébergement relevant de la PJJ
      location_category == "MEANS_OF_TRANSPORT" ~ "PPTC-transp.col" ,  # PPTC - présence prolongée dans un transport collectif
      location_category == "HOME" ~ "RPFA-Réunions.famil/amic" ,   # RPFA - Réunions privées dans un cadre familial, amical ou autre
      location_category %in% c("PUBLIC_PLACE", "OTH_MED_FACILITY") ~ "AUTR-Autres" ,
      location_category == "UNDEFINED" ~  'INDEFINIE'))
  return(ret)
}