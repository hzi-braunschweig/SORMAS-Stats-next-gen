userExport <- function(sormas_db){
  # This function get the user table in sormas, Hashing Passwords with sodium and export
  # This username with hased passwords are then used for autentification in sormas-stats
  # refs:
  # https://paulc91.github.io/shinyauthr/
  # https://doc.libsodium.org/password_hashing/
  
  #loading user table
  queryUsers <- paste0("SELECT uuid, username, firstname, lastname
                        FROM public.users
                       WHERE active = TRUE")
  users = dbGetQuery(sormas_db,queryUsers)
  
  ret = users %>%
    dplyr::mutate(password = purrr::map_chr(substr(uuid,1,13), sodium::password_store),  .keep = "unused") # hassing
  return(ret)
}

