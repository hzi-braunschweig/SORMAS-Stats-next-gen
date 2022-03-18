userExport = function(sormas_db, authenticat_user){
  # This function get the user table in sormas, Hashing Passwords with sodium and export to sormas-stats
  # This username with hashed passwords are then used for authentication in sormas-stats  
  # refs: 
  # https://paulc91.github.io/shinyauthr/
  # https://doc.libsodium.org/password_hashing/
  
  #loading user table

  if(authenticat_user==FALSE){
    # create a dummy user account with ars as user name and sormas-stats as password
    users = data.frame(username = "ars", uuid= "sormas-stats" )
  }else{
    # extracting users from sormas
    queryUsers <- paste0("SELECT uuid, username 
                        FROM public.users
                       WHERE active = TRUE")
    users = dbGetQuery(sormas_db,queryUsers)
  }
  # hash user password
  ret = users %>%
    dplyr::mutate(password = purrr::map_chr(substr(uuid,1,13), sodium::password_store),  .keep = "unused") # hassing
  return(ret)
}
