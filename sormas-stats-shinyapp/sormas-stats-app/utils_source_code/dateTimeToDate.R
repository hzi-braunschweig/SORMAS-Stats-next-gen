
# Converting a datetime object to date object
dateTimeToDate = function(x)
{
  temp1 = substr(x,1,10) # cut first 10 characters in the string
  temp1[temp1==""]=NA  # replace empty substring with NA
  return(as.Date(temp1)) # convert substring to R date 
}
