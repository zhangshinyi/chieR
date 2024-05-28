#' Function to fetch user details
#' @param access_token 
#' @keywords token, user, email
fetch_user_email <- function(access_token){
  req <- GET(url = "https://graph.microsoft.com/v1.0/me",
             add_headers(Authorization = paste("Bearer", access_token)),
             content_type_json()
  )
  if(req$status_code == 200){
    user_profile <- content(req)
    user_email   <- if(!is.null(user_profile$mail)) user_profile$mail else "Not Available"
    # user_name    <- if(!is.null(user_profile$displayName)) user_profile$displayName else "Not Available"
    # paste("Name:", user_name, "Email:", user_email)
    user_email
  } else {
    paste("Error fetching user details: HTTP status", req$status_code)
  }
}