#' Function to get token using authorization code
#' @param code
#' @keywords token
#' @export
#' @examples
get_token <- function(code, app, tenant, password, redirect_uri){
  token_url <- sprintf("https://login.microsoftonline.com/%s/oauth2/v2.0/token", tenant)

  req_body <- list(client_id     = app,
                   scope         = "https://graph.microsoft.com/.default",
                   code          = code,
                   redirect_uri  = redirect_uri,
                   grant_type    = "authorization_code",
                   client_secret = password)

  token_response <- POST(token_url, body = req_body, encode = "form")

  if(token_response$status_code == 200){
    content_response     <- content(token_response, type = "application/json")
    tokens$access_token  <- content_response$access_token
    tokens$refresh_token <- content_response$refresh_token
    tokens$expires_at    <- Sys.time() + as.numeric(content_response$expires_in)
    TRUE
  } else {
    FALSE
  }
}
