#' function to inspect permissions
#' @export
inspect_permissions <- function(x, n, ga_level){
  
  id <- x$items[n, "id"]
  name <- x$items[n, "name"]
  permissions <- x$items$permissions[n, "effective"]
  
  if(grepl(".*(MANAGE_USERS).*", permissions))
    return(TRUE)
  else
    
  {
    cat(ga_level, name, " - ", id, " - You don't have enough permissions to view or remove users.", "\n")
    return(FALSE)
  }
}

#' function to delete user from account
#' @export
delete_user_from_account <- function(acc_id, email_to_delete){
  
  acc_user_list <- ga_users_list(acc_id, webPropertyId = NULL, viewId = NULL)
  userlink_to_delete <- ""
  
  for(i in 1:nrow(acc_user_list$items))
  {
    if(email_to_delete == acc_user_list$items$userRef[i, "email"])
    {
      userlink_to_delete <- acc_user_list$items[i, "id"]
      break()
    }
  }
  
  if(userlink_to_delete == "")
  {
    message("The user does not have access to this account.")
    return(FALSE)
  }
  
  url <- sprintf("https://www.googleapis.com/analytics/v3/management/accounts/%s/entityUserLinks/%s", acc_id, userlink_to_delete)
  
  del <- gar_api_generator(url, "DELETE")
  
  tryCatch(
    {
      del()
    },
    warning=function(cond) {
    },
    error = function(cond){
      message(cond)
      return(FALSE)
    })
  
  return(TRUE)
}

#' function to delete user from webproperty
#' @export
delete_user_from_webproperty <- function(acc_id, webproperty_id, email_to_delete){
  
  webproperty_user_list <- ga_users_list(acc_id, webproperty_id, viewId = NULL)
  userlink_to_delete <- ""
  
  for(i in 1:nrow(webproperty_user_list$items))
  {
    if(email_to_delete == webproperty_user_list$items$userRef[i, "email"])
    {
      userlink_to_delete <- webproperty_user_list$items[i, "id"]
      break()
    }
  }
  if(userlink_to_delete == "")
  {
    message("The user does not have access to this webproperty.")
    return(FALSE)
  }
  url <- sprintf("https://www.googleapis.com/analytics/v3/management/accounts/%s/webproperties/%s/entityUserLinks/%s",
                 acc_id, webproperty_id, userlink_to_delete)
  
  del <- gar_api_generator(url,
                           "DELETE")
  tryCatch(
    {
      del()
    },
    warning=function(cond) {
    },
    error = function(cond){
      message(cond)
      return(FALSE)
    })
  return(TRUE)
}

#' function to delete user from view
#' @export
delete_user_from_view <- function(acc_id, webproperty_id, view_id, email_to_delete){
  
  view_user_list <- ga_users_list(acc_id, webproperty_id, view_id)
  userlink_to_delete <- ""
  
  for(i in 1:nrow(view_user_list$items))
  {
    if(email_to_delete == view_user_list$items$userRef[i, "email"])
    {
      userlink_to_delete <- view_user_list$items[i, "id"]
      break()
    }
  }
  if(userlink_to_delete == "")
  {
    message("The user does not have access to this view.")
    return(FALSE)
  }
  url <- sprintf("https://www.googleapis.com/analytics/v3/management/accounts/%s/webproperties/%s/profiles/%s/entityUserLinks/%s",
                 acc_id, webproperty_id, view_id, userlink_to_delete)
  
  del <- gar_api_generator(url,
                           "DELETE")
  tryCatch(
    {
      del()
    },
    warning=function(cond) {
    },
    error= function(cond){
      message(cond)
      return(FALSE)
    })
  return(TRUE)
}

check_permissions <- function(permissions_effective, permissions_local) {
  read_and_analyze <- TRUE
  if (grepl(".*(READ_AND_ANALYZE).*", permissions_effective)) {
    read_and_analyze <- (grepl(".*(READ_AND_ANALYZE).*", permissions_local) || grepl(".*(COLLABORATE).*", permissions_local) || grepl(".*(EDIT).*", permissions_local))
  }
  
  collaborate <- TRUE
  if (grepl(".*(COLLABORATE).*", permissions_effective)) {
    collaborate <- (grepl(".*(COLLABORATE).*", permissions_local) || grepl(".*(EDIT).*", permissions_local))
  }
  
  edit <- TRUE
  if (grepl(".*(EDIT).*", permissions_effective)) {
    edit <- grepl(".*(EDIT).*", permissions_local)
  }
  
  manage <- TRUE
  if (grepl(".*(MANAGE_USERS).*", permissions_effective)) {
    manage <- grepl(".*(MANAGE_USERS).*", permissions_local)
  }
  
  return (read_and_analyze && collaborate && edit && manage)
}

#' @title Management API: Delete users permissions
#' @description function gets an emails list as an input and removes access for each email from Google Analytics accounts on every hierarchy level.
#'
#' @param emails_to_delete List of emails
#'
#' @export
delete_users_permissions <- function(emails_to_delete){
  
  acc_list <- ga_accounts()
  
  for(i in 1:nrow(acc_list$items))
  {
    id <- acc_list$items[i, "id"]
    ga_level <- "Account"
    
    inspect_account_response <- inspect_permissions(acc_list, i, ga_level)
    if(inspect_account_response)
    { 
      acc_user_list <- ga_users_list(id, webPropertyId = NULL, viewId = NULL)
      
      for(email in emails_to_delete)
      {
        if(email %in% acc_user_list$items$userRef$email)
        {
          cat(email, "has access to", acc_list$items[i, "name"], "(Account)", "\n")
         
          if(delete_user_from_account(id, email)) {
            cat(email, "has been removed from account", acc_list$items[i, "name"], "\n")
          }
        }
      }
    }
    webproperty_list <- ga_webproperty_list(id)
    
    for(j in 1:nrow(webproperty_list$items))
    {
      ga_level <- "\tWebproperty"
      web_property_id <- webproperty_list$items[j, "id"]
      emailsResponse <- list()
      
      inspect_webproperty_response <- inspect_permissions(webproperty_list, j, ga_level)
      if(inspect_webproperty_response)
      {
        prop_user_list <- ga_users_list(id, web_property_id, viewId = NULL)
        
        count <- 1;
        for(email in emails_to_delete)
        {
          for(index in 1:nrow(prop_user_list$items))
          {
            if(email == prop_user_list$items$userRef[index, "email"])
            {
              cat("\t", email, "has access to", webproperty_list$items[j, "name"], "(Webproperty)", "\n")
              permissions_effective <- prop_user_list$items$permissions[index, "effective"]
              permissions_local <- prop_user_list$items$permissions[index, "local"]
              response <- check_permissions(permissions_effective, permissions_local)
              emailsResponse[[ count ]] <- response
              count <- count + 1;
              
              if (inspect_account_response == FALSE && response == FALSE) {
                cat("\t\t", "You don't have enough permissions to remove users from", webproperty_list$items[j, "name"], "- inherited permissions", "\n")
              } else if(delete_user_from_webproperty(id, web_property_id, email)) {
                cat("\t", email, "has been removed from Webproperty", webproperty_list$items[j, "name"], "\n")
              }
              break()
            }
          }
        }
      }
      
      view_list <- ga_view_list(id, web_property_id)
      
      for(k in 1:nrow(view_list$items))
      {
        ga_level <- "\t\tView"
        view_id <- view_list$items[k, "id"]
        
        if(inspect_permissions(view_list, k, ga_level))
        {
          view_user_list <- ga_users_list(id, web_property_id, view_id)
          
          count <- 1
          for(email in emails_to_delete)
          {
            for(index in 1:nrow(view_user_list$items))
            {
              if(email == view_user_list$items$userRef[index, "email"])
              {
                cat("\t\t", email, "has access to", view_list$items[k, "name"], "(View)", "\n")
                permissions_effective <- view_user_list$items$permissions[index, "effective"]
                permissions_local <- view_user_list$items$permissions[index, "local"]
                response <- check_permissions(permissions_effective, permissions_local)
                
                if (inspect_account_response == FALSE && inspect_webproperty_response == FALSE && response == FALSE) {
                  cat("\t\t", "You don't have enough permissions to remove users from", view_list$items[k, "name"], "- inherited permissions", "\n")
                } else if (inspect_account_response == FALSE && inspect_webproperty_response == TRUE && emailsResponse[[ count ]] == FALSE) {
                  cat("\t\t", "You don't have enough permissions to remove users from", view_list$items[k, "name"], "- inherited permissions", "\n")
                  count <- count + 1
                } else if(delete_user_from_view(id, web_property_id, view_id, email)) {
                  cat("\t\t", email, "has been removed from View", view_list$items[k, "name"], "\n")
                }
                
                break()
              }
            }
          }
        }
      }
    }
  }
}