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

#' @title Management API: Delete users permissions
#' @description delete_users_permissions() function gets an emails list as an input and removes access for each email from Google Analytics accounts on every hierarchy level.
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
    
    if(inspect_permissions(acc_list, i, ga_level))
    { 
      acc_user_list <- ga_users_list(id, webPropertyId = NULL, viewId = NULL)
      
      for(email in emails_to_delete)
      {
        if(email %in% acc_user_list$items$userRef$email)
        { 
          cat(email, "has access to", acc_list$items[i, "name"], "(Account)", "\n")
          
          if(delete_user_from_account(id, email))
            cat(email, "has been removed from account", acc_list$items[i, "name"], "\n")
          
        }
      }
    }
    webproperty_list <- ga_webproperty_list(id)
    
    for(j in 1:nrow(webproperty_list$items))
    {
      ga_level <- "\tWebproperty"
      web_property_id <- webproperty_list$items[j, "id"]
      
      if(inspect_permissions(webproperty_list, j, ga_level))
      {
        prop_user_list <- ga_users_list(id, web_property_id, viewId = NULL)
        
        for(email in emails_to_delete)
        {
          if(email %in% prop_user_list$items$userRef$email)
          { 
            cat("\t", email, "has access to", webproperty_list$items[j, "name"], "(Webproperty)", "\n")
            
            if(delete_user_from_webproperty(id, web_property_id, email))
              cat("\t", email, "has been removed from Webproperty", webproperty_list$items[j, "name"], "\n")
            
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
          
          for(email in emails_to_delete)
          {
            if(email %in% view_user_list$items$userRef$email)
            { 
              cat("\t\t", email, "has access to", view_list$items[k, "name"], "(View)", "\n")
              
              if(delete_user_from_view(id, web_property_id, view_id, email))
                cat("\t\t", email, "has been removed from View", view_list$items[k, "name"], "\n")
              
            }
          }
        }
      }
    }
  }
}