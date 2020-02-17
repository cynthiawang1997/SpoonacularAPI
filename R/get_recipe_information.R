#' Get detailed information of multiple recipes.
#'
#' This functions gets detailed information of multiple recipes entered.
#' @param key API key
#' @param recipe_ids The recipe IDs.
#' @return The detailed information of the recipes entered.
#' @examples
#' get_recipe_information(key = Sys.getenv("SPOON_KEY"), recipe_ids = "509163,825127,949313")
#' @export
get_recipe_information <- function(key = NULL,recipe_ids = NULL) {
  if (is.null(key) | is.null(recipe_ids)) {
    return("API key or recipe_ids is missing.")
  }
  querypar_info <- list("apiKey" = key, ids = recipe_ids)  # add more parameters later
  info_url <- "https://api.spoonacular.com/recipes/informationBulk"
  info_data <- httr::GET(paste(info_url), query = querypar_info)
  if (httr::http_status(info_data)$category != "Success") {
    return(httr::http_status(info_data))
  } else {
    info_df <- jsonlite::fromJSON(httr::content(info_data,as = "text"), flatten = TRUE)
    if (is.data.frame(info_df) == FALSE) {
      return("No results found. Please enter new parameters.")
    } else {
      info_df_2 <- info_df %>%
        dplyr::select(recipe_id = id, recipe_name = title, readyInMinutes, instructions, recipe_link = sourceUrl, healthScore, cuisines, dishTypes, servings, vegetarian, vegan, glutenFree, dairyFree, veryHealthy, cheap, veryPopular)
      for (i in 1:nrow(info_df_2)) {
        info_df_2$dishTypes[[i]] <- paste(info_df_2$dishTypes[[i]], collapse = ", ")
      }
      for (i in 1:nrow(info_df_2)) {
        info_df_2$cuisines[[i]] <- paste(info_df_2$cuisines[[i]], collapse = ", ")
      }
      for (i in 1:ncol(info_df_2)) {
        if (is.list((info_df_2[,i]))) {
          info_df_2[,i] <- unlist(info_df_2[,i])
        }
      }
      for (i in 1:nrow(info_df_2)) {
        for (j in 1:ncol(info_df_2)) {
          if (is.na(info_df_2[i,j])) {
            info_df_2[i,j] <- "NA"
          }
        }
      }
      return(info_df_2)
    }
  }
}
