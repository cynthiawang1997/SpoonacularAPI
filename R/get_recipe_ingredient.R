#' Get the ingredients required in recipes.
#'
#' This functions gets ingredients needed for one recipe.
#' @param key API key
#' @param recipe_id The recipe ID.
#' @return The ingredients for the recipe entered.
#' @examples
#' get_recipe_ingredient(key = Sys.getenv("SPOON_KEY"), recipe_id = "753644")
#' @export
get_recipe_ingredient <- function(key = NULL, recipe_id = NULL) {
  if (is.null(key) | is.null(recipe_id)) {
    return("API key or recipe_id is missing.")
  }
  querypar_ing <- list("apiKey" = key)
  ing_data <- httr::GET(paste("https://api.spoonacular.com/recipes/",recipe_id,"/ingredientWidget.json",sep = ""), query = querypar_ing)
  if (httr::http_status(ing_data)$category != "Success") {
    return(httr::http_status(ing_data))
  } else {
    ing_df <- jsonlite::fromJSON(httr::content(ing_data,as = "text"), flatten = TRUE)[[1]]
    if (is.data.frame(ing_df) == FALSE) {
      return("No results found. Please enter new parameters.")
    } else {
      ing_df$amount_metric <- paste(ing_df$amount.metric.value,ing_df$amount.metric.unit)
      ing_df$amount_us <- paste(ing_df$amount.us.value,ing_df$amount.us.unit)
      ing_df_2 <- ing_df %>% dplyr::select(ingredient = name,amount_metric,amount_us)
      return(ing_df_2)
    }
  }
}
