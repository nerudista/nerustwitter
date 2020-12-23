#' @import magrittr
#' @importFrom dplyr mutate case_when
#' @title calcula_contenido_propio
#' @description Esta es una funcion pra calcular si el contenido es propio o retweet o quote
#' @param df Dataframe con columnas: is_quote
#' @details Esta funcion calcula si el contenido es propio o si es retweet
#' @examples
#' calcula_contenido_propio(df_tweets_from_rtweet)
#' @export



# Declarar funcion para obetner timeline y  calcular tipo de contenido
calcula_contenido_propio <- function(df){

  #df$contenido_propio <-
  df <- df %>%
    dplyr::mutate( contenido_propio = dplyr::case_when(is_quote == "FALSE" & is_retweet == "FALSE" & is.na( reply_to_screen_name) ~ "Contenido Propio" ,
                                                       is_quote == "TRUE" ~ "Citar Tweet",
                                                       is_retweet == "TRUE" ~ "Retweet",
                                                       !is.na(reply_to_screen_name) ~ "Respuesta a Tweet"
    )
    )

  return( df)
}
