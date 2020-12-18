#' @import magrittr
#' @title leer_usuario_rtweet
#' @description Esta es una funcion de ejemplo para multiplicar un valor o vector \code{x} por dos.
#' @param user Usuario del que se leerá el timeline de Twitter
#' @param num Número de tuits a leer del timeline
#' @details Esta funcion lee el timeline y calcula si el contenido es propio o si es retweet
#' @examples
#' leer_usuario_rtweet('nerudista',3)
#' @export



# Declarar funcion para obetner timeline y  calcular tipo de contenido
leer_usuario_rtweet <- function(user,num){

  df <- rtweet::get_timeline(user,n=num)

  # Revisar contenido propio o retweet

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
