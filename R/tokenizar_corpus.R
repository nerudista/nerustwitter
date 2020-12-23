#' @import magrittr stopwords
#' @importFrom quanteda tokens tokens_select
#' @title tokenizar_tweets
#' @description Función que tokenizaun corpus y quita stopwords
#' @param corp_tmln corpus que será tokenizado
#' @param stopword_pers lista de stopwords personalizadas para filtrar
#' @details Genera un objeto tokens
#' @examples
#' grafica_frecuencia_palabras(corp_tmln,"covid19","tendencia",20,"./03Graficos/")
#' @export


tokenizar_corpus <- function(corp_tmln,
                             stopwords_pers="")
  {

  stopifnot( is.corpus(corp_tmln)
  )



  # Crear objeto tokens a partir del corpus recibido
  tokens_tmln <-quanteda::tokens(corp_tmln,
                                 remove_punct = TRUE,
                                 remove_symbols = TRUE,
                                 remove_numbers = TRUE,
                                 remove_url = TRUE
  )

  #Quitar stopwords en inglés y español

  # Si hay un vector de palabras , se agrega a los stopwords en español
  ifelse(is.character(stopwords_pers),
         stopwords_pers <-  c( stopwords::stopwords(language = 'es',source = "stopwords-iso"),stopwords_pers),
         stopwords_pers <-  stopwords::stopwords(language = 'es',source = "stopwords-iso")
  )

  tokens_tmln <- quanteda::tokens_select(tokens_tmln, pattern=stopwords_pers ,selection='remove')

  tokens_tmln <- quanteda::tokens_select(tokens_tmln, stopwords::stopwords(language = 'en',source = "stopwords-iso"),selection='remove')

  # quitar emojis
  tokens_tmln <- quanteda::tokens_select(tokens_tmln, valuetype = "glob", pattern = ".U000.*", selection = 'remove')

  return(tokens_tmln)
}
