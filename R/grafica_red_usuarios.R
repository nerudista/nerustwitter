#' @import magrittr ggplot2 ggthemes showtext sysfonts
#' @importFrom forcats fct_relevel
#' @importFrom quanteda tokens tokens_select dfm dfm_select fcm fcm_select textplot_network
#' @title grafica_usuarios
#' @description Función que grafica una red de usuarios
#' @param name nombre del usuario o del trend a analizar
#' @param type el tipo de objeto twitter a analizas. Puede ser "tendencia" o "usuario"
#' @param num_freq Top n de palabras a mostrar
#' @param corp_tmln corpus que será tokenizado
#' @param folder Folder donde se va  a guardar la imagen
#' @param stopword_pers lista de stopwords personalizadas para filtrar
#' @details Genera grafica de barras
#' @examples
#' grafica_usuarios('nerudista',corpus,"./03Graficos/")
#' @export


grafica_usuarios <- function(corp_tmln,
                             name="nombre",
                             type="usuario_tendencia",
                             num_freq = 25,
                             folder=".",
                             stopwords_pers=""){


  stopifnot( is.corpus(corp_tmln)
  )
  # Crear objeto tokens a partir del corpus recibido
  tokens_tmln <- nerustwitter::tokenizar_corpus(corp_tmln = corp_tmln,
                                                stopwords_pers = stopwords_pers)


  # Construir una document  feature matrix a partir del objeto de tokens
  tweet_dfm <- quanteda::dfm(tokens_tmln)
  #head(tweet_dfm)

  # seleccionar hastags para grafica
  tag_dfm <- quanteda::dfm_select(tweet_dfm, pattern = ("@*"))
  #head(tag_dfm)

  toptag <- names(quanteda::topfeatures(tag_dfm, num_freq))
  #head(toptag)

  #Crear feature-occurrence matrix de hashtags
  tag_fcm <- quanteda::fcm(tag_dfm)
  #head(tag_fcm)

  # Crear grafica de hastags
  topgat_fcm <- quanteda::fcm_select(tag_fcm, pattern = toptag)


  quanteda::textplot_network(topgat_fcm,
                             min_freq = 0.1,
                             edge_color = "orange",
                             edge_alpha = 0.7,
                             edge_size = 4) %>%
    ggplot2::ggsave(filename = paste0( folder,"/red_usuarios_",name,".png"),
           device = "png",
           width = 8,
           height = 5,
           dpi = 72)



}
