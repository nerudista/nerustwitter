#' @import magrittr ggplot2 ggthemes showtext sysfonts
#' @importFrom forcats fct_relevel
#' @importFrom quanteda tokens tokens_select dfm dfm_select fcm fcm_select textplot_network
#' @title grafica_usuarios
#' @description Función que grafica una red de usuarios
#' @param user corpus que será tokenizado
#' @param corp_tmln corpus que será tokenizado
#' @param folder Folder donde se va  a guardar la imagen
#' @details Genera grafica de barras
#' @examples
#' grafica_usuarios('nerudista',corpus,"./03Graficos/")
#' @export


grafica_usuarios <- function(user,corp_tmln, folder){

  tokens_tmln <-quanteda::tokens(corp_tmln,
                        remove_punct = TRUE,
                        remove_symbols = TRUE,
                        remove_numbers = TRUE,
                        remove_url = TRUE
  )

  tokens_tmln <- quanteda::tokens_select(tokens_tmln, stopwords('es'),selection='remove')

  tokens_tmln <- quanteda::tokens_select(tokens_tmln, stopwords('en'),selection='remove')

  tokens_tmln <- quanteda::tokens_select(tokens_tmln, valuetype = "glob", pattern = ".U000.*", selection = 'remove')


  # Construir una document  feature matrix a partir del objeto de tokens
  tweet_dfm <- quanteda::dfm(tokens_tmln)
  #head(tweet_dfm)

  # seleccionar hastags para grafica
  tag_dfm <- quanteda::dfm_select(tweet_dfm, pattern = ("@*"))
  #head(tag_dfm)

  toptag <- names(quanteda::topfeatures(tag_dfm, 30))
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
    ggplot2::ggsave(filename = paste0( folder,"/red_usuarios_",user,".png"),
           device = "png",
           dpi = 72)



}
