#' @import magrittr ggplot2 ggthemes showtext sysfonts
#' @importFrom forcats fct_relevel
#' @importFrom quanteda tokens tokens_select dfm dfm_select fcm fcm_select textplot_network
#' @importFrom RColorBrewer brewer.pal
#' @title grafica_wordcloud_clean
#' @description Función que grafica una nube de palabras sin hastags ni usuarios
#' @param user corpus que será tokenizado
#' @param corp_tmln corpus que será tokenizado
#' @param folder Folder donde se va  a guardar la imagen
#' @details Genera wordcloud quitando hashtags y usuarios
#' @examples
#' grafica_wordcloud_clean('nerudista',corpus,"./03Graficos/")
#' @export


grafica_wordcloud_clean <- function(user,corp_tmln, folder){

  tokens_tmln <-quanteda::tokens(corp_tmln,
                                 remove_punct = TRUE,
                                 remove_symbols = TRUE,
                                 remove_numbers = TRUE,
                                 remove_url = TRUE
  )

  tokens_tmln <- quanteda::tokens_select(tokens_tmln, stopwords('es'),selection='remove')

  tokens_tmln <- quanteda::tokens_select(tokens_tmln, stopwords('en'),selection='remove')

  # quitar emojis
  tokens_tmln <- quanteda::tokens_select(tokens_tmln, valuetype = "glob", pattern = ".U000.*", selection = 'remove')


  # Construir una document  feature matrix a partir del objeto de tokens
  tweet_dfm <- quanteda::dfm(tokens_tmln)


  # eliminar hasstags y usuarios hastags para grafica
  words_dfm <- quanteda::dfm_select(tweet_dfm, pattern = ("#*"), selection = 'remove')

  words_dfm <- quanteda::dfm_select(words_dfm, pattern = ("@*"), selection = 'remove')


  print("dfm creado")

  # Grafica sin hastags ni usuarios
  quanteda::textplot_wordcloud(words_dfm,
                     min_size = .5,
                     max_size = 4,
                     random_order = FALSE,
                     color = RColorBrewer::brewer.pal(7, "Blues"),
                     max_words = 100) %>%
    ggplot2::ggsave(filename = paste0( folder,"/clean_wordcloud_top100_",user,".png"),
                    device = "png",
                    dpi = 72)

  print("Imagen guardada")
}
