#' @import magrittr ggplot2 ggthemes showtext sysfonts
#' @importFrom forcats fct_relevel
#' @importFrom quanteda tokens tokens_select dfm dfm_select fcm fcm_select textplot_network
#' @importFrom RColorBrewer brewer.pal
#' @title grafica_wordcloud_all
#' @description Función que grafica una nube de palabras sin hastags ni usuarios
#' @param user corpus que será tokenizado
#' @param corp_tmln corpus que será tokenizado
#' @param folder Folder donde se va  a guardar la imagen
#' @details Genera wordcloud quitando hashtags y usuarios
#' @examples
#' grafica_wordcloud_all('nerudista',corpus,"./03Graficos/")
#' @export


grafica_wordcloud_all <- function(user,corp_tmln, folder){

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

  quanteda::textplot_wordcloud(tweet_dfm,
                     min_size = .8,
                     max_size = 2.5,
                     random_order = FALSE,
                     #color = alpha("#356E40",seq(0.2,1,.23)),
                     color = RColorBrewer::brewer.pal(6, "Reds"),
                     max_words = 100) %>%
    ggsave(filename = paste0( folder,"/all_wordcloud_top100_",user,"_top_100.png"),
           device = "png",
           dpi = 72)
}
