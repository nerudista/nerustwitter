#' @import magrittr ggplot2 ggthemes showtext sysfonts
#' @importFrom forcats fct_relevel
#' @importFrom quanteda tokens tokens_select dfm dfm_select fcm fcm_select
#' @importFrom quanteda.textplots textplot_network
#' @title grafica_hashtags
#' @description Función que grafica una red de hastags a partir de un corpus recibido.
#' @param num_freq Número de palabras a mostrar.
#' @param corp_tmln corpus que será tokenizado
#' @param stopword_pers Vector de stopwords personalizadas para filtrar
#' @details Genera grafica red con los hashtags más representativos del corpus recibido.
#' @examples
#' ## Generar corpus
#' corpus_tweets <- df_tweets %>% select(text) %>% corpus()
#'
#' ## Generar gráfica
#' grafica_hashtags(corpus_tweets,"covid19","tendencia",20)
#'
#' ## Para guardar
#' png(file=paste0( folder,"/all_wordcloud_top.png"),width=600,height=600, units='px', res=120)
#' grafica_hashtags(corpus_tweets,"covid19","tendencia",20)
#' dev.off()
#'
#'
#' @usage
#' ## grafica_hashtags(
#' corp_tmln=corpus,
#' num_freq = 10,
#' stopwords_pers=c("una","palabra","otra")
#' )
#' @export


grafica_hashtags <- function(corp_tmln,
                             num_freq = 25,
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
  tag_dfm <- quanteda::dfm_select(tweet_dfm, pattern = ("#*"))
  #head(tag_dfm)

  toptag <- names(quanteda::topfeatures(tag_dfm, num_freq))
  #head(toptag)

  #Crear feature-occurrence matrix de hashtags
  tag_fcm <- quanteda::fcm(tag_dfm)
  #head(tag_fcm)

  # Crear grafica de hastags
  topgat_fcm <- quanteda::fcm_select(tag_fcm, pattern = toptag)


  plot <- quanteda.textplots::textplot_network(topgat_fcm, min_freq = 0.1, edge_alpha = 0.7, edge_size = 4)

    # ggplot2::ggsave(filename = paste0( folder,"/red_hastags_",name,".png"),
    #        device = "png",
    #        width = 8,
    #        height = 5,
    #        dpi = 72)

    print("Grafica creada")

  return(plot)



}
