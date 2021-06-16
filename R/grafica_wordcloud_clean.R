#' @import magrittr ggplot2 ggthemes showtext sysfonts
#' @importFrom forcats fct_relevel
#' @importFrom quanteda tokens tokens_select dfm dfm_select fcm fcm_select
#' @importFrom quanteda.textplots textplot_network
#' @importFrom RColorBrewer brewer.pal
#' @title grafica_wordcloud_clean
#' @description Función que grafica una nube de palabras sin hashtags ni nombres de usuarios
#' @param name Nombre del usuario o del trend a analizar
#' @param type El tipo de objeto twitter a analizar. Puede ser "tendencia" o "usuario". Otros valores pueden causar error
#' @param num_freq Número de palabras a mostrar.
#' @param corp_tmln corpus que será tokenizado
#' @param stopword_pers Vector de stopwords personalizadas para filtrar
#' @details Genera grafica de nube de palabras sin hastags ni nombres de usuarios
#' @examples
#' ## Generar corpus
#' corpus_tweets <- df_tweets %>% select(text) %>% corpus()
#'
#' ## Generar gráfica
#' grafica_wordcloud_clean(corpus_tweets,"covid19","tendencia",20)
#' @usage
#' ## grafica_wordcloud_clean(
#' corp_tmln=corpus,
#' name="nombre",
#' type="usuario_tendencia",
#' num_freq = 10,
#' stopwords_pers=c("una","palabra","otra")
#' )
#' @export


grafica_wordcloud_clean <- function(corp_tmln,
                                    name="nombre",
                                    type="usuario_tendencia",
                                    num_freq = 100,
                                    stopwords_pers=""){

  stopifnot( is.corpus(corp_tmln)
  )

  # Crear objeto tokens a partir del corpus recibido
  tokens_tmln <- nerustwitter::tokenizar_corpus(corp_tmln = corp_tmln,
                                                stopwords_pers = stopwords_pers)

  # Construir una document  feature matrix a partir del objeto de tokens
  tweet_dfm <- quanteda::dfm(tokens_tmln)


  # eliminar hasstags y usuarios hastags para grafica
  words_dfm <- quanteda::dfm_select(tweet_dfm, pattern = ("#*"), selection = 'remove')

  words_dfm <- quanteda::dfm_select(words_dfm, pattern = ("@*"), selection = 'remove')


  print("dfm creado")

  # to work well with the RStudio graphics device (RStudioGD).
  # showtext::showtext_opts(dpi = 96)
  # showtext::showtext_auto()



  # Grafica sin hastags ni usuarios
  # Hay que guardar como plot d eR base porque el wordcloud no e funciona como ggplot
  # https://stackoverflow.com/questions/46499719/error-in-using-heatmap-as-the-plot-input-of-ggsave

  # png(file=paste0( folder,"/clean_wordcloud_top",num_freq,"_",type,"_",name,".png"),
  #     width=600,height=600, units='px', res=120)


  quanteda.textplots::textplot_wordcloud( words_dfm,
                     min_size = .8,
                     max_size = 3,
                     random_order = FALSE,
                     color = RColorBrewer::brewer.pal(7, "Blues"),
                     max_words = num_freq)

  # dev.off()
  print("Grafica creada")

}
