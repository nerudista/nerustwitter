#' @import magrittr ggplot2 ggthemes showtext sysfonts
#' @importFrom forcats fct_relevel
#' @importFrom quanteda tokens tokens_select dfm dfm_select fcm fcm_select textplot_network
#' @importFrom RColorBrewer brewer.pal
#' @title grafica_wordcloud_all
#' @description Función que grafica una nube de palabras sin hastags ni usuarios
#' @param name nombre del usuario o del trend a analizar
#' @param type el tipo de objeto twitter a analizas. Puede ser "tendencia" o "usuario"
#' @param num_freq Top n de palabras a mostrar
#' @param corp_tmln corpus que será tokenizado
#' @param folder Folder donde se va  a guardar la imagen
#' @param stopword_pers lista de stopwords personalizadas para filtrar
#' @details Genera wordcloud quitando hashtags y usuarios
#' @examples
#' grafica_wordcloud_all('nerudista',corpus,"./03Graficos/")
#' @export


grafica_wordcloud_all <- function(corp_tmln,
                                  name="nombre",
                                  type="usuario_tendencia",
                                  num_freq = 100,
                                  folder=".",
                                  stopwords_pers=""){

  stopifnot( is.corpus(corp_tmln)
  )

  # Crear objeto tokens a partir del corpus recibido
  tokens_tmln <- nerustwitter::tokenizar_corpus(corp_tmln = corp_tmln,
                                                stopwords_pers = stopwords_pers)

  # Construir una document  feature matrix a partir del objeto de tokens
  tweet_dfm <- quanteda::dfm(tokens_tmln)


  # Hay que guardar como plot d eR base porque el wordcloud no e funciona como ggplot
  # https://stackoverflow.com/questions/46499719/error-in-using-heatmap-as-the-plot-input-of-ggsave

  png(file=paste0( folder,"/all_wordcloud_top",num_freq,"_",type,"_",name,".png"),
      width=600,height=600, units='px', res=120)

  quanteda::textplot_wordcloud(tweet_dfm,
                     min_size = .8,
                     max_size = 3,
                     random_order = FALSE,
                     #color = alpha("#356E40",seq(0.2,1,.23)),
                     color = RColorBrewer::brewer.pal(6, "Reds"),
                     max_words = num_freq)
  dev.off()

  print("Grafica guardada")
}
