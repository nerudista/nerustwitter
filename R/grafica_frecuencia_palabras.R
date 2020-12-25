#' @import magrittr ggthemes showtext sysfonts stopwords
#' @importFrom forcats fct_relevel
#' @importFrom ggplot2 ggplot geom_bar theme labs
#' @importFrom quanteda tokens tokens_select dfm dfm_select textstat_frequency ndoc
#' @importFrom RColorBrewer brewer.pal
#' @importFrom dplyr case_when
#' @title grafica_frecuencia_palabras
#' @description Función que genera una gráfica de barras con la frecuencia de palabras usadas en el corpus.
#' @param name Nombre del usuario o del trend a analizar
#' @param type El tipo de objeto twitter a analizar. Puede ser "tendencia" o "usuario". Otros valores causan error.
#' @param num_freq Top n de palabras a mostrar
#' @param corp_tmln Corpus que será tokenizado
#' @param stopword_pers Vector de  stopwords personalizadas para filtrar
#' @details Genera wordcloud quitando hashtags y usuarios
#' @examples
#' ## Generar corpus
#' corpus_tweets <- df_tweets %>% select(text) %>% corpus()
#'
#' ## Generar gráfica
#' plot <- grafica_frecuencia_palabras(corpus_tweets,"covid19","tendencia",20)
#'
#' ## #guardar grafica
#' ggplot2::ggsave(
#'   filename = "frecuencia_top_20.png",
#'   plot = plot,
#'   width = 8,
#'   height = 5,
#'   device = "png",
#'   dpi =96
#' )
#'
#' @usage
#' ## Generar grafica
#' plot <- grafica_frecuencia_palabras(
#' corp_tmln=corpus,
#' name="nombre",
#' type="usuario_tendencia",
#' num_freq = 10,
#' stopwords_pers=c("una","palabra","otra")
#' )
#'
#'
#' @export


grafica_frecuencia_palabras <- function(corp_tmln,
                                        name="nombre",
                                        type="usuario_tendencia",
                                        num_freq = 10,
                                        stopwords_pers=""){

  stopifnot( is.corpus(corp_tmln)
             )

  #cargar fuente de google
  sysfonts::font_add_google("Nunito","nunito")

  # Calcular num tweets en el corpus
  ntweets <- quanteda::ndoc(corp_tmln)

  # Crear objeto tokens a partir del corpus recibido
  tokens_tmln <- nerustwitter::tokenizar_corpus(corp_tmln = corp_tmln,
                                                stopwords_pers = stopwords_pers)

  # Construir una document  feature matrix a partir del objeto de tokens
  tweet_dfm <- quanteda::dfm(tokens_tmln)

  # eliminar hasstags y usuarios hastags para grafica
  tweet_dfm <- quanteda::dfm_select(tweet_dfm, pattern = ("#*"), selection = 'remove')

  tweet_dfm <- quanteda::dfm_select(tweet_dfm, pattern = ("@*"), selection = 'remove')

  # sacar frecuencia
  tstat_freq <- quanteda::textstat_frequency(tweet_dfm, n = num_freq)

  # to work well with the RStudio graphics device (RStudioGD).
  showtext_opts(dpi = 96)
  showtext::showtext_auto()

  mi_titulo <- dplyr::case_when( tolower(type) == 'tendencia' ~  paste0("Palabras más usadas en la ",type," #",name),
                                 tolower(type) == 'usuario' ~   paste0("Palabras más usadas del ",type," @",name),
                                 TRUE ~   paste0("Palabras más usadas para el usuario/tendencia ",name)
  )

  # graficar
  plot <- ggplot2::ggplot(data = tstat_freq,
                          aes(y = reorder(feature, frequency),
                              x = frequency)
  )  +
    ggplot2::geom_bar(stat = "identity",
                      fill = "#0C6583") +
    ggplot2::labs(title= mi_titulo,
                  subtitle = paste0("Últimos ",ntweets, " tweets" ),
                  y = NULL,
                  x = "Número de Tweets",
                  caption = "Gráfica realizada por @nerudista con datos del paquete rtweet")+
    ggthemes::scale_fill_economist()+
    ggplot2::theme( legend.position = "none",
                    plot.title = element_text(size=18,
                                              face = "bold",
                                              margin = margin(0,0,5,0) #dar espacio entre title y subtitle)
                    ),
                    plot.subtitle = element_text(size=13, face = "italic",
                                                 margin = margin(0,0,8,0) #dar espacio entre title y subtitle)
                    ),
                    plot.caption = element_text(size=9, face = "italic"),
                    panel.background = element_blank(),
                    panel.border = element_blank(), #junta el axis con la plot
                    panel.grid.major.x = element_line(linetype = 2,
                                                      size = .2,
                                                      color="#1ca4d4"),
                    panel.grid = element_blank(),
                    text = element_text(family = "nunito"),
                    axis.title.x = element_text(size=11,
                                                margin=margin(10,0,0,0)),
                    axis.text.y = element_text(size=11,
                                               margin = margin(0,-20,0,0)),
                    axis.text.x = element_text(size=9 ),
                    axis.ticks = element_blank(),
    )

  print("Gráfica creada")

  return(plot)


  # #guardar grafica
  # ggplot2::ggsave( plot=plot
  #   filename = paste0( folder,"/frecuencia_top_20_",type,"_",name,".png"),
  #   plot = plot,
  #   width = 8,
  #   height = 5,
  #   device = "png",
  #   dpi =96
  # )


}
