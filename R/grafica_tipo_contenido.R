#' @import magrittr ggplot2 ggthemes showtext sysfonts
#' @importFrom forcats fct_relevel
#' @importFrom dplyr group_by summarise distinct
#' @title grafica_contenido
#' @description Función que grafica un plot de barras
#' @param tmln dataframe con una columna llamada "contenido_propio"
#' @details Genera grafica de barras
#' @examples
#' grafica_contenido('tmln')
#' @export
#' @export ggplot


grafica_tipo_contenido <- function(tmln){

  #cargar fuente de google
  sysfonts::font_add_google("Nunito","nunito")



  # REcupero el usuario
  user <- tmln %>% dplyr::distinct(screen_name)
  #print(user)
  #print(folder)

  num_tweets <- tmln %>% tally( )

  ntweets <- num_tweets$n

  tmln_group <- tmln %>%
    dplyr::group_by(contenido_propio) %>%
    dplyr::summarise(ocurrencias= n())

  #print("group terminado")

 plot <-  ggplot2::ggplot(data=tmln_group, aes(x=forcats::fct_relevel( contenido_propio,
                                             "Contenido Propio",
                                             "Citar Tweet",
                                             "Respuesta a Tweet",
                                             "Retweet"),
                              y=ocurrencias,
                              fill=contenido_propio)
  )+
    ggplot2::geom_col() +
    ggplot2::geom_text(position = "identity",
                       aes(label=ocurrencias,
                           vjust=-.8),
                       # nudge_y = 28
    )+
   ggplot2::labs(title= "Distribucion por Tipo de Interacción",
         subtitle = paste0("Últimos ",ntweets, " tweets para el usuario @",user),
         x = NULL,
         y = "Número de Tweets")+
    ggthemes::scale_fill_economist()+
    ggplot2::theme( legend.position = "none",
           title = element_text(size=18, face = "bold"),
           plot.subtitle = element_text(size=15, face = "italic"),
           panel.background = element_blank(),
           panel.grid.major = element_blank(),
           panel.grid = element_blank(),
           text = element_text(family = "nunito"),
           axis.title = element_text(size=13),
           axis.text.x = element_text(size=11,face="bold"),
           axis.ticks = element_blank(),
           axis.text.y = element_blank()
    )

  # to work well with the RStudio graphics device (RStudioGD).
 showtext_opts(dpi = 96)
 showtext::showtext_auto()


    # ggplot2::ggsave(
    #   filename = paste0( folder,"/contenido_plot_",user,".png"),
    #   plot = plot,
    #   width = 8,
    #   height = 5,
    #   device = "png",
    #   dpi =96
    # )

  print("Grafica creada")

  return(plot)

}

