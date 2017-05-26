library(ggplot2)
# This is a ggplot splash screen that shows up when the program first loads
splash_screen <- function(){

    p <- ggplot() +
        geom_point() +
        annotate("text", x = 4, y = 25, label = 'bold("RFRET")', color = "blue",
                 size=20, parse=TRUE) +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank())
    return(p)
}
