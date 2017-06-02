library(ggplot2)
# This is a ggplot splash screen that shows up when the program first loads
splash_screen <- function(){

    log_curve1 <- function(x) {(10)/(1+exp(1)^(-3*(x-1)))}
    log_curve2 <- function(x) {(10)/(1+exp(1)^(-3*(x-3)))}
    log_curve3 <- function(x) {(10)/(1+exp(1)^(-3*(x-5)))}

    p <- ggplot(data.frame(x=c(0, 2)), aes(x)) +
        stat_function(fun=log_curve1, n = 50, geom = "point", aes(colour="red")) +
        stat_function(fun=log_curve2, n = 40, geom = "point", aes(colour="green")) +
        stat_function(fun=log_curve3, n = 40, geom = "point", aes(colour="yelow")) +
        annotate("text", x = 3, y = 5, label = 'bold("RFRET")', color = "black",
                 size=20, parse=TRUE) +
        ylim(0, 10) +
        xlim(0, 6) +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              legend.position="none")
    return(p)
}
