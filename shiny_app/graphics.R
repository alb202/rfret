library(ggplot2)
# This is a ggplot splash screen that shows up when the program first loads
splash_screen <- function(){

    log_curve1 <- function(x) {(10)/(1+exp(1)^(-3*(x-1)))}
    log_curve2 <- function(x) {(10)/(1+exp(1)^(-3*(x-3)))}
    log_curve3 <- function(x) {(10)/(1+exp(1)^(-3*(x-5)))}

    p <- ggplot(data.frame(x=c(0, 2)), aes(x)) +
        stat_function(fun=log_curve1, n = 50, geom = "point", aes(colour="red")) +
        stat_function(fun=log_curve2, n = 40, geom = "point", aes(colour="green")) +
        stat_function(fun=log_curve3, n = 40, geom = "point", aes(colour="grey")) +
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

decision_indicator <- function(decision_index, position_index){
    decision_index[decision_index==TRUE] <- as.character("g")
    decision_index[decision_index==FALSE] <- as.character("r")
    decision_index[is.na(decision_index)] <- as.character("y")
    decision_index <- factor(decision_index, levels = c("g", "r", "y"), exclude = NULL, ordered = TRUE)
    decision_length <- length(decision_index)
    df <- data.frame(index=1:decision_length,
                     decisions=decision_index,
                     row=1,
                     stringsAsFactors = TRUE)
    p <- ggplot(df, aes(x = index, y = row)) +
        geom_tile(aes(fill = decisions), width=.8+(.2/decision_length), height=1) +
        theme_void() +
        theme(legend.position="none") +
        geom_rect(mapping = aes(xmin=position_index-.4-(.1/decision_length), xmax=position_index+.4+(.1/decision_length), ymin=0.5, ymax=1.5, fill=NULL), color="black", alpha = 0) +
        scale_fill_manual(values = c("g"="green", "r"="red", "y"="grey"))
    return(p)
}

