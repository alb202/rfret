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
        scale_fill_manual(values = c("g"="green", "r"="red", "y"="#e7e7e7"))
    return(p)
}

make_indicator <- function(decision=NA, text_label=NULL, selected=NULL){

    col_TRUE <- "#00D24E"
    col_FALSE <- "#FF1300"
    col_NA <- "#e7e7e7"
    # Palette link: http://paletton.com/#uid=303050kE1GeyhT4KUTotKsltupU

    line_color <- "black"
    if(isTRUE(decision)){
        fill_color <- col_TRUE
        }
    if(!isTRUE(decision) & !is.na(decision)){
        fill_color <- col_FALSE
        }
    if(is.na(decision)){
        fill_color <- col_NA
        }
    center <- c(1,1)
   diameter <- 1
   npoints <- 100
   start <- 0
   end <- 2
   line_size <- .05
   if(isTRUE(selected)) line_size <- 1
   filled <- TRUE
   tt <- seq(start*pi, end*pi, length.out=npoints)
   df <- data.frame(
       x = center[1] + diameter / 2 * cos(tt),
       y = center[2] + diameter / 2 * sin(tt)
   )

    p <- ggplot() + theme_void() +
        geom_polygon(data=df, aes(x,y), color=line_color, fill=fill_color, size=line_size)

    if(!is.null(text_label))
        p <- p + annotate(geom = "text", x = 1, y = 1, label = as.character(text_label), size=5)
    return(p)
}

html_indicator <- function(decisions){

    # The color values for the 3 options
    col_TRUE <- "SpringGreen"
    col_FALSE <- "Salmon"
    col_NA <- "#e7e7e7"

    # For each decision, create a text string for the DIV that will be used in the HTML
    unlist(mapply(1:length(decisions), decisions, FUN = function(x, y) {
        if(is.na(y)) bg_color <- col_NA
        if(isTRUE(y)) bg_color <- col_TRUE
        if(identical(y,FALSE)) bg_color <- col_FALSE
        paste('<div align="center" style="font-weight: normal; border-style: solid; border-width: 1px; border-color: #cccccc; line-height: 22px; margin: 3px; border-radius: 27px; width: 27px; height: 27px; background-color:', bg_color,'; color: black;">', x, '</div>', sep = "")
    }))
}
