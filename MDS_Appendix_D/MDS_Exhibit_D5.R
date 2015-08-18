# Correlation Heat Map Utility (R)
#
# Input correlation matrix. Output heat map of correlation matrix.
# Requires R lattice package.
 
correlation_heat_map <- function(cormat, order_variable = NULL) {
    if (is.null(order_variable)) order_variable = rownames(cormat)[1]
    cormat_line <- cormat[order_variable, ]
    ordered_cormat <- 
        cormat[names(sort(cormat_line, decreasing=TRUE)),
            names(sort(cormat_line, decreasing=FALSE))]
    x <- rep(1:nrow(ordered_cormat), times=ncol(ordered_cormat))
    y <- NULL
    for (i in 1:ncol(ordered_cormat)) 
        y <- c(y,rep(i,times=nrow(ordered_cormat)))
    # use fixed format 0.XXX in cells of correlation matrix
    cortext <- sprintf("%0.3f", as.numeric(ordered_cormat))  
    text.data.frame <- data.frame(x, y, cortext)
    text.data.frame$cortext <- as.character(text.data.frame$cortext)
    text.data.frame$cortext <- ifelse((text.data.frame$cortext == "1.000"),
    NA,text.data.frame$cortext)  # define diagonal cells as missing
    text.data.frame <- na.omit(text.data.frame)  # diagonal cells have no text
    # determine range of correlations all positive or positive and negative
    if (min(cormat) > 0) 
        setcolor_palette <- colorRampPalette(c("white", "#00BFC4"))
    if (min(cormat) < 0) 
        setcolor_palette <- colorRampPalette(c("#F8766D", "white", "#00BFC4"))    
    # use larger sized type for small matrices
    set_cex = 1.0
    if (nrow(ordered_cormat) <= 4) set_cex = 1.5    
    print(levelplot(ordered_cormat, cuts = 25, tick.number = 9,
        col.regions = setcolor_palette, 
        scales=list(tck = 0, x = list(rot=45), cex = set_cex),
        xlab = "", 
        ylab = "",
        panel = function(...) {
            panel.levelplot(...)  
            panel.text(text.data.frame$x, text.data.frame$y, 
            labels = text.data.frame$cortext, cex = set_cex)
            }))
    }
save(correlation_heat_map, file = "correlation_heat_map.RData")  
  
  
  
