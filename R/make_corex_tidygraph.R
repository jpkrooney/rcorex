#' @title make_corex_tidygraph
#' @description Function to created a tidygraph object from a fitted corex object or list of objects that represent hierarchical data structures
#' @param corexes A single corex object, or a list of corex objects that should be each a layer of a hierarchy (see examples)
#' @return Returns a tidygraph object that represent the learned structure in the supplied corex object or objects. In the tidygraph object edge thickness is proportional to mutual information and node size is represents mutual multivariate information among children
#' @importFrom tidygraph tbl_graph
#' @export
#' @examples
#'  \dontrun{
#' library(rcorex)
#' library(ggraph)
#'
#' data("iris")
#' # Need to convert species factor variable to indicator variables
#' iris <- data.frame(iris , model.matrix(~iris$Species)[,2:3])
#' iris$Species <- NULL
#'
#' # fit two layers of corex
#' layer1 <- biocorex(iris, 3, 2, marginal_description = "gaussian", repeats = 5)
#' layer2 <- biocorex(layer1$labels, 1,2, marginal_description = "discrete", repeats = 5)
#'
#' # make a tidygraph of one layer
#' g1 <- make_corex_tidygraph( layer1 )
#' # make a tidygraph of two layers
#' g_hier <- make_corex_tidygraph( list(layer1, layer2))
#'
#' # Plot network graph of one layer
#' ggraph(g1, layout = "fr") +
#'     geom_node_point(aes(size = node_size), show.legend = FALSE) +
#'     geom_edge_hive(aes(width = thickness), alpha = 0.75, show.legend = FALSE) +
#'     #scale_edge_width(range = c(0.2, 2)) +
#'     geom_node_text(aes(label = names), repel = TRUE) +
#'     theme_graph()
#'
#' # Plot network graph of both layers
#' ggraph(g_hier, layout = "fr") +
#'     geom_node_point(aes(size = node_size), show.legend = FALSE) +
#'     geom_edge_hive(aes(width = thickness), alpha = 0.75, show.legend = FALSE) +
#'     #scale_edge_width(range = c(0.2, 2)) +
#'     geom_node_text(aes(label = names), repel = TRUE) +
#'     theme_graph()
#'     }
#'
make_corex_tidygraph <- function( corexes ){

    if( class(corexes) == "rcorex"){
        layer1 <- corexes
        edges <- calc_edge_thickness(layer1)

        node_names <- c(names(layer1$clusters), paste0("L1_", 1:length(layer1$tcs)))
        node_size <- c(rep(0, length(names(layer1$clusters))), layer1$tcs )
        nodes <- data.frame(names= node_names, node_size = node_size)

    } else if(class(corexes) == "list"){

        if( ! all(lapply(corexes, class) == "rcorex") ){
            stop(" You can only enter rcorex objects as list. i.e. corexes = list(layer1, layer2)")
        }
        len <- length(corexes)
        result_nodes <- list()
        result_edges <- list()
        for(i in 1:len){
            edges <- calc_edge_thickness(corexes[[i]], layer=i)
            node_names <- c(names(corexes[[i]]$clusters), paste0("L", i, "_", 1:length(corexes[[i]]$tcs)))
            node_size <- c(rep(0, length(names(corexes[[i]]$clusters))), corexes[[i]]$tcs )
            result_nodes[[i]] <- data.frame(names= node_names, node_size = node_size)
            result_edges[[i]] <- edges
        }
        nodes <- do.call("rbind" ,result_nodes)
        edges <- do.call("rbind", result_edges)

    } else {
        stop(" You can only enter rcorex objects as list. i.e. corexes = list(layer1, layer2)")
    }

    g <- tbl_graph(nodes = nodes, edges = edges, directed = FALSE)

    return(g)
}


# Support function
calc_edge_thickness <- function(corex_obj, layer=1){
    from <- names(corex_obj$clusters)
    if( is.null(from) ){
        from <- paste0("L", layer - 1, "_", 1:length(corex_obj$clusters))
    }
    temp <- data.frame(from = from, to = paste0("L", layer, "_", corex_obj$clusters + 1),
                       stringsAsFactors = FALSE)

    edge_thick <- data.frame( round(corex_obj$alpha) * corex_obj$mis)
    names(edge_thick) <- from
    edge_thick$cluster <- paste0("L", layer,"_", 1:nrow(edge_thick))

    temp$thickness <- NA
    for (i in 1:nrow(temp)){
        temp[i, ]$thickness <- edge_thick[ match( temp[i, ]$to, edge_thick$cluster),
                                       match( temp[i, ]$from, names(edge_thick))]
    }
    return(temp)
}
