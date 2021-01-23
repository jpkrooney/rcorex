data("sunspots")

df <- data.frame(.preformat.ts(datasets::sunspots), stringsAsFactors = FALSE)
df <- sapply(df, as.numeric)
df <- data.frame(df)

# Build a hierarchical corex
set.seed(12345)
L1 <- biocorex(df, 10, 2, marginal_description = "gaussian", repeats = 1)
L2 <- biocorex(L1$labels, 6, 2, marginal_description = "gaussian", repeats = 1)
L3 <- biocorex(L2$labels, 3, 2, marginal_description = "gaussian", repeats = 1)
L4 <- biocorex(L2$labels, 1, 2, marginal_description = "gaussian", repeats = 1)

# make a tidygraph hierarchy
g1 <- make_corex_tidygraph( list(L1, L2, L3, L4 ))

# Plot network graph
p <- ggraph(g1, layout = "kk") +
    geom_node_point(aes(col=names), show.legend = FALSE) +
    geom_edge_link(aes(width = thickness, col=to), alpha = 0.75, show.legend = FALSE) +
    scale_edge_width(range = c(0.1, 1))
p <- p + theme_void() + theme_transparent()

#Generate sticker
sticker(p, package="rcorex", p_size=10, p_y =1.4, p_color = "#1881C2",
        s_x=1, s_y=.7, s_width=1.3, s_height=0.8,
        h_fill = "#FFFFFF", h_color = "#1881C2",
        filename="man/figures/rcorexlogo.svg")


