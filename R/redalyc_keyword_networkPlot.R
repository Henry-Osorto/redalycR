#' Keyword co-occurrence network plot (DE field)
#'
#' @param data data.frame.
#' @param keywords_col Columna de keywords (default "DE").
#' @param sep_regex Regex separador (default "\\\\s*[,;/]\\\\s*").
#' @param use_clean_helpers Si TRUE, usa split_keywords() y clean_keyword() del paquete.
#' @param normalize Normalización: NULL, "association", "jaccard", "inclusion", "salton", "equivalence".
#' @param n Número de nodos a plotear (top por grado).
#' @param min_freq Frecuencia mínima (document frequency) para conservar término.
#' @param edges_min Umbral mínimo de peso para conservar aristas (después de normalizar si aplica).
#' @param cluster Método de comunidades: "none", "walktrap", "louvain", "leiden", "infomap".
#' @param community_repulsion [0,1] separación adaptativa entre comunidades (tipo bibliometrix). Default 0.5.
#' @param layout_type Layout: "auto", "circle", "mds", "fruchterman", "kamada".
#' @param label_n Número de etiquetas a dibujar (se implementa como cuantil del grado, tipo bibliometrix).
#' @param labelsize Tamaño base de la etiqueta.
#' @param label_cex Si TRUE, tamaño de etiqueta proporcional al grado (tipo bibliometrix).
#' @param label_color Si TRUE, color de etiqueta = color de cluster (tipo bibliometrix). Si FALSE, negro.
#' @param label_dist Distancia etiqueta-nodo.
#' @param label_font Fuente etiqueta.
#' @param size Tamaño base nodo.
#' @param size_cex Si TRUE, tamaño nodo proporcional al grado.
#' @param size_add Aumento fijo a todos los tamaños de nodo (mejora visibilidad).
#' @param size_cap Tope máximo del tamaño del nodo.
#' @param curved Curvatura de aristas (FALSE o número 0-1).
#' @param edge_lty_inter Tipo de línea para aristas inter-comunidad.
#' @param edge_lty_intra Tipo de línea para aristas intra-comunidad.
#' @param alpha Transparencia [0,1].
#' @param edge_alpha Transparencia específica para aristas. Si NULL usa alpha/4.
#' @param seed Semilla.
#' @param plot Si TRUE grafica en pantalla.
#'
#' @param save Si TRUE guarda salidas en ~/Documents/output_redalycR.
#' @param width Ancho de la figura (guardado).
#' @param height Alto de la figura (guardado).
#' @param prefix Prefijo del nombre de archivo.
#' @param ext Extensiones a guardar (png/pdf/svg).
#'
#' @return Lista con graph (igraph), NetMatrix, freq, layout, community_obj, cluster_res,
#'   y tablas nodes/edges útiles para exportación.
#' @export
redalyc_keyword_networkPlot <- function(
    data,
    keywords_col = "DE",
    sep_regex = "\\s*[,;/]\\s*",
    use_clean_helpers = TRUE,
    normalize = "association",
    n = 80,
    min_freq = 2,
    edges_min = 0,
    cluster = "walktrap",
    community_repulsion = 0.5,
    layout_type = "fruchterman",
    label_n = 30,
    labelsize = 0.8,
    label_cex = FALSE,
    label_color = FALSE,
    label_dist = 0.8,
    label_font = 2,
    size = 6,
    size_cex = TRUE,
    size_add = 1.5,
    size_cap = 12,
    curved = FALSE,
    edge_lty_inter = 5,
    edge_lty_intra = 1,
    alpha = 0.7,
    edge_alpha = NULL,
    seed = 123,
    plot = TRUE,
    save = FALSE,
    width = 10,
    height = 6,
    prefix = "keyword_network",
    ext = c("png", "pdf", "svg")
) {
  if (!is.data.frame(data)) stop("`data` debe ser un data.frame.", call. = FALSE)
  
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Paquete requerido 'igraph' no está instalado.", call. = FALSE)
  }
  if (!requireNamespace("Matrix", quietly = TRUE)) {
    stop("Paquete requerido 'Matrix' no está instalado.", call. = FALSE)
  }
  
  if (!keywords_col %in% names(data)) {
    stop(sprintf("No existe la columna '%s' en `data`.", keywords_col), call. = FALSE)
  }
  
  if (alpha < 0 || alpha > 1) alpha <- 0.5
  if (community_repulsion < 0) community_repulsion <- 0
  if (community_repulsion > 1) community_repulsion <- 1
  if (is.null(edge_alpha)) edge_alpha <- max(0.05, alpha / 4)
  
  x <- as.character(data[[keywords_col]])
  x[is.na(x)] <- ""
  
  # --- split + clean ---
  if (isTRUE(use_clean_helpers)) {
    if (!requireNamespace("stringr", quietly = TRUE) || !requireNamespace("dplyr", quietly = TRUE)) {
      stop("Para use_clean_helpers=TRUE necesitas 'stringr' y 'dplyr' instalados.", call. = FALSE)
    }
    
    split_kw <- split_keywords(x, sep_regex = sep_regex)
    split_kw <- lapply(split_kw, function(v) {
      v <- clean_keyword(v)
      v <- v[!is.na(v)]
      v <- unique(v)
      v
    })
  } else {
    split_kw <- strsplit(x, split = ";", fixed = TRUE)
    split_kw <- lapply(split_kw, function(v) {
      v <- gsub("[\r\n\t]+", " ", v)
      v <- trimws(v)
      v <- v[nzchar(v)]
      v <- toupper(v)
      unique(v)
    })
  }
  
  # eliminar docs sin keywords
  has_kw <- lengths(split_kw) > 0
  split_kw <- split_kw[has_kw]
  if (length(split_kw) < 2) stop("Muy pocos registros con keywords (DE) para construir una red.", call. = FALSE)
  
  # document frequency real
  df_terms <- unlist(lapply(split_kw, unique), use.names = FALSE)
  dfreq <- sort(table(df_terms), decreasing = TRUE)
  
  # filtrar por min_freq
  keep_terms <- names(dfreq)[dfreq >= min_freq]
  if (length(keep_terms) < 2) stop("min_freq es muy alto: no quedan términos suficientes.", call. = FALSE)
  
  split_kw <- lapply(split_kw, function(v) intersect(unique(v), keep_terms))
  
  # recomputar frecuencia
  df_terms2 <- unlist(split_kw, use.names = FALSE)
  dfreq2 <- sort(table(df_terms2), decreasing = TRUE)
  
  terms <- names(dfreq2)
  t_index <- setNames(seq_along(terms), terms)
  
  # matriz doc-term (sparse)
  i <- integer(0); j <- integer(0)
  for (doc_id in seq_along(split_kw)) {
    v <- split_kw[[doc_id]]
    if (length(v) == 0) next
    i <- c(i, rep.int(doc_id, length(v)))
    j <- c(j, unname(t_index[v]))
  }
  
  X <- Matrix::sparseMatrix(
    i = i, j = j, x = 1,
    dims = c(length(split_kw), length(terms)),
    dimnames = list(NULL, terms)
  )
  
  # co-ocurrencia
  Net <- Matrix::t(X) %*% X
  Matrix::diag(Net) <- 0
  
  # normalización
  NetN <- Net
  if (!is.null(normalize)) {
    normalize <- tolower(normalize)
    wi <- Matrix::diag(Matrix::t(X) %*% X)
    wi[wi == 0] <- 1
    
    if (normalize == "association") {
      denom <- outer(wi, wi, "*")
      NetN <- Net / denom
    } else if (normalize == "jaccard") {
      W1 <- matrix(wi, nrow = length(wi), ncol = length(wi))
      W2 <- t(W1)
      NetN <- Net / (W1 + W2 - Net)
    } else if (normalize == "inclusion") {
      W1 <- matrix(wi, nrow = length(wi), ncol = length(wi))
      W2 <- t(W1)
      NetN <- Net / pmin(W1, W2)
    } else if (normalize == "salton") {
      W1 <- matrix(wi, nrow = length(wi), ncol = length(wi))
      W2 <- t(W1)
      NetN <- Net / sqrt(W1 * W2)
    } else if (normalize == "equivalence") {
      W1 <- matrix(wi, nrow = length(wi), ncol = length(wi))
      W2 <- t(W1)
      NetN <- (Net * Net) / (W1 * W2)
    } else {
      stop("normalize no reconocido. Usa NULL, 'association', 'jaccard', 'inclusion', 'salton' o 'equivalence'.",
           call. = FALSE)
    }
    
    NetN[is.na(NetN)] <- 0
  }
  
  # igraph
  g <- igraph::graph_from_adjacency_matrix(as.matrix(NetN), mode = "undirected", weighted = TRUE, diag = FALSE)
  igraph::V(g)$name <- colnames(as.matrix(NetN))
  
  # edges_min
  if (!is.null(edges_min) && edges_min > 0) {
    w0 <- igraph::E(g)$weight
    drop_e <- which(w0 < edges_min)
    if (length(drop_e) > 0) g <- igraph::delete_edges(g, igraph::E(g)[drop_e])
  }
  if (igraph::ecount(g) == 0) stop("No quedaron aristas: baja edges_min o min_freq.", call. = FALSE)
  
  # top-n por grado
  deg0 <- igraph::degree(g, mode = "all")
  ord <- order(deg0, decreasing = TRUE)
  n <- min(n, length(ord))
  g <- igraph::induced_subgraph(g, ord[seq_len(n)])
  
  # recomputar grado
  deg <- igraph::degree(g, mode = "all")
  igraph::V(g)$deg <- deg
  
  # tamaños nodos
  if (isTRUE(size_cex)) {
    mdeg <- max(deg); if (mdeg <= 0) mdeg <- 1
    igraph::V(g)$size <- (deg / mdeg) * size
  } else {
    igraph::V(g)$size <- rep(size, igraph::vcount(g))
  }
  
  if (!is.null(size_add) && is.finite(size_add) && size_add != 0) {
    igraph::V(g)$size <- igraph::V(g)$size + size_add
  }
  if (!is.null(size_cap) && is.finite(size_cap)) {
    igraph::V(g)$size <- pmin(igraph::V(g)$size, size_cap)
  }
  
  # tamaño etiquetas
  if (isTRUE(label_cex)) {
    lsize <- log(1 + (deg / max(deg))) * labelsize
    lsize[lsize < 0.5] <- 0.5
    igraph::V(g)$label.cex <- lsize
  } else {
    igraph::V(g)$label.cex <- labelsize
  }
  
  # --- clustering ---
  set.seed(seed)
  comm <- NULL
  membership <- rep(1L, igraph::vcount(g))
  cluster <- tolower(cluster)
  
  if (cluster != "none") {
    comm <- switch(
      cluster,
      walktrap = igraph::cluster_walktrap(g),
      louvain  = igraph::cluster_louvain(g),
      leiden   = igraph::cluster_leiden(g, objective_function = "modularity"),
      infomap  = igraph::cluster_infomap(g),
      igraph::cluster_walktrap(g)
    )
    membership <- comm$membership
  }
  igraph::V(g)$community <- membership
  
  # colores estables
  base_cols <- c("#1f77b4","#ff7f0e","#2ca02c","#d62728","#9467bd",
                 "#8c564b","#e377c2","#7f7f7f","#bcbd22","#17becf")
  k <- length(unique(membership))
  cols <- rep(base_cols, length.out = k)
  igraph::V(g)$color <- cols[membership]
  
  # --- community repulsion adaptativa (para layout) ---
  adaptive_repulsion_strength <- function(community_repulsion, n_nodes, n_communities, avg_community_size) {
    scale_factor <- log10(n_nodes + 10) / log10(100)
    community_factor <- 1 + (n_communities - 2) * 0.1
    community_factor <- max(0.5, min(community_factor, 2))
    x <- community_repulsion * 10
    sigmoid_transform <- x / (1 + x)
    sigmoid_transform * scale_factor * community_factor
  }
  
  apply_community_repulsion <- function(g, community_repulsion) {
    if (is.null(community_repulsion) || community_repulsion <= 0) return(g)
    if (community_repulsion > 1) community_repulsion <- 1
    
    memb <- igraph::V(g)$community
    if (is.null(memb)) return(g)
    
    n_nodes <- igraph::vcount(g)
    n_comm <- length(unique(memb))
    comm_sizes <- table(memb)
    avg_size <- mean(comm_sizes)
    
    rep_strength <- adaptive_repulsion_strength(community_repulsion, n_nodes, n_comm, avg_size)
    
    row <- igraph::get.edgelist(g, names = TRUE)
    w0 <- igraph::E(g)$weight
    if (is.null(w0) || all(is.na(w0))) w0 <- rep(1, nrow(row))
    
    new_w <- numeric(nrow(row))
    names(memb) <- igraph::V(g)$name
    
    for (ii in seq_len(nrow(row))) {
      n1 <- row[ii, 1]; n2 <- row[ii, 2]
      c1 <- memb[[n1]]; c2 <- memb[[n2]]
      
      if (c1 == c2) {
        multiplier <- 1 + (rep_strength^0.7) * 1.5
        new_w[ii] <- w0[ii] * multiplier
      } else {
        divisor <- 1 + exp(rep_strength * 1.2) - 1
        new_w[ii] <- w0[ii] / divisor
      }
    }
    
    igraph::E(g)$weight <- new_w
    g
  }
  
  g_layout <- apply_community_repulsion(g, community_repulsion)
  
  # --- edge styles (intra vs inter) ---
  El <- as.data.frame(igraph::get.edgelist(g, names = FALSE))
  comm_v <- igraph::V(g)$community
  
  ecols <- apply(El, 1, function(x) {
    if (comm_v[x[1]] == comm_v[x[2]]) cols[comm_v[x[1]]] else "gray70"
  })
  igraph::E(g)$color <- ecols
  igraph::E(g)$lty <- edge_lty_intra
  igraph::E(g)$lty[igraph::E(g)$color == "gray70"] <- edge_lty_inter
  
  # edge width (suave)
  w <- as.numeric(igraph::E(g)$weight)
  if (all(is.na(w))) w <- rep(1, length(w))
  wmin <- min(w); wmax <- max(w)
  if (wmax == 0) wmax <- 1
  igraph::E(g)$width <- ((w + wmin) / max(w + wmin)) * 1
  
  # --- layout ---
  layout_type <- tolower(layout_type)
  L <- switch(
    layout_type,
    auto = igraph::layout_with_fr(g_layout),
    circle = igraph::layout_in_circle(g_layout),
    mds = igraph::layout_with_mds(g_layout),
    fruchterman = igraph::layout_with_fr(g_layout),
    kamada = igraph::layout_with_kk(g_layout),
    igraph::layout_with_fr(g_layout)
  )
  L <- igraph::norm_coords(L)
  
  # --- Labelling (cuantil por grado, tipo bibliometrix) ---
  LABEL <- ""
  if (!is.null(label_n)) {
    LABEL <- igraph::V(g)$name
    q <- 1 - (label_n / length(igraph::V(g)$deg))
    if (q <= 0) {
      igraph::V(g)$labelsize <- 10
    } else {
      if (q > 1) q <- 1
      thr <- stats::quantile(igraph::V(g)$deg, q)
      LABEL[igraph::V(g)$deg < thr] <- ""
      igraph::V(g)$labelsize <- 10
      igraph::V(g)$labelsize[igraph::V(g)$deg < thr] <- 0
    }
  }
  igraph::V(g)$label <- LABEL
  
  # label color
  if (isTRUE(label_color)) {
    lab.col <- igraph::V(g)$color
  } else {
    lab.col <- "black"
  }
  
  # --- atributos tipo bibliometrix para plot base igraph ---
  igraph::graph_attr(g, "alpha") <- alpha
  igraph::graph_attr(g, "ylim") <- c(-1, 1)
  igraph::graph_attr(g, "xlim") <- c(-1, 1)
  igraph::graph_attr(g, "rescale") <- TRUE
  igraph::graph_attr(g, "asp") <- 0
  igraph::graph_attr(g, "layout") <- L
  igraph::graph_attr(g, "main") <- ""
  
  igraph::E(g)$curved <- curved
  igraph::E(g)$color <- grDevices::adjustcolor(igraph::E(g)$color, edge_alpha)
  
  igraph::V(g)$label.dist <- label_dist
  igraph::V(g)$frame.color <- grDevices::adjustcolor("black", alpha)
  igraph::V(g)$color <- grDevices::adjustcolor(igraph::V(g)$color, alpha)
  igraph::V(g)$label.color <- grDevices::adjustcolor(lab.col, min(1, alpha + 0.1))
  igraph::V(g)$label.font <- label_font
  
  # --- tablas útiles para exportar ---
  nodes_df <- data.frame(
    node = igraph::V(g)$name,
    degree = igraph::V(g)$deg,
    cluster = igraph::V(g)$community,
    size = igraph::V(g)$size,
    stringsAsFactors = FALSE
  )
  
  ed <- igraph::as_data_frame(g, what = "edges")
  if (!("weight" %in% names(ed))) ed$weight <- NA_real_
  edges_df <- data.frame(
    from = ed$from,
    to = ed$to,
    weight = ed$weight,
    stringsAsFactors = FALSE
  )
  
  # --- resultados clustering ---
  if (!is.null(comm) && cluster != "none") {
    cluster_res <- data.frame(
      vertex = igraph::V(g)$name,
      cluster = igraph::V(g)$community,
      btw_centrality = as.numeric(igraph::betweenness(g, directed = FALSE, normalized = FALSE)),
      clos_centrality = suppressWarnings(as.numeric(igraph::closeness(g))),
      pagerank_centrality = as.numeric(igraph::page_rank(g)$vector),
      stringsAsFactors = FALSE
    )
    cluster_res <- cluster_res[order(cluster_res$cluster), ]
  } else {
    cluster_res <- NA
  }
  
  # --- plot en pantalla ---
  if (isTRUE(plot)) {
    graphics::plot(g)
  }
  
  # --- Guardado (gráfico + Excel) ---
  if (isTRUE(save)) {
    
    # Excel requiere writexl (Suggests), igual que tu frequency_keywords()
    .redalycR_need("writexl", "to save Excel output")
    
    if ("svg" %in% ext && !requireNamespace("svglite", quietly = TRUE)) {
      warning(
        "Package 'svglite' is not installed. SVG output may use a fallback device.\n",
        "Install it with install.packages('svglite') for better SVG output.",
        call. = FALSE
      )
    }
    
    out_dir <- file.path(path.expand("~"), "Documents", "output_redalycR")
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
    
    base <- file.path(
      out_dir,
      sprintf(
        "%s_%s_n%03d_minfreq%02d_%s",
        prefix, keywords_col, n, min_freq, tolower(layout_type)
      )
    )
    
    # Guardar gráficos desde dispositivo base (igraph)
    for (e in ext) {
      f <- paste0(base, ".", e)
      
      if (tolower(e) == "pdf") {
        grDevices::pdf(f, width = width, height = height)
      } else if (tolower(e) == "png") {
        grDevices::png(f, width = width, height = height, units = "in", res = 300)
      } else if (tolower(e) == "svg") {
        # si svglite está, úsalo; si no, fallback a grDevices::svg
        if (requireNamespace("svglite", quietly = TRUE)) {
          svglite::svglite(f, width = width, height = height)
        } else {
          grDevices::svg(f, width = width, height = height)
        }
      } else {
        warning(sprintf("Extensión '%s' no soportada. Usa png/pdf/svg.", e), call. = FALSE)
        next
      }
      
      # re-plot en el device
      graphics::plot(g)
      grDevices::dev.off()
    }
    
    # Excel con tablas (freq + nodes + edges + cluster)
    writexl::write_xlsx(
      list(
        freq_all = data.frame(KW = names(dfreq2), n = as.integer(dfreq2), row.names = NULL),
        nodes = nodes_df,
        edges = edges_df,
        cluster = if (is.data.frame(cluster_res)) cluster_res else data.frame()
      ),
      path = paste0(base, "_ALL.xlsx")
    )
  }
  
  invisible(list(
    graph = g,
    NetMatrix = NetN,
    freq = dfreq2,
    nodes = nodes_df,
    edges = edges_df,
    layout = L,
    community_obj = comm,
    cluster_res = cluster_res
  ))
}

