# ---- FIGURE 2 ----

library(tidyverse)
library(ggbrace)
library(ggforce)
library(latex2exp)
library(pedtools)
library(paletteer)

library(ibdsim2)
library(ibdrel)
library(patchwork)

# ---- SETUP ----

off <- 0.5
s <- 1
s2 <- s*2
scale.factor <- 0.8
col.vals <- c("anchor" = "#F8CECC",
              "free" = "#DAE8FC",
              "target" = "#F5F5F5",
              "founder" = "#F5F5F5",
              "female" = "#d6f8cc",
              "male" = "#f8f1cc")

# Avuncular

nodes <- data.frame(
  id = c("mother", "father", "desc111", "desc112",
         "desc121", "desc131", "desc141",
         "mother", "father", "desc211", "desc212",
         "desc221", "desc231", "desc241",
         "mother", "father", "desc311", "desc312",
         "desc321", "desc331", "desc341"),
  x = c(0, 2, 0, 2, 0, 0, 0,
        4+s, 6+s, 4+s, 6+s, 4+s, 4+s, 4+s,
        8+s2, 10+s2, 8+s2, 10+s2, 8+s2, 8+s2, 8+s2),
  y = c(8, 8, 6, 6, 4, 2, 0,
        8, 8, 6, 6, 4, 2, 0,
        8, 8, 6, 6, 4, 2, 0),
  shape = c("square", "circle", "square", "diamond",
            "square", "circle", "diamond",
            "square", "circle", "square", "diamond",
            "circle", "square", "diamond",
            "square", "circle", "circle", "diamond",
            "square", "square", "diamond"),
  type = c("founder", "founder", "male", "target",
           "male", "female", "target",
           "founder", "founder", "male", "target",
           "female", "male",   "target",
           "founder", "founder", "female", "target",
           "male", "male", "target")
)
diamond_nodes <- nodes |>
  filter(shape == "diamond") |>
  rowwise() |>
  do({
    x <- .$x
    y <- .$y
    col <- .$type
    data.frame(
      id = .$id,
      x = c(x - off, x, x + off, x),
      y = c(y, y + off, y, y - off),
      point = 1:4,
      type = .$type
    )
  })
edges.top <- data.frame(
  x = c(0, 2, 1, 0, 2, 0, 2,
        4+s, 6+s, 5+s, 4+s, 6+s, 4+s, 6+s,
        8+s2, 10+s2, 9+s2, 8+s2, 10+s2, 8+s2, 10+s2),
  xend = c(1, 1, 1, 1, 1, 0, 2,
           5+s, 5+s, 5+s, 5+s, 5+s, 4+s, 6+s,
           9+s2, 9+s2, 9+s2, 9+s2, 9+s2, 8+s2, 10+s2),
  y = c(8, 8, 8, 7, 7, 7, 7,
        8, 8, 8, 7, 7, 7, 7,
        8, 8, 8, 7, 7, 7, 7),
  yend = c(8, 8, 7, 7, 7, 6, 6,
           8, 8, 7, 7, 7, 6, 6,
           8, 8, 7, 7, 7, 6, 6)
)
edges <- data.frame(
  x = c(0, 0, 0,
        4+s, 4+s, 4+s,
        8+s2, 8+s2, 8+s2),
  xend = c(0, 0, 0,
           4+s, 4+s, 4+s,
           8+s2, 8+s2, 8+s2),
  y = c(6, 4, 2,
        6, 4, 2,
        6, 4, 2),
  yend = c(4, 2, 0,
           4, 2, 0,
           4, 2, 0),
  linetype = c("solid", "solid", "solid",
               "solid", "solid", "solid",
               "solid", "solid", "solid")
)
text <- data.frame(
  x = c(3+s/2, 7+(s/2*3)),
  y = c(3, 3),
  lab = c("=", "≠")
)
annot <- data.frame(
  x = c(1,5+s,9+s2),
  y = rep(9, 3),
  lab = c("I", "II", "III")
)
ggplot() +
  geom_segment(data = edges.top, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_segment(data = edges, aes(x = x, y = y-off*scale.factor, xend = xend, yend = yend+off*scale.factor),
               linetype = edges$linetype) +
  geom_rect(data = nodes[nodes$shape == "square",], mapping =
              aes(xmin = x - off*scale.factor, xmax = x + off*scale.factor, ymin = y-off*scale.factor, ymax = y+off*scale.factor,
                  fill = type),
            colour = "black") +
  geom_circle(data = nodes[nodes$shape == "circle",],
              aes(x0 = x, y0 = y, r = off*scale.factor, fill = type), color = "black") +
  geom_polygon(
    data = diamond_nodes,
    aes(x = x, y = y, group = id, fill = type),
    color = "black"
  ) +
  geom_point(data = data.frame(x = c(0, 2, 4+s, 6+s, 8+s2, 10+s2), y = c(0, 6, 0, 6, 0, 6)), aes(x, y)) +
  geom_text(data = text, mapping = aes(x = x, y = y, label = lab), size = 24, size.unit = "pt") +
  geom_text(data = annot, mapping = aes(x, y, label = lab), size = 11, size.unit = "pt", family = "serif") +
  coord_equal() +
  scale_fill_manual(values = col.vals) +
  theme_void(base_size=11) +
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(-1, 10), expand = c(0, 0)) +
  scale_x_continuous(expand = c(0,0)) +
  theme(plot.margin = margin(0,0,0,0))-> avuncular
avuncular

nodes <- data.frame(
  id = c("mother", "father", "desc111", "desc112",
         "desc121", "desc122", "desc131", "desc141",
         "mother", "father", "desc211", "desc212",
         "desc221", "desc222", "desc231", "desc232",
         "mother", "father", "desc311", "desc312",
         "desc321", "desc322", "desc331", "desc332"),
  x = c(0, 2, 0, 2, 0, 2, 0, 0,
        4+s, 6+s, 4+s, 6+s, 4+s, 6+s, 4+s, 6+s,
        8+s2, 10+s2, 8+s2, 10+s2, 8+s2, 10+s2, 8+s2, 10+s2),
  y = c(8, 8, 6, 6, 4, 4, 2, 0,
        8, 8, 6, 6, 4, 4, 2, 2,
        8, 8, 6, 6, 4, 4, 2, 2),
  shape = c("square", "circle", "square", "circle",
            "square", "diamond", "circle", "diamond",
            "square", "circle", "square", "circle",
            "circle", "square", "diamond", "diamond",
            "square", "circle", "circle", "circle",
            "square", "square", "diamond", "diamond"),
  type = c("founder", "founder", "male", "female",
           "male", "target", "female", "target",
           "founder", "founder", "male", "female",
           "female", "male", "target", "target",
           "founder", "founder", "female", "female",
           "male", "male", "target", "target"))
diamond_nodes <- nodes |>
  filter(shape == "diamond") |>
  rowwise() |>
  do({
    x <- .$x
    y <- .$y
    col <- .$type
    data.frame(
      id = .$id,
      x = c(x - off, x, x + off, x),
      y = c(y, y + off, y, y - off),
      point = 1:4,
      type = .$type
    )
  })
edges.top <- data.frame(
  x = c(0, 2, 1, 0, 2, 0, 2,
        4+s, 6+s, 5+s, 4+s, 6+s, 4+s, 6+s,
        8+s2, 10+s2, 9+s2, 8+s2, 10+s2, 8+s2, 10+s2),
  xend = c(1, 1, 1, 1, 1, 0, 2,
           5+s, 5+s, 5+s, 5+s, 5+s, 4+s, 6+s,
           9+s2, 9+s2, 9+s2, 9+s2, 9+s2, 8+s2, 10+s2),
  y = c(8, 8, 8, 7, 7, 7, 7,
        8, 8, 8, 7, 7, 7, 7,
        8, 8, 8, 7, 7, 7, 7),
  yend = c(8, 8, 7, 7, 7, 6, 6,
           8, 8, 7, 7, 7, 6, 6,
           8, 8, 7, 7, 7, 6, 6)
)
edges <- data.frame(
  x = c(0, 2, 0, 0,
        4+s, 6+s, 4+s, 6+s,
        8+s2, 10+s2, 8+s2, 10+s2),
  xend = c(0, 2, 0, 0,
           4+s, 6+s, 4+s, 6+s,
           8+s2, 10+s2, 8+s2, 10+s2),
  y = c(6, 6, 4, 2,
        6, 6, 4, 4,
        6, 6, 4, 4),
  yend = c(4, 4, 2, 0,
           4, 4, 2, 2,
           4, 4, 2, 2),
  linetype = c("solid", "solid", "solid", "solid",
               "solid", "solid", "solid", "solid",
               "solid", "solid", "solid", "solid")
)
ggplot() +
  geom_segment(data = edges.top, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_segment(data = edges, aes(x = x, y = y-off*scale.factor, xend = xend, yend = yend+off*scale.factor),
               linetype = edges$linetype) +
  geom_rect(data = nodes[nodes$shape == "square",], mapping =
              aes(xmin = x - off*scale.factor, xmax = x + off*scale.factor, ymin = y-off*scale.factor, ymax = y+off*scale.factor,
                  fill = type),
            colour = "black") +
  geom_circle(data = nodes[nodes$shape == "circle",],
              aes(x0 = x, y0 = y, r = off*scale.factor, fill = type), color = "black") +
  geom_polygon(
    data = diamond_nodes,
    aes(x = x, y = y, group = id, fill = type),
    color = "black"
  ) +
  geom_point(data = data.frame(x = c(0, 2, 4+s, 6+s, 8+s2, 10+s2), y = c(0, 4, 2, 2, 2, 2)), aes(x, y)) +
  geom_text(data = text, mapping = aes(x = x, y = y, label = lab), size = 24, size.unit = "pt") +
  geom_text(data = annot, mapping = aes(x, y, label = lab), size = 11, size.unit = "pt", family = "serif") +
  coord_equal() +
  scale_fill_manual(values = col.vals) +
  theme_void(base_size=11) +
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(-1, 10), expand = c(0, 0)) +
  scale_x_continuous(expand = c(0,0)) +
  theme(plot.margin = margin(0,0,0,0)) -> cousin
cousin


# Simulations
extractFeatures <- function(segments, df = T) {

  features <- lapply(segments, function(x) {
    list(
      count = lengths(x),
      mean = vapply(x, mean, FUN.VALUE = 1),
      total = vapply(x, sum, FUN.VALUE = 1)
    )
  })

  if (isFALSE(df)) return(features)

  f <- do.call(rbind.data.frame, features)

  f <- f |>
    rownames_to_column(var = "rel")

  f$rel <- sapply(strsplit(f$rel, "\\."), `[[`, 1)

  f
}

# AVUNCULAR
avpeds <- list(avuncularPed(removal = 3) |> swapSex(c(8, 10)),
               avuncularPed(removal = 3) |> swapSex(c(6, 10)),
               avuncularPed(removal = 3) |> swapSex(c(4, 10)))
avannot <- list("I", "II", "III")
avmeta <- ibdrel::pedsMetadata(avpeds)

avsim = ibdrel::ibdSimulations(avpeds, N = 100000, seed = c(20250902, 20250903, 20250904))
avsegs = ibdrel::lengthIBD(avsim, avpeds, avannot)

#saveRDS(avsegs, file = "data/avsegs.rds")

#avsegs <- readRDS("~/relationship-equivalence-paper/data/avsegs.rds")

# COUSIN

couspeds <- list(cousinPed(1, removal = 2) |> swapSex(c(3, 10,12)),
                 cousinPed(2) |> swapSex(c(5, 7,12)),
                 cousinPed(2) |> swapSex(c(3,5,12)))
cousannot <- list("I",
                  "II",
                  "III")
cousmeta <- ibdrel::pedsMetadata(couspeds)

coussim <- ibdrel::ibdSimulations(couspeds, N = 100000, seed = c(20250902, 20250903, 20250904))
coussegs = ibdrel::lengthIBD(coussim, couspeds, cousannot)

#saveRDS(coussegs, file = "data/coussegs.rds")
#coussegs = readRDS("data/coussegs.rds")


sim_theme = theme_bw(base_size = 11) +
  theme_sub_plot(
    margin = margin(t = 0.5, l = 1, r = 0.1, 0, unit = "lines")
  ) +
  theme_sub_legend(
    title = element_blank(),
    text = element_text(family = "serif"),
    position = "inside",
    position.inside = c(.995, .995),
    justification.inside = c(1, 1),
    key.width = unit(1.05, "cm"),
    key.height = unit(0.8, "lines")
  )

sim_plot = function(segs, breaks, xlim) {
  dat = extractFeatures(segs)
  ggplot(dat, aes(x = mean, y = count, colour = rel, linetype = rel)) +
    stat_ellipse(level = 0.95, linewidth = 1.2) +
    labs(x = "Mean segment length (cM)",
         y = "Number of segments") +
    scale_colour_paletteer_d(
      "MoMAColors::Clay",
      guide = guide_legend(override.aes = list(linewidth = 0.9))
    ) +
    scale_linetype_manual(values = c("solid", "dashed", "dotdash")) +
    scale_x_continuous(breaks = breaks) +
    coord_cartesian(xlim = xlim) +
    sim_theme
}

pad = function(x, gap = 1) {
  wrap_elements(full = x + theme(plot.margin = margin(0, gap, gap, 2*gap, unit = "cm")))
}

#avsegs = readRDS("data/avsegs.rds")
#coussegs = readRDS("data/coussegs.rds")

avsim = sim_plot(avsegs, c(10, 15, 20, 25, 30, 35), c(10, 35))
csim = sim_plot(coussegs, c(10, 15, 20, 25), c(7, 27))

# Combine plots
p = (pad(avuncular, 0.25) + avsim) / (pad(cousin, 0.25) + csim) +
  plot_annotation(tag_levels = "A",
                  theme = theme(
                    plot.tag = element_text(size = 16))
  )

p

#ggsave("figures/Figure_2.pdf", plot = p, width = 7.5, height = 6.5, units = "in", device = cairo_pdf)
