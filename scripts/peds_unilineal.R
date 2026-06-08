# ---- FIGURE 1 ----

library(tidyverse)
library(ggbrace)
library(ggforce)
library(latex2exp)
library(showtext)


sysfonts::font_add_google("Noto Sans Math", "notomath")
showtext_auto()

# ---- SETUP ----

off <- .5
scale.factor <- 0.8
col.vals <- c("anchor" = "#F8CECC",
              "free" = "#DAE8FC",
              "target" = "#F5F5F5",
              "founder" = "white")

# --- DIRECT ----

nodes <- data.frame(
  id = c("founder", "desc1", "desc2", "desc3"),
  x = c(0, 0, 0, 0),
  y = c(8, 6, 4, 2),
  shape = c("diamond", "diamond", "diamond", "diamond"),
  type = c("target", "free", "free", "target")
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

edges <- data.frame(
  x = c(0, 0, 0),
  xend = c(0, 0, 0),
  y = c(8, 6, 4),
  yend = c(6, 4, 2),
  linetype = c("solid", "dashed", "solid")
)


ggplot() +
  geom_polygon(
    data = diamond_nodes,
    aes(x = x, y = y, group = id, fill = type),
    color = "black"
  ) +
  geom_point(data = data.frame(x = c(0, 0), y = c(2, 8)), aes(x, y)) +
  geom_segment(data = edges, aes(x = x, y = y-off, xend = xend, yend = yend+off),
               linetype = edges$linetype) +
  stat_brace(data = data.frame(x = c(0, 0), y = c(6.5, 3.5)), mapping = aes(x = x, y = y),
             rotate = 270, outerstart = -0.5, width = .5) +
  stat_bracetext(data.frame(x = c(2, 2), y = c(6.5, 3.5)), mapping = aes(x, y, label = TeX("$n$", italic = T)),
                 rotate = 270, outerstart = 0) +
  #geom_text(data = data.frame(x = c(0,0), y = c(0,0)), mapping = aes(x = x, y = y, label = TeX("$d=n+1$", italic = T))) +
  coord_equal(clip = "off") +
  scale_fill_manual(values = col.vals) +
  theme_void(base_size=11,
             base_family = "serif") +
  theme(legend.position = "none",
        plot.margin = margin(0,0.5,0,0, unit = "in"),
        plot.title = element_text(hjust = 0.75, size = 14)) +
  labs(title = "D") +
  scale_y_continuous(limits = c(1, 11), expand = c(0, 0)) -> direct
direct

# --- HALF-SIB ----


nodes <- data.frame(
  id = c("founder", "desc11", "desc12", "desc21", "desc22", "desc31", "desc32"),
  x = c(1, 0, 2, 0, 2, 0, 2),
  y = c(8, 6, 6, 4, 4, 2, 2),
  shape = c("diamond", "diamond", "diamond", "diamond", "diamond", "diamond", "diamond"),
  type = c("anchor", "free", "free", "free", "free", "target", "target")
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
  x = c(1-off, 1+off, 0, 2),
  xend = c(0, 2, 0, 2),
  y = c(8, 8, 8, 8),
  yend = c(8, 8, 6+off, 6+off)
)

edges <- data.frame(
  x = c(0, 2, 0, 2),
  xend = c(0, 2, 0, 2),
  y = c(6, 6, 4, 4),
  yend = c(4, 4, 2, 2),
  linetype = c("dashed", "dashed", "solid", "solid")
)

ggplot() +
  geom_polygon(
    data = diamond_nodes,
    aes(x = x, y = y, group = id, fill = type),
    color = "black"
  ) +
  geom_point(data = data.frame(x = c(0, 2), y = c(2, 2)), aes(x, y)) +
  geom_segment(data = edges.top, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_segment(data = edges, aes(x = x, y = y-off, xend = xend, yend = yend+off),
              linetype = edges$linetype) +
  stat_brace(data = data.frame(x = c(0, 0), y = c(6.5, 3.5)), mapping = aes(x = x, y = y),
             rotate = 270, outerstart = -0.5, width = .5) +
  stat_brace(data = data.frame(x = c(2, 2), y = c(6.5, 3.5)), mapping = aes(x, y),
             rotate = 90, outerstart = 2.5, width = .5) +
  stat_bracetext(data.frame(x = c(2, 2), y = c(6.5, 3.5)), mapping = aes(x, y, label = TeX("$n_1$", italic=T)),
                 rotate = 270, outerstart = 0) +
  stat_bracetext(data = data.frame(x = c(2, 2), y = c(6.5, 3.5)), mapping = aes(x, y, label = TeX("$n_2$", italic=T)),
                 rotate = 90, outerstart = 2) +
  #geom_text(data = data.frame(x = c(1,1), y = c(0,0)), mapping = aes(x = x, y = y, label = TeX("$d=n_1+n_2+2$",italic=T))) +
  coord_equal(clip = "off") +
  scale_fill_manual(values = col.vals) +
  theme_void(base_size=11,
             base_family="serif") +
  theme(legend.position = "none",
        plot.margin = margin(0,0.5,0,0, unit = "in"),
        plot.title = element_text(hjust = 0.5, size = 14)) +
  labs(title = "H") +
  scale_y_continuous(limits = c(1, 11), expand = c(0, 0)) -> hsib
hsib

# --- AVUNCULAR TYPE ----

nodes <- data.frame(
  id = c("mother", "father", "desc11", "desc12",
         "desc21", "desc31", "desc41"),
  x = c(0, 2, 0, 2, 0, 0, 0),
  y = c(10, 10, 8, 8, 6, 4, 2),
  shape = c("square", "circle", "diamond", "diamond",
            "diamond", "diamond", "diamond"),
  type = c("founder", "founder", "anchor", "target",
           "free", "free", "target")
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
  x = c(0+off*scale.factor, 2-off*scale.factor, 1, 0, 2, 0, 2),
  xend = c(1, 1, 1, 1, 1, 0, 2),
  y = c(10, 10, 10, 9, 9, 9, 9),
  yend = c(10, 10, 9, 9, 9, 8+off, 8+off)
)

edges <- data.frame(
  x = c(0, 0, 0),
  xend = c(0, 0, 0),
  y = c(8, 6, 4),
  yend = c(6, 4, 2),
  linetype = c("solid", "dashed", "solid")
)

ggplot() +
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
  geom_point(data = data.frame(x = c(0, 2), y = c(2, 8)), aes(x, y)) +
  geom_segment(data = edges.top, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_segment(data = edges, aes(x = x, y = y-off, xend = xend, yend = yend+off),
               linetype = edges$linetype) +
  stat_brace(data = data.frame(x = c(0, 0), y = c(6.5, 3.5)), mapping = aes(x = x, y = y),
             rotate = 90, outerstart = 0.5, width = .5) +
  stat_bracetext(data.frame(x = c(2, 2), y = c(6.5, 3.5)), mapping = aes(x, y, label = TeX("$n$",italic=T)),
                 rotate = 90, outerstart = 0) +
  #geom_text(data = data.frame(x = c(1,1), y = c(0,0)), mapping = aes(x = x, y = y, label = TeX("$d=n+2$",italic=T))) +
  coord_equal(clip = "off") +
  scale_fill_manual(values = col.vals) +
  theme_void(base_size=11,
             base_family = "serif") +
  theme(legend.position = "none",
        plot.margin = margin(0,0.5,0,0, unit = "in"),
        plot.title = element_text(hjust = 0.5, size = 14)) +
  labs(title = "A") +
  scale_y_continuous(limits = c(1, 11), expand = c(0, 0)) -> avuncular
avuncular

# --- COUSIN TYPE ----

nodes <- data.frame(
  id = c("mother", "father", "desc11", "desc12",
         "desc21", "desc22", "desc31", "desc32",
         "desc41", "desc42"),
  x = c(0, 2, 0, 2,
        0, 2, 0, 2,
        0, 2),
  y = c(10, 10, 8, 8,
        6, 6, 4, 4,
        2, 2),
  shape = c("square", "circle", "diamond", "diamond",
            "diamond", "diamond", "diamond", "diamond",
            "diamond", "diamond"),
  type = c("founder", "founder", "anchor", "anchor",
           "free", "free", "free", "free",
           "target", "target")
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
  x = c(0+off*scale.factor, 2-off*scale.factor, 1, 0, 2, 0, 2),
  xend = c(1, 1, 1, 1, 1, 0, 2),
  y = c(10, 10, 10, 9, 9, 9, 9),
  yend = c(10, 10, 9, 9, 9, 8+off, 8+off)
)

edges <- data.frame(
  x = c(0, 2, 0, 2, 0, 2),
  xend = c(0, 2, 0, 2, 0, 2),
  y = c(8, 8, 6, 6, 4, 4),
  yend = c(6, 6, 4, 4, 2, 2),
  linetype = c("solid", "solid","dashed", "dashed",
               "solid", "solid")
)

ggplot() +
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
  geom_point(data = data.frame(x = c(0, 2), y = c(2, 2)), aes(x, y)) +
  geom_segment(data = edges.top, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_segment(data = edges, aes(x = x, y = y-off, xend = xend, yend = yend+off),
               linetype = edges$linetype) +
  stat_brace(data = data.frame(x = c(0, 0), y = c(6.5, 3.5)), mapping = aes(x = x, y = y),
             rotate = 270, outerstart = -0.5, width = .5) +
  stat_brace(data = data.frame(x = c(2, 2), y = c(6.5, 3.5)), mapping = aes(x, y),
             rotate = 90, outerstart = 2.5, width = .5) +
  stat_bracetext(data.frame(x = c(2, 2), y = c(6.5, 3.5)), mapping = aes(x, y, label = TeX("$n_1$",italic=T)),
                 rotate = 270, outerstart = 0) +
  stat_bracetext(data = data.frame(x = c(2, 2), y = c(6.5, 3.5)), mapping = aes(x, y, label = TeX("$n_2$",italic=T)),
                 rotate = 90, outerstart = 2) +
  #geom_text(data = data.frame(x = c(1,1), y = c(0,0)), mapping = aes(x = x, y = y, label = TeX("$d=n_1+n_2+3$", italic = T))) +
  coord_equal(clip = "off") +
  scale_fill_manual(values = col.vals) +
  theme_void(base_size=11,
             base_family="serif") +
  theme(legend.position = "none",
        plot.margin = margin(0,0,0,0, unit = "in"),
        plot.title = element_text(hjust = 0.5, size = 14)) +
  labs(title = "C") +
  scale_y_continuous(limits = c(1, 11), expand = c(0, 0)) -> cousin
cousin

# Path plots
library(patchwork)

p <- (direct | hsib | avuncular| cousin) &
  theme(plot.tag = element_text(size = 24,family="sans"))
p
#ggsave("figures/peds_unilineal.emf")
#ggsave("figures/peds_unilineal.png")
#ggsave("figures/Figure_1.pdf", width = 200, height = 100, units = "mm", dpi = 1000, device = cairo_pdf)

