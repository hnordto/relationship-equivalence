# ---- FIGURE 3 and S2 ----

library(pedtools)
library(ibdrel)
library(ibdsim2)
library(ggplot2)
library(forcats)
library(dplyr)
library(paletteer)
library(parallel)

# ---- Simulations ----

segmentranges <- function(pedlist) {

  df = data.frame(ped = character(),
                  rel = character(),
                  type = character(),
                  deg = factor(),
                  pathn = factor(),
                  mean = numeric())

  # Comment out and replace with map = loadMap() for Fig. 3
  #k = 20000/2875
  #map = ibdsim2::uniformMap(Mb = 20000, cM = c("male" = 2602.29*k, "female" = 4180.42*k))
  map = ibdsim2::loadMap()
  allsegs = list()

  for (i in 1:length(pedlist)) {
    ped = pedlist[[i]]
    rel = strsplit(ibdrel::annotatePedigree(ped), "-")[[1]][1]
    verb = verbalisr::verbalise(ped, ids = ibdrel::identifyLeaves(ped))[[1]]
    deg = verb$degree

    if (isTRUE(verb$full)) {
      type = verb$type
    } else {
      type = verb$type
      if (type == "lineal") {
        type = "direct"
      } else {
        type = "half-cousin"
      }
    }

    pathn = length(strsplit(verb$sexPath, "")[[1]])

    variants = ibdrel:::pedVariants(ped)
    peds = ibdrel::constructPedigrees(variants)

    seed = seq(1,length(peds))+20261702

    annotation = sapply(peds, ibdrel:::annotatePedigree)
    sims_training <- ibdrel:::ibdSimulations(peds, N = 5000, seed = seed, map = map)
    segments <- ibdrel:::lengthIBD(sims_training, peds, annotation)

    sapply(segments, function(x) {
      unlist(x) |> mean()
    }) -> means

    sapply(segments, function(x) {
      sapply(x, length) |> mean()
    }) -> counts

    segs <- lapply(segments, function(x) {
      unlist(x)
    })

    allsegs[names(means)] <- segs

    npeds = length(annotation)


    df <- rbind(df, data.frame(ped = annotation,
                               rel = rep(rel, npeds),
                               type = rep(type, npeds),
                               deg = rep(deg, npeds),
                               pathn = rep(pathn, npeds),
                               mean = means,
                               count = counts))
  }

  return (list(df = df, allsegs = allsegs))
}

lpeds <- list(pedtools::linearPed(2) |> swapSex(5), pedtools::linearPed(3) |> swapSex(7), pedtools::linearPed(4) |> swapSex(9), pedtools::linearPed(5) |> swapSex(11),
              pedtools::linearPed(6) |> swapSex(13), pedtools::linearPed(7) |> swapSex(15), pedtools::linearPed(8) |> swapSex(17), pedtools::linearPed(9) |> swapSex(19))
couspeds <- list(pedtools::cousinPed(1) |> swapSex(7), pedtools::cousinPed(1, 1) |> swapSex(7), pedtools::cousinPed(2) |> swapSex(11), pedtools::cousinPed(2,1) |> swapSex(11), pedtools::cousinPed(3) |> swapSex(15), pedtools::cousinPed(3, 1) |> swapSex(15), pedtools::cousinPed(4) |> swapSex(19))
hcouspeds <- list(pedtools::halfSibPed() |> swapSex(4), pedtools::avuncularPed(half=TRUE) |> swapSex(4), pedtools::cousinPed(1, half = T) |> swapSex(8), pedtools::cousinPed(1, 1, half = T) |> swapSex(8),
                  pedtools::cousinPed(2, half = T) |> swapSex(12), pedtools::cousinPed(2, 1, half = T) |> swapSex(12),
                  pedtools::cousinPed(3, half = T) |> swapSex(16), pedtools::cousinPed(3, 1, half = T) |> swapSex(16))
avpeds <- list(pedtools::avuncularPed(removal = 1) |> swapSex(3), pedtools::avuncularPed(removal = 2) |> swapSex(3), pedtools::avuncularPed(removal = 3) |> swapSex(3), pedtools::avuncularPed(removal = 4) |> swapSex(3), pedtools::avuncularPed(removal = 5) |> swapSex(3), pedtools::avuncularPed(removal = 6) |> swapSex(3),
               pedtools::avuncularPed(removal = 7) |> swapSex(3), pedtools::avuncularPed(removal = 8)|> swapSex(3))

inputs <- list(
  lpeds = lpeds,
  couspeds = couspeds,
  hcouspeds = hcouspeds,
  avpeds = avpeds
)

cl <- makeCluster(4)
clusterExport(cl, varlist = c("segmentranges"))

res <- parLapply(cl, inputs, segmentranges)

stopCluster(cl)

lranges      <- res[[1]]
cousranges   <- res[[2]]
hcousranges  <- res[[3]]
avpedranges  <- res[[4]]

pedranges <- rbind(lranges$df, cousranges$df, hcousranges$df, avpedranges$df)

#saveRDS(lranges, file = "lranges.rds")
#saveRDS(cousranges, file = "cousranges.rds")
#saveRDS(hcousranges, file = "hcousranges.rds")
#saveRDS(avpedranges, file = "avpedranges.rds")
#saveRDS(pedranges, file = "pedranges.rds")

#pedranges <- readRDS("data/pedranges_new.rds")

pedranges <- pedranges |>
  mutate(type = replace(type, type=="direct", "D"),
         type = replace(type, type=="half-cousin", "H"),
         type = replace(type, type=="avuncular", "A"),
         type = replace(type, type=="cousin", "C"))

# ---- Segment lengths ----

# Identify examples of overlaps in terms of mean segment lengths
pedranges |> filter(type == "C", deg == 4) |> arrange(mean)
pedranges |> filter(type == "H", deg == 5) |> arrange(mean)
pedranges |> filter(type == "A", deg == 6) |> arrange(mean)

ped = cousinPed(1, removal = 1, half = T) |> swapSex(c(2,4))
ibdrel::annotatePedigree(ped)

ped = cousinPed(1, removal = 1, half = T) |> swapSex(c(4,6,9))
ibdrel::annotatePedigree(ped)

ped = cousinPed(1, removal = 1) |> swapSex(c(3,8))
ibdrel::annotatePedigree(ped)

ped = cousinPed(1, removal = 1) |> swapSex(c(3,5))
ibdrel::annotatePedigree(ped)

pedranges <- pedranges |>
  mutate(mark = ifelse(ped %in% c("1C1R-mpm", "1C1R-mmp",
                                  "h1C1R-mmpp", "h1C1R-mpmm",
                                  "L6-mmmpp"), TRUE, NA))

# Rangeplot

pedranges <- pedranges |>
  mutate(type = factor(type,
                       levels = c("D", "H", "A", "C")),
         deg = factor(deg, levels = seq(1,11))) |>
  mutate(mark = ifelse(ped %in% c("1C1R-mpm", "1C1R-mmp",
                                  "h1C1R-mmpp", "h1C1R-mpmm",
                                  "L6-mmmpp"), TRUE, NA))

endpoints <- pedranges |>
  group_by(rel, type, deg) |>
  summarise(lower_variant = ped[which.min(mean)],
            lower_value = min(mean),
            upper_variant = ped[which.max(mean)],
            upper_value = max(mean),
            .groups = "drop") |>
  mutate(lower_symbol = "♀",
         upper_symbol = "♂") |>
  mutate(deg = factor(deg, levels = seq(1,11)))

ggplot() +
  geom_linerange(data = endpoints, mapping = aes(y = deg, xmin = lower_value, xmax = upper_value, colour = deg),
                 alpha = .5, linewidth = 1.5) +
  geom_point(data = pedranges, mapping = aes(x = mean, y = deg, colour = deg, fill = deg)) +
  geom_point(data = subset(pedranges, mark == TRUE), mapping = aes(x = mean, y = deg),
             shape = 21, size = 2, colour = "black", fill = NA, stroke = 1) +
  facet_grid(rows = vars(type), scales = "free_y", switch = "y") +
  labs(x = "Mean segment length (cM)",
       y = "Degree",
       colour = "Degree") +
  scale_x_continuous(breaks = c(7.5,seq(10,75, by = 5)),
                     labels = c(7.5, seq(10, 75, by = 5)),
                     limits = c(7.5, 75),
                     transform = "log2",
                     expand = c(0,0)) +
  scale_colour_paletteer_d("MoMAColors::Klein") +
  theme_bw(base_size = 11) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        strip.text.y = element_text(size = 11)) -> rangeplot
rangeplot

#ggsave("figures/Figure_3.pdf", plot = rangeplot, dpi = 1000, device = cairo_pdf,
#       width = 200, height = 200, unit = "mm")


# ---- Number of segments ----

pedranges <- pedranges |>
  mutate(type = factor(type,
                       levels = c("D", "H", "A", "C")),
         deg = factor(deg))

endpoints <- pedranges |>
  group_by(rel, type, deg) |>
  summarise(lower_variant = ped[which.min(count)],
            lower_value = min(count),
            upper_variant = ped[which.max(count)],
            upper_value = max(count),
            .groups = "drop") |>
  mutate(lower_symbol = "♀",
         upper_symbol = "♂") |>
  mutate(deg = factor(deg))

ggplot() +
  geom_linerange(data = endpoints, mapping = aes(y = deg, xmin = lower_value, xmax = upper_value, colour = deg),
                 alpha = .5, linewidth = 1.5) +
  geom_point(data = pedranges, mapping = aes(x = count, y = deg, colour = deg, fill = deg)) +
  facet_grid(rows = vars(type), scales = "free_y", switch = "y") +
  labs(x = "Mean number of segments",
       y = "Degree",
       colour = "Degree") +
  scale_x_continuous(breaks = c(1,2,5,seq(5,60, by = 5)),
                     labels = c(1,2,5,seq(5,60,by=5)),
                     limits = c(0.9, 60),
                     transform = "log2",
                     expand = c(0,0)) +
  scale_colour_paletteer_d("MoMAColors::Klein") +
  theme_bw(base_size = 11) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        strip.text.y = element_text(size = 11)) -> rangeplot
rangeplot

#ggsave("figures/Figure_S2.pdf", plot = rangeplot, dpi = 1000, device = cairo_pdf, width = 200, height = 200,
#       units = "mm")

# ---- Number of segments: With adjustment correcting for boundary effects (a single long chromosome) ----
# Re-run simulations above, with the custom uniform genetic map.

cols <- paletteer_d("MoMAColors::Klein")[1:5]

pedranges <- pedranges |>
  mutate(mark1 = ifelse(ped %in% c("2C0R-mppp", "2C0R-pmpp",
                                  "L4-mpp"), T, F),
         mark2 = ifelse(ped %in% c("2C0R-mmpm", "2C0R-mmmp", "L4-mmp"), T, F),
         mark3 = ifelse(ped %in% c("L2-m", "L3-m", "h1C0R-m"), T, F),
         mark4 = ifelse(ped %in% c("L2-p", "L3-p", "h1C0R-p"), T, F),
         mark5 = ifelse(ped %in% c("L3-mp", "h1C0R-pmp", "h1C0R-mpm"), T, F)) |>
  mutate(id = row_number()) |>
  pivot_longer(
    cols = starts_with("mark"),
    names_to = "mark_type",
    values_to = "marked"
  ) |>
  filter(marked) |>
  select(-marked) |>
  group_by(id) |>
  summarise(mark = paste(mark_type, collapse = "'"), .groups = "drop") |>
  right_join(
    pedranges |> mutate(id = row_number()),
    by = "id"
  ) |>
  mutate(mark = replace_na(mark, "FALSE")) |>
  select(-id) |>
  mutate(type = factor(type,
                       levels = c("direct", "half-cousin", "avuncular", "cousin")),
         deg = factor(deg))

endpoints <- pedranges |>
  group_by(rel, type, deg) |>
  summarise(lower_variant = ped[which.min(count)],
            lower_value = min(count),
            upper_variant = ped[which.max(count)],
            upper_value = max(count),
            .groups = "drop") |>
  mutate(deg = factor(deg)) |>
  mutate(mark = F)
  #mutate(mark = ifelse(rel %in% c("L2", "L3", "h1C0R"), "mark6", FALSE))

ggplot() +
  geom_linerange(data = endpoints, mapping = aes(y = deg, xmin = lower_value, xmax = upper_value, colour = mark),
                 alpha = .5, linewidth = 1.5) +
  geom_point(data = pedranges, mapping = aes(x = count, y = deg, colour = mark, fill = mark)) +
  facet_grid(rows = vars(type), scales = "free_y", switch = "y") +
  labs(x = "Mean number of segments",
       y = "Degree",
       colour = "Degree") +
  scale_x_continuous(breaks = c(5,seq(10,320, by = 50)),
                     labels = c(5,seq(10,320, by = 50)),
                     limits = c(5, 320),
                     transform = "log2",
                     expand = c(0,0)) +
  scale_colour_manual(values = c("grey80", cols)) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9)) -> rangeplot
rangeplot

#ggsave("figures/rangeplot_count.pdf", plot = rangeplot, dpi = 600, device = cairo_pdf, width = 9, height = 9)
