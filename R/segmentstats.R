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
    sims_training <- ibdrel:::ibdSimulations(peds, N = 5000, seed = seed)
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

lpeds <- list(pedtools::linearPed(2), pedtools::linearPed(3), pedtools::linearPed(4), pedtools::linearPed(5),
              pedtools::linearPed(6), pedtools::linearPed(7), pedtools::linearPed(8), pedtools::linearPed(9))
couspeds <- list(pedtools::cousinPed(1), pedtools::cousinPed(1, 1), pedtools::cousinPed(2), pedtools::cousinPed(2,1), pedtools::cousinPed(3), pedtools::cousinPed(3, 1), pedtools::cousinPed(4))
hcouspeds <- list(pedtools::halfSibPed(), pedtools::avuncularPed(half=TRUE), pedtools::cousinPed(1, half = T), pedtools::cousinPed(1, 1, half = T),
                  pedtools::cousinPed(2, half = T), pedtools::cousinPed(2, 1, half = T),
                  pedtools::cousinPed(3, half = T), pedtools::cousinPed(3, 1, half = T))
avpeds <- list(pedtools::avuncularPed(removal = 1), pedtools::avuncularPed(removal = 2), pedtools::avuncularPed(removal = 3), pedtools::avuncularPed(removal = 4), pedtools::avuncularPed(removal = 5), pedtools::avuncularPed(removal = 6),
               pedtools::avuncularPed(removal = 7), pedtools::avuncularPed(removal = 8))

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

saveRDS(lranges, file = "lranges.rds")
saveRDS(cousranges, file = "cousranges.rds")
saveRDS(hcousranges, file = "hcousranges.rds")
saveRDS(avpedranges, file = "avpedranges.rds")
saveRDS(pedranges, file = "pedranges.rds")

pedranges <- readRDS("data/pedranges.rds")

# ---- Segment lengths ----

# Identify examples of overlaps in terms of mean segment lengths
pedranges |> filter(type == "cousin", deg == 4) |> arrange(mean)
pedranges |> filter(type == "half-cousin", deg == 5) |> arrange(mean)
pedranges |> filter(type == "direct", deg == 6) |> arrange(mean)

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
                       levels = c("direct", "half-cousin", "avuncular", "cousin")),
         deg = factor(deg)) |>
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
  mutate(deg = factor(deg))

ggplot() +
  geom_linerange(data = endpoints, mapping = aes(y = deg, xmin = lower_value, xmax = upper_value, colour = deg),
                 alpha = .5, linewidth = 1.5) +
  geom_point(data = pedranges, mapping = aes(x = mean, y = deg, colour = deg, fill = deg)) +
  geom_point(data = subset(pedranges, mark == TRUE), mapping = aes(x = mean, y = deg),
             shape = 21, size = 2, colour = "grey40", fill = NA, stroke = 1) +
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
  theme_bw(base_size = 14) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9)) -> rangeplot
rangeplot

ggsave("figures/rangeplot.pdf", plot = rangeplot, dpi = 600, device = cairo_pdf, width = 9, height = 9)


# ---- Number of segments ----

pedranges <- pedranges |>
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
  theme_bw(base_size = 14) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9)) -> rangeplot
rangeplot

ggsave("figures/rangeplot_count.pdf", plot = rangeplot, dpi = 600, device = cairo_pdf, width = 9, height = 9)
