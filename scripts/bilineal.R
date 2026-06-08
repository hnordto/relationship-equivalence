library(pedtools)
library(ibdrel)
library(ibdsim2)

ped <- pedtools::readPed("d2c.ped")
plot(ped, arrows=F)

ped2 <- ped |> swapSex(5)
ped3 <- ped |> swapSex(7)
ped4 <- ped |> swapSex(10)
ped5 <- ped |> swapSex(12)

peds <- list(ped, ped2, ped3)

annot <- c("ped1", "ped2", "ped3")
meta <- pedsMetadata(peds)


sim = ibdsim2::ibdsim(ped, N = 5000, map = map)
sim2 = ibdsim2::ibdsim(ped2, N = 5000, map = map)
sim3 = ibdsim2::ibdsim(ped3, N = 5000, map = map)
sim4 = ibdsim2::ibdsim(ped4, N = 5000, map = map)
sim5 = ibdsim2::ibdsim(ped5, N = 5000, map = map)


plotSegmentDistribution(list(sim2, sim3, sim4, sim5), type = "ibd1", ids = c(17,18))

# Bad example, due to symmetry


ped <- pedtools::readPed("bilineal2.ped")
plot(ped)
verbalisr::verbalise(ped, ids = c(12,14))

ped2 <- ped |> swapSex(3)
ped3 <- ped |> swapSex(5)
ped4 <- ped |> swapSex(7)
ped5 <- ped |> swapSex(11)

sim2 = ibdsim2::ibdsim(ped2, N = 5000)
sim3 = ibdsim2::ibdsim(ped3, N = 5000)
sim4 = ibdsim2::ibdsim(ped4, N = 5000)
sim5 = ibdsim2::ibdsim(ped5, N = 5000)

plotSegmentDistribution(list(sim2, sim3, sim4, sim5), type = "ibd1", ids = c(9,13))

ped <- pedtools::readPed("bilineal3.ped")

verbalisr::verbalise(ped)

ped2 <- ped |> swapSex(10)
ped3 <- ped |> swapSex(8)
ped4 <- ped |> swapSex(5)
ped5 <- ped |> swapSex(2)

sim2 = ibdsim2::ibdsim(ped2, N = 5000)
sim3 = ibdsim2::ibdsim(ped3, N = 5000)
sim4 = ibdsim2::ibdsim(ped4, N = 5000)
sim5 = ibdsim2::ibdsim(ped5, N = 5000)

plotSegmentDistribution(list(sim2, sim3, sim4, sim5), type = "ibd1", ids = c(14,16))


ped <- pedtools::readPed("bilineal5.ped")
plot(ped)

ped2 <- ped |> swapSex()

plot(ped)



ped <- pedtools::readPed("ped6.ped")
plot(ped)

ped2 <- ped |> swapSex(8)
ped3 <- ped |> swapSex(7)

sim2 = ibdsim2::ibdsim(ped2, N = 1000)
sim3 = ibdsim2::ibdsim(ped3, N = 1000)
plotSegmentDistribution(list(sim2, sim3), type = "ibd1", ids = c(12,14))


ped <- pedtools::readPed("bilineal8.ped")
verbalisr::verbalise(ped)

par(mfrow=c(1,2))
plot(ped, fill = list(red = c(16,12,11,7,8,14,18)),
     labs = NULL, cex = 2,
     hatched = c(12,14))
plot(ped, fill = list(blue = c(16,12,5,3,6,8,14,18)),
     labs = NULL, cex = 2,
     hatched = c(12,14))

map = data.frame(chrom = c(1,1),
                 mb = c(0, 2000),
                 male = c(0, 500),
                 female = c(0, 1500))
map = customMap(map)


ped2 <- ped |> swapSex(12)
ped3 <- ped |> swapSex(14)

sim2 = ibdsim2::ibdsim(ped2, N = 5000, map = map, seed = 202502101)
sim3 = ibdsim2::ibdsim(ped3, N = 5000, map = map, seed = 202502102)

plotSegmentDistribution(list(sim2, sim3), type = "ibd1", ids = c(16,18))

ped <- pedtools::readPed("bilineal9.ped")
plot(ped, fill = list(red = c(12,7,11,5,14)))
plot(ped, fill = list(blue = c(12,8,4,3,5,14)))


ped <- pedtools::readPed("bilineal10.ped")
plot(ped)

ped2 <- ped |> swapSex(4)
ped3 <- ped |> swapSex(6)

sim2 = ibdsim2::ibdsim(ped2, N = 5000, map = map, seed = 202502101)
sim3 = ibdsim2::ibdsim(ped3, N = 5000, map = map, seed = 202502102)
plotSegmentDistribution(list(sim2, sim3), type = "ibd1", ids = c(11,14))

ped <- pedtools::readPed("bilineal11.ped")
plot(ped)
verbalisr::verbalise(ped)

ped2 <- ped |>  swapSex(5)
ped3 <- ped |> swapSex(7)
sim2 = ibdsim2::ibdsim(ped2, N = 5000, map = map, seed = 202502101)
sim3 = ibdsim2::ibdsim(ped3, N = 5000, map = map, seed = 202502102)
plotSegmentDistribution(list(sim2, sim3), type = "ibd1", ids = c(17,20))


# HS + C

ped1 <- pedtools::readPed("2chS.ped")
ped2 <- pedtools::readPed("1c2rhS.ped")

sim1 = ibdsim2::ibdsim(ped1, N = 5000, map = map, seed = 202502101, ids = c(10,11))
sim2 = ibdsim2::ibdsim(ped2, N = 5000, map = map, seed = 202502101, ids = c(6,14))

segments1 = ibdsim2::findPattern(sim1, pattern = list(carriers = c(10,11))) |> segmentStats()
segments2 = ibdsim2::findPattern(sim2, pattern = list(carriers = c(6,14))) |> segmentStats()

library(ggplot2)
ggplot() +
  geom_jitter(data = segments1$perSim, mapping = aes(x = Count, y = Average), colour = "blue") +
  geom_jitter(data = segments1$perSim, mapping = aes(x = Count, y = Average), colour = "red")
