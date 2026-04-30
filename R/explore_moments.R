library(pedtools)
library(ibdsim2)

simAndPlot <- function(pedlist) {
  res <- list()
  for (i in 1:length(pedlist)) {
    ped = pedlist[[i]]

    sim = ibdsim(ped, N = 1000, ids = ibdrel::identifyLeaves(ped))

    res <- append(res, list(sim))

  }

  plot <- plotSegmentDistribution(res, type = "ibd1")
  return (list(sim = res, plot = plot))
}


# A(Nf,Nm+2)=B_m(Nf,Nm)

a <- linearPed(3)
b <- halfSibPed(type = "paternal")

res <- simAndPlot(list(a,b))

l <- lengthIBD(list(res$sim[[1]],res$sim[[2]]),list(a,b))


# D_mm(2,2)=D_mf(1,3)

a <- cousinPed(3) |> swapSex(c(11,13))
b <- cousinPed(3) |> swapSex(c(3,11))

res <- simAndPlot(list(a,b))
