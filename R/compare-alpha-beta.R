# Identify (loop through) combinations of alpha and beta to find agreements

nfl <- seq(0,10)
nml <- seq(0,10)

A <- function(nf,nm) {
  k1 = 1/2^(nf+nm)
  c(k1*nf,k1*nm)
}

Bf <- function(nf,nm) {
  k1 = 1/2^(nf+nm+1)
  c(k1*(nf+2),k1*nm)
}

Bm <- function(nf,nm) {
  k1 = 1/2^(nf+nm+1)
  c(k1*nf,k1*(nm+2))
}

Dff <- function(nf,nm) {
  k1 = 1/2^(nf+nm+2)
  c(k1*(nf+3),k1*(nm+1))
}
Dfm <- function(nf,nm) {
  k1 = 1/2^(nf+nm+2)
  c(k1*(nf+2),k1*(nm+2))
}

Dmm <- function(nf,nm) {
  k1 = 1/2^(nf+nm+2)
  c(k1*(nf+1),k1*(nm+3))
}

res <- data.frame(nf = factor(),
                  nm = factor(),
                  k1alpha = numeric(),
                  k2beta = numeric(),
                  bothequal = logical())

wrap <- function(class, nfl,nml) {
  res <- data.frame(class = character(),
                    nf = factor(),
                    nm = factor(),
                    k1alpha = numeric(),
                    k2beta = numeric())

  for (nf in nfl) {
    for (nm in nml) {
      if (class == "A") {
        val = A(nf,nm)
      } else if (class == "Bm") {
        val = Bm(nf,nm)
      } else if (class == "Bf") {
        val = Bf(nf,nm)
      } else if (class == "Dff") {
        val = Dff(nf,nm)
      } else if(class == "Dfm") {
        val = Dfm(nf,nm)
      } else if(class == "Dmm") {
        val = Dmm(nf,nm)
      }

      res <- rbind(res, data.frame(class = class,
                                   nf = nf,
                                   nm = nm,
                                   k1alpha = val[1],
                                   k1beta = val[2]))

    }
  }

  res
}

a <- wrap("A",nfl,nml)
bm <- wrap("Bm",nfl,nml)
bf <- wrap("Bf",nfl,nml)
dmm <- wrap("Dmm",nfl,nml)
dfm <- wrap("Dfm",nfl,nml)
dff <- wrap("Dff",nfl,nml)

df <- rbind(a,bm,bf,dmm,dfm,dff)

df %>%
  mutate(row_id = row_number()) %>%
  inner_join(
    df %>% mutate(row_id = row_number()),
    by = c("k1alpha", "k1beta"),
    suffix = c("_1", "_2")
  ) %>%
  filter(row_id_1 < row_id_2) -> dup
dup

# Adjusted case

nfl <- seq(0,10)
nml <- seq(0,10)

A <- function(nf,nm) {
  k1 = 1/2^(nf+nm)
  c(k1, nf, nm)
}

Bf <- function(nf,nm) {
  k1 = 1/2^(nf+nm+1)
  c(k1,nf+2,nm)
}

Bm <- function(nf,nm) {
  k1 = 1/2^(nf+nm+1)
  c(k1,nf,(nm+2))
}

Dff <- function(nf,nm) {
  k1 = 1/2^(nf+nm+2)
  c(k1,(nf+3),(nm+1))
}
Dfm <- function(nf,nm) {
  k1 = 1/2^(nf+nm+2)
  c(k1,(nf+2),(nm+2))
}

Dmm <- function(nf,nm) {
  k1 = 1/2^(nf+nm+2)
  c(k1,(nf+1),(nm+3))
}

res <- data.frame(nf = factor(),
                  nm = factor(),
                  k1 = numeric(),
                  alpha = numeric(),
                  beta = numeric(),
                  allequal = logical())

wrap <- function(class, nfl,nml) {
  res <- data.frame(class = character(),
                    nf = factor(),
                    nm = factor(),
                    k1 = numeric(),
                    alpha = numeric(),
                    beta = numeric())

  for (nf in nfl) {
    for (nm in nml) {
      if (class == "A") {
        val = A(nf,nm)
      } else if (class == "Bm") {
        val = Bm(nf,nm)
      } else if (class == "Bf") {
        val = Bf(nf,nm)
      } else if (class == "Dff") {
        val = Dff(nf,nm)
      } else if(class == "Dfm") {
        val = Dfm(nf,nm)
      } else if(class == "Dmm") {
        val = Dmm(nf,nm)
      }

      res <- rbind(res, data.frame(class = class,
                                   nf = nf,
                                   nm = nm,
                                   k1 = val[1],
                                   alpha = val[2],
                                   beta = val[3]))

    }
  }

  res
}

a <- wrap("A",nfl,nml)
bm <- wrap("Bm",nfl,nml)
bf <- wrap("Bf",nfl,nml)
dmm <- wrap("Dmm",nfl,nml)
dfm <- wrap("Dfm",nfl,nml)
dff <- wrap("Dff",nfl,nml)

df <- rbind(a,bm,bf,dmm,dfm,dff)

df %>%
  mutate(row_id = row_number()) %>%
  inner_join(
    df %>% mutate(row_id = row_number()),
    by = c("k1", "alpha", "beta"),
    suffix = c("_1", "_2")
  ) %>%
  filter(row_id_1 < row_id_2) -> dup
dup
