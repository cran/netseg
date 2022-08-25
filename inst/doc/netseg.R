## ----setup, include = FALSE---------------------------------------------------
library(netseg)
library(igraph)
requireNamespace("scales")

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width = "100%",
  fig.width=10,
  fig.height=6
)

set.seed(666)

## ----load-data----------------------------------------------------------------
data(Classroom)

## ----plot-data----------------------------------------------------------------
plot(
  Classroom,
  vertex.color = c("Skyblue", "Pink")[match(V(Classroom)$gender, c("Boy", "Girl"))],
  vertex.label = NA,
  vertex.size = 10,
  edge.arrow.size = .7
)
legend(
  "topright",
  pch = 21,
  legend = c("Boy", "Girl"),
  pt.bg = c("Skyblue", "Pink"),
  pt.cex = 2,
  bty = "n"
)

## ----data-undirected----------------------------------------------------------
undir <- as.undirected(Classroom, mode="mutual")
plot(
  undir,
  vertex.color = c("Skyblue", "Pink")[match(V(undir)$gender, c("Boy", "Girl"))],
  vertex.label = NA,
  vertex.size = 10,
  edge.arrow.size = .7
)
legend(
  "topright",
  pch = 21,
  legend = c("Boy", "Girl"),
  pt.bg = c("Skyblue", "Pink"),
  pt.cex = 2,
  bty = "n"
)

## ----mixing-matrix-2d---------------------------------------------------------
mixingm(Classroom, "gender")

## ----mixing-matrix-3d---------------------------------------------------------
m <- mixingm(Classroom, "gender", full=TRUE)
m

## ----mm_condprob--------------------------------------------------------------
round( prop.table(m, c(1,2)) * 100, 1)

## ----mm_contact_layer---------------------------------------------------------
round( prop.table(m[,,2], 1 ) * 100, 1)

## ----mixingm-undir------------------------------------------------------------
mixingm(undir, "gender")
mixingm(undir, "gender", full=TRUE)

## ----mixingdf-classroom-------------------------------------------------------
mixingdf(Classroom, "gender")
mixingdf(Classroom, "gender", full=TRUE)

## ----mixingdf-undir-----------------------------------------------------------
mixingdf(undir, "gender")
mixingdf(undir, "gender", full=TRUE)

## ----assort-------------------------------------------------------------------
assort(Classroom, "gender")
assort(undir, "gender")

## ----coleman------------------------------------------------------------------
coleman(Classroom, "gender")

## ----ei-----------------------------------------------------------------------
ei(Classroom, "gender")
ei(undir, "gender")

## ----freeman------------------------------------------------------------------
freeman(undir, "gender")

## ----gamix--------------------------------------------------------------------
gamix(Classroom, "gender")
gamix(undir, "gender")

## ----orwg---------------------------------------------------------------------
orwg(Classroom, "gender")
orwg(undir, "gender")

## ----smi----------------------------------------------------------------------
smi(Classroom, "gender")

## ----ssi----------------------------------------------------------------------
(v <- ssi(undir, "gender"))

## ----ssi-plot-----------------------------------------------------------------
kol <- gray(scales::rescale(v, 1:0))
plot(
  undir,
  vertex.shape = c("circle", "square")[match(V(undir)$gender, c("Boy", "Girl"))],
  vertex.color = kol,
  vertex.label = V(undir),
  vertex.label.color = ifelse(apply(col2rgb(kol), 2, mean) > 125, "black", "white"),
  vertex.size = 15,
  vertex.label.family = "sans",
  edge.arrow.size = .7
)

