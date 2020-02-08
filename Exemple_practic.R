## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message = F)


## ------------------------------------------------------------------------

source("rcode_zombies.R")
require("crayon")


set.seed(1)
tots <- c(rep("verd",6),rep("groc",4),rep("vermell",3)) 


TAULA <- NULL
(t1 <- jugar(daus = tots))
(t2 <- jugar(daus = t1$daus_restants, daus_obligatoris = t1$passos))
(t3 <- jugar(daus = t2$daus_restants, daus_obligatoris = t2$passos))

# (t4 <- jugar(daus = t3$daus_restants, daus_obligatoris = t3$passos))
# dau_paint(t4$resultat)
# dau_paint(TAULA)

