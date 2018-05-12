library(bnlearn)

dag <- empty.graph(nodes = c("EquipeExp", "GerenteExp", "Transacoes", 
                             "Entidades", "FatoresHumanos", "FatoresTecnicos", "Esforco"))

dag <- set.arc(dag, from = "EquipeExp", to = "FatoresHumanos")
dag <- set.arc(dag, from = "GerenteExp", to = "FatoresHumanos")
dag <- set.arc(dag, from = "Transacoes", to = "FatoresTecnicos")
dag <- set.arc(dag, from = "Entidades", to = "FatoresTecnicos")
dag <- set.arc(dag, from = "FatoresHumanos", to = "Esforco")
dag <- set.arc(dag, from = "FatoresTecnicos", to = "Esforco")

arc.set <- matrix(c("EquipeExp", "FatoresHumanos", 
                    "GerenteExp", "FatoresHumanos", 
                    "Transacoes", "FatoresTecnicos", 
                    "Entidades", "FatoresTecnicos", 
                    "FatoresHumanos", "Esforco", 
                    "FatoresTecnicos", "Esforco"), 
                  byrow = TRUE, ncol = 2, 
                  dimnames = list(NULL, c("from", "to")))

arcs(dag) <- arc.set

library(Rgraphviz)

graphviz.plot(dag)

EquipeExp.lv <- c("EqBxExp", "EqMedExp", "EqAltaExp")
GerenteExp.lv <- c("GerBxExp", "GerMedExp", "GerAltaExp")
Transacoes.lv <- c("TrsBxQt", "TrsMedQt", "TrsAltaQt")
Entidades.lv <- c("EntBxQt", "EntMedQt", "EntAltaQt")
FatoresHumanos.lv <- c("FHBxExp", "FHMedExp", "FHAltaExp")
FatoresTecnicos.lv <- c("FTBxExp", "FTMedExp", "FTAltaExp")
Esforco.lv <- c("Baixo", "Medio", "Alto")

EquipeExp.prob <- array(c(0.34, 0.4, 0.26), 
                        dim = 3, dimnames = list(EquipeExp = EquipeExp.lv))
GerenteExp.prob <- array(c(0.25, 0.65, 0.1), 
                         dim = 3, dimnames = list(GerenteExp = GerenteExp.lv))
Transacoes.prob <- array(c(0.7, 0.25, 0.05), 
                         dim = 3, dimnames = list(Transacoes = Transacoes.lv))
Entidades.prob <- array(c(0.7, 0.2, 0.1), 
                        dim = 3, dimnames = list(Entidades = Entidades.lv))
FatoresHumanos.prob <- array(c(1, 0, 0, 0.4, 0.6, 0, 0, 1, 0, 0.6, 0.4, 0, 0, 1, 0, 0, 0.4, 0.6, 0, 1, 0, 0, 0.6, 0.4, 0, 0, 1), 
                             dim = c(3, 3, 3), dimnames = list(FatoresHumanos = FatoresHumanos.lv, EquipeExp = EquipeExp.lv, GerenteExp = GerenteExp.lv))
FatoresTecnicos.prob <- array(c(1, 0, 0, 0.3, 0.6, 0.1, 0.15, 0.6, 0.25, 0.2, 0.6, 0.2, 0.2, 0.6, 0.2, 0, 0.6, 0.4, 0.1, 0.65, 0.25, 0.1, 0.5, 0.4, 0, 0, 1), 
                              dim = c(3, 3, 3), dimnames = list(FatoresTecnicos = FatoresTecnicos.lv, Transacoes = Transacoes.lv, Entidades = Entidades.lv))
Esforco.prob <- array(c(0.70, 0.2, 0.10, 0.85, 0.10, 0.05, 0.9, 0.09, 0.01, 0.17, 0.68, 0.15, 0.7, 0.2, 0.1, 0.75, 0.15, 0.1, 0, 0.2, 0.8, 0.1, 0.25, 0.65, 0.05, 0.70, 0.25), 
                      dim = c(3, 3, 3), dimnames = list(Esforco = Esforco.lv, FatoresHumanos = FatoresHumanos.lv, FatoresTecnicos = FatoresTecnicos.lv))

# duly-prepared data
desharnais <- read.table("desharnais.txt", header = TRUE)

arc.strength(dag, data = desharnais, criterion = "mi")
arc.strength(dag, data = desharnais, criterion = "x2")

cpt <- list(EquipeExp = EquipeExp.prob, GerenteExp = GerenteExp.prob, Transacoes = Transacoes.prob, Entidades = Entidades.prob, 
            FatoresHumanos = FatoresHumanos.prob, FatoresTecnicos = FatoresTecnicos.prob, Esforco = Esforco.prob)

bn <- custom.fit(dag, cpt)

library(gRain)

junction <- compile(as.grain(bn))

i = 0;
while(i < 76) {
  i <- i + 1;
  
  evd <- setEvidence(junction, nodes = c("EquipeExp", "GerenteExp", "Transacoes", "Entidades"),
                     states = c(as.character(desharnais[i, 1]), 
                                as.character(desharnais[i, 2]), 
                                as.character(desharnais[i, 3]), 
                                as.character(desharnais[i, 4])))
  
  print(querygrain(evd, nodes = "Esforco", type = "joint"))
}