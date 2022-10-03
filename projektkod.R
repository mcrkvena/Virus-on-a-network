library("rJava")
library("RNetLogo")
setwd("Z:/Program Files/Everything/NetLogo/app")
nl.path <- getwd()
NLStart(nl.path)
model.path <- file.path("models", "Sample Models", "Networks", "Virus on a network.nlogo")
NLLoadModel(file.path(nl.path, model.path))
NLCommand("set number-of-nodes 100",
          "set average-node-degree 3",
          "set initial-outbreak-size 2",
          "set virus-spread-chance 100",
          "set virus-check-frequency 1",
          "set recovery-chance 2.5",
          "set gain-resistance-chance 20",
          "setup")

zarazeni <- NLDoReport(365, "go", "((count turtles with [infected?]) / (count turtles)) * 100")
print(unlist(zarazeni))
plot(1:365,
     unlist(zarazeni),
     xlab="Dan",
     ylab="Postotak zarazenih",
     type="l",
     lwd=3,
     col="red",
     main="Postotak zaraženih osoba COVID-19 virusom tijekom godine dana
     (prosjeèni kontakt s 3 osobe, 100% vjerojatnost zaraze, svakodnevne provjere,
     2,5% vjerojatnost oporavka, 20% vjerojatnost stjecanja imunosti)")

NLCommand("setup")
zarazeni <- NLDoReportWhile(
  "any? turtles with [infected?]", "go", c("ticks", "((count turtles with [infected?]) / (count turtles)) * 100"),
  as.data.frame = TRUE,
  df.col.names = c("Dan", "Postotak zarazenih"))
plot(zarazeni,
     type="l",
     lwd=3,
     col="red",
     main="Vrijeme potrebno da 0% ljudi bude zaraženo virusom COVID-19
     (prosjeèni kontakt s 3 osobe, 100% vjerojatnost zaraze, svakodnevne provjere,
     2,5% vjerojatnost oporavka, 20% vjerojatnost stjecanja imunosti)")

NLCommand("set number-of-nodes 100",
          "set average-node-degree 3",
          "set initial-outbreak-size 2",
          "set virus-spread-chance 100",
          "set virus-check-frequency 1",
          "set recovery-chance 1",
          "set gain-resistance-chance 20",
          "setup")

average_node_degree <- c(3:5)
zarazeni <- list()
for(i in seq_along(average_node_degree)){
  NLCommand("set average-node-degree", average_node_degree[i], "setup")
  zarazeni[[i]] <- NLDoReport(365,"go",
                              c("ticks", "((count turtles with [infected?]) / (count turtles)) * 100"),
                              as.data.frame=TRUE,
                              df.col.names = c("tick", "postotakzarazenih"))
}
zarazeni.dt <- data.table::rbindlist(Map(cbind, zarazeni, average_node_degree=3:5))
zarazeni.dt
library(ggplot2)
ggplot(zarazeni.dt,
       aes(x=tick,
           y=postotakzarazenih,
           group=factor(average_node_degree),
           color=factor(average_node_degree))) +
        geom_path(lwd=1) +
        theme_bw() +
        labs(x = "Dan", y = "Postotak zaraženih", color = "Broj osoba u kontaktu\n") +
        ggtitle("Usporedba broja zaraženih COVID-19 virusom s obzirom na broj osoba u kontaktu
        (100% vjerojatnost zaraze, svakodnevne provjere,
         1% vjerojatnost oporavka, 20% vjerojatnost stjecanja imunosti)")


sim <- function(average_node_degree){
  NLCommand("set average-node-degree", average_node_degree, "setup")
  NLDoCommand(365, "go");
  ret <- NLReport("((count turtles with [resistant?]) / (count turtles)) * 100")
  return(ret)
}

d <- seq(1, 10, 1)
pb = sapply(d, sim)
plot(d, pb,
     pch=19,
     type="b",
     col="blue",
     lwd=2,
     xlab="Broj osoba u kontaktu",
     ylab="Postotak imunih",
     main="Odnos broja imunih osoba s brojem osoba u kontaktu s agentom
        (100% vjerojatnost zaraze, svakodnevne provjere,
         1% vjerojatnost oporavka, 20% vjerojatnost stjecanja imunosti)")

rep.sim <- function(average_node_degree, rep) {
  lapply(average_node_degree, function(x) replicate(rep, sim(x)))
}

d <- seq(1, 10, 1)
res <- rep.sim(d, 10)
boxplot(res, names = d,
        col=c("green","lightgreen","yellow","red","red","red","red","red","red","red"),
        xlab="Broj osoba u kontaktu",
        ylab="Postotak imunih",
        main="Odnos broja imunih osoba s brojem osoba u kontaktu s agentom
        (100% vjerojatnost zaraze, svakodnevne provjere,
         1% vjerojatnost oporavka, 20% vjerojatnost stjecanja imunosti)")
NLQuit()
