##################################################
###      Projeto avaliação da universidade     ###
##################################################


#################### Alunos ######################
##        Alex da Fonseca Dantas Júnior         ##
##         Bruno Campos de Oliveira Rocha       ##
##          Tassio Carvalho Rodrigues           ##
##           Hugo Ribeiro De O. Filho           ##
##################################################


if(!require(dplyr))
  install.packages("dplyr")
if(!require(car))
  install.packages("car")
if(!require(psych))
  install.packages("psych")
if(!require(pacman))
  install.packages("pacman")
if(!require(tidyverse))
  install.packages("tidyverse")
if(!require(ggplot2))
  install.packages("ggplot2")

#################### Carregando pacotes ####################
require (ggplot2)
require(tidyverse)
require(dplyr)
require(car)
require(psych)
pacman::p_load(ggplot2,dplyr)

library(readxl)
#setwd("C:/Users/BRUNO/git/Project-in-R/Database")
setwd("C:/Users/tassi/Desktop/Banco/UTF-8")
BancoEstatistica <- read.csv('BancoEstatistica.csv', sep = ';', dec = ',', stringsAsFactors = T)

View(BancoEstatistica)

# pacotes instalados:
#install.packages('readxl') #importar banco em excel
#install.packages('descr') #crosstable e teste qui-quadrado
library(readxl) #importar banco em excel
library(descr)#crosstable

names(BancoEstatistica)
attach(BancoEstatistica)
View(BancoEstatistica)

###################### FUNÇÕES #########################
### Funções para reduzir a necessidade de repetir código
## Função que troca o nome da coluna ##
trocandoNomeColuna <- function (nome, novoNome) {
  x = BancoEstatistica
  i <- grep(nome, colnames(x))
  names(x)[i] <- c(novoNome)
  return(x)
}

###########################################################################################
#### Trocando os nomes #####
BancoEstatistica <- trocandoNomeColuna("Periodo","periodo")
BancoEstatistica <- trocandoNomeColuna("Semestre.que.está.cursando","semestre")
BancoEstatistica <- trocandoNomeColuna("Renda.familiar","renda")
BancoEstatistica <- trocandoNomeColuna("Idade","idade")
BancoEstatistica <- trocandoNomeColuna("Sexo","sexo")
BancoEstatistica <- trocandoNomeColuna("Você.trabalha.?","trabalho")
BancoEstatistica <- trocandoNomeColuna("Mora.com.quem.?","moradia")
BancoEstatistica <- trocandoNomeColuna("Tempo.de.estudo.diário","estudo")
BancoEstatistica <- trocandoNomeColuna("A.infraestrutura.das.salas.de.aula.e.de.estudo.são.adequadas?","salas")
BancoEstatistica <- trocandoNomeColuna("A.infraestrutura.das.bibliotecas.são.boas?","infrabib")
BancoEstatistica <- trocandoNomeColuna("Como.você.avalia.a.estrutura.geral.da.universidade?","estrutura")
BancoEstatistica <- trocandoNomeColuna("Como.você.avalia.a.internet.da.universidade?","internet")
BancoEstatistica <- trocandoNomeColuna("Como.você.avalia.o.laboratório.da.universidade?","laboratorio")
BancoEstatistica <- trocandoNomeColuna("Como.você.avalia.a.qualidade.de.ensino?","ensino")
BancoEstatistica <- trocandoNomeColuna("O.atendimento.nas.secretarias.são.bons?","secretarias")
BancoEstatistica <- trocandoNomeColuna("No.geral.a.relação.professor.aluno.era.boa.e.favorecia.o.processo.de.ensino.aprendizagem.","aprendizagem")
BancoEstatistica <- trocandoNomeColuna("No.geral.os.métodos.de.avaliação.são.consistentes.com.os.conteúdos.apresentados?","avaliacao")
BancoEstatistica <- trocandoNomeColuna("Você.se.dedica.as.disciplinas.mais.de.3.horas.por.semana.fora.da.sala.de.aula?","dedicacao")
BancoEstatistica <- trocandoNomeColuna("Você.participa.intensamente.dos.trabalhos.em.classe.e.fora.de.classe?","trabalhos")
BancoEstatistica <- trocandoNomeColuna("Você.participa.de.mais.de.70..das.aulas.","participacao")

attach(BancoEstatistica)
names(BancoEstatistica)

####################################################
#### Estrutura para gerar os gráficos de pizza #####

slices <- table(cut(BancoEstatistica$semestre, seq(1,15, l = 8)))
lbls <- names(slices)
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls,"-" ,pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices, labels = lbls, col=rainbow(length(lbls)), main="Semestre que está cursando: ")

slices <- table(periodo)
lbls <- names(slices)
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls,"-", pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices, labels = lbls, col=rainbow(length(lbls)), main="Periodo de estudo:")

slices <- table(cut(BancoEstatistica$idade, seq(18,42, l = 7 )))
lbls <- names(slices)
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls,"-", pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices, labels = lbls, col=rainbow(length(lbls)), main="Idade dos alunos:")

slices <- table(sexo)
lbls <- names(slices)
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls,"-", pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices, labels = lbls, col=rainbow(length(lbls)), main="Sexo dos alunos:")

slices <- table(cut(BancoEstatistica$renda, seq(0.5,8.5, l = 8 )))
lbls <- names(slices)
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls," - ", pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices, labels = lbls, col=rainbow(length(lbls)), main="Salário familiar:")

slices <- table(trabalho)
lbls <- names(slices)
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls,"-", pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices, labels = lbls, col=rainbow(length(lbls)), main="Alunos que trabalham:")

slices <- table(moradia)
lbls <- names(slices)
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls,"-", pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices, labels = lbls, col=rainbow(length(lbls)), main="Os alunos moram com:")

slices <- slices <- table(cut(BancoEstatistica$estudo, seq(0,16, l = 8 )))
lbls <- names(slices)
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls,"-", pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices, labels = lbls, col=rainbow(length(lbls)), main="Horas de estudo dos alunos:")

slices <- table(salas)
lbls <- names(slices)
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls,"-", pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices, labels = lbls, col=rainbow(length(lbls)), main="As infraestruturas das salas de aula e de estudo são adequadas?")

slices <- table(infrabib)
lbls <- names(slices)
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls,"-", pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices, labels = lbls, col=rainbow(length(lbls)), main="A infraestrutura das bibliotecas são boas?")

slices <- table(estrutura)
lbls <- names(slices)
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls,"-", pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices, labels = lbls, col=rainbow(length(lbls)), main="Avaliação da estrutura geral da universidade")

slices <- table(internet)
lbls <- names(slices)
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls,"-", pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices, labels = lbls, col=rainbow(length(lbls)), main="Como você avalia a internet da universidade?")

slices <- table(laboratorio)
lbls <- names(slices)
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls,"-", pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices, labels = lbls, col=rainbow(length(lbls)), main="Como você avalia o laboratório da universidade?")

slices <- table(ensino)
lbls <- names(slices)
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls,"-", pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices, labels = lbls, col=rainbow(length(lbls)), main="Como você avalia a qualidade de ensino?")

slices <- table(secretarias)
lbls <- names(slices)
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls,"-", pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices, labels = lbls, col=rainbow(length(lbls)), main="O atendimento nas secretarias são bons?")

slices <- table(aprendizagem)
lbls <- names(slices)
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls,"-", pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices, labels = lbls, col=rainbow(length(lbls)), main="No geral a relação professor aluno era boa e favorecia o processo de ensino-aprendizagem?")

slices <- table(avaliacao)
lbls <- names(slices)
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls,"-", pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices, labels = lbls, col=rainbow(length(lbls)), main="No geral os métodos de avaliação são consistentes com os conteúdos apresentados?")

slices <- table(dedicacao)
lbls <- names(slices)
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls,"-", pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices, labels = lbls, col=rainbow(length(lbls)), main="Você se dedica as disciplinas mais de 3 horas por semana fora da sala de aula?")

slices <- table(trabalho)
lbls <- names(slices)
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls,"-", pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices, labels = lbls, col=rainbow(length(lbls)), main="Você participa intensamente dos trabalhos em classe e fora de classe?")

slices <- table(participacao)
lbls <- names(slices)
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls,"-", pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices, labels = lbls, col=rainbow(length(lbls)), main="Você participa de mais de 70% das aulas?")

#### Medidas descritivas ####

View(describe(summary(BancoEstatistica$idade)))
View(describe(summary(BancoEstatistica$semestre)))
View(describe(summary(BancoEstatistica$renda)))
View(describe(summary(BancoEstatistica$estudo)))

#### Graficos de barras utilizados####

counts <- table(cut(BancoEstatistica$idade, seq(18,42, l = 7 )))
barplot(counts, main="Faixa etária dos alunos", xlab="Idade em anos", ylab = "Número de alunos", col = rainbow(15))

counts <- table(cut(BancoEstatistica$renda, seq(0.5,8.5, l = 8 )))
barplot(counts, main="Renda familiar dos alunos", xlab="Salário", ylab = "Número de alunos", col = rainbow(15))

counts <- table(cut(BancoEstatistica$estudo, seq(0,16, l = 8 )))
barplot(counts, main="Hora de estudos dos alunos", xlab="Horas", ylab = "Número de alunos", col = rainbow(15))

counts <- table(cut(BancoEstatistica$semestre, seq(1,15, l = 8)))
barplot(counts, main="Distribuição de alunos por semestre", xlab="Semestre", ylab="Número de alunos", cex.names = 0.8, col = rainbow(15))

#Relações de probabilidade dos dados da pesquisa

#Tabela de frequencia para entender a relação do sexo dos alunos e do 
#periodo em que estudam e a sua probabilidade de ocorrencia
View(prop.table(table(sexo,periodo)))
p <- as.numeric(as.character(factor(prop.table(table(sexo,periodo)))))
resposta1 <- ((p[1]+p[3])*100)
#solução:
paste(resposta1, "%")

#Tabela de frequencia para entender a relação da renda familiar e participação de 70%
#dos alunos nas aulas e a sua probabilidade de ocorrencia
View(prop.table(table(renda,participacao)))
p <- as.numeric(as.character(factor(prop.table(table(renda,participacao)))))
resposta2 <- (p[33]*100)+(p[34]*100)+(p[35]*100)
#solução
paste(resposta2, "%")

#Tabela de frequencia para entender a relação do sexo do aluno, da qualidade de ensino 
#e como avaliam os laboratorios e sua probabilidade  de ocorrencia
View(prop.table(table(sexo,ensino,laboratorio)))
p <- as.numeric(as.character(factor(prop.table(table(sexo,ensino,laboratorio)))))
resposta3 <- p[14]*100
#solução:
paste(resposta3, "%")


#Tabela de frequencia para entender a relação do aluno com trabalho e participação 
#de 70% das aulas e sua probabilidade de ocorrencia
View(prop.table(table(trabalho, participacao)))
p <- as.numeric(as.character(factor(prop.table(table(trabalho, participacao)))))
resposta4 <- p[4]*100
#Qual a chance de um aluno trabalhar e participar de mais de 70% das aulas?
#solução:
paste(resposta4, "%")

#Tabela de frequencia para entender a relação do sexo do aluno com a participação 
#nas atividades extra curriculares e do trabalho e sua probabilidade de ocorrencia
View(prop.table(table(sexo, trabalhos,trabalho)))
p <- as.numeric(as.character(factor(prop.table(table(sexo, trabalhos)))))
resposta5 <- p[3]*100
#solução:
paste(resposta5, "%")
