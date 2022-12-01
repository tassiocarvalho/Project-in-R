##################################################
###      Projeto avaliação da universidada     ###
##################################################

library(readxl)
BancoEstatistica <- read_excel("C:/Users/tassi/Desktop/BancoEstatistica.xlsx")
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

## Função que calcula as medidas descritivas ##
tabMedidasDesc = function (nome) {
  nomes <- c("mín","máx", "Média","Médiana","Amplitude","Desvio padrão", "Variância","coeficiente de variação")
  minimo <- min(nome)
  maximo <- max(nome)
  media <- mean(nome)
  mediana <- median(nome)
  amplitude <- diff(range(nome, na.rm=T))
  dp <- sd(nome)
  varian <- var(nome)
  cv <- sd(nome)/mean(nome)*100 
  valores <- c(minimo,maximo,media,mediana,amplitude,dp,varian,cv)
  tab <- matrix(valores,ncol=8,dimnames = list(c("Valores"),nomes))
  print(tab)
}

###########################################################################################
#### Trocando os nomes #####
BancoEstatistica <- trocandoNomeColuna("Periodo","periodo")
BancoEstatistica <- trocandoNomeColuna("Semestre que está cursando","semestre")
BancoEstatistica <- trocandoNomeColuna("Renda familiar","renda")
BancoEstatistica <- trocandoNomeColuna("Idade","idade")
BancoEstatistica <- trocandoNomeColuna("Sexo","sexo")
BancoEstatistica <- trocandoNomeColuna("Você trabalha ?","trabalho")
BancoEstatistica <- trocandoNomeColuna("Mora com quem ?","moradia")
BancoEstatistica <- trocandoNomeColuna("EstudoDiario","estudo")
BancoEstatistica <- trocandoNomeColuna("A infraestrutura das salas de aula e de estudo são adequadas?","salas")
BancoEstatistica <- trocandoNomeColuna("A infraestrutura das bibliotecas são boas?","infrabib")
BancoEstatistica <- trocandoNomeColuna("Como você avalia a estrutura geral da universidade?","estrutura")
BancoEstatistica <- trocandoNomeColuna("Como você avalia a internet da universidade?","internet")
BancoEstatistica <- trocandoNomeColuna("Como você avalia o laboratório da universidade?","laboratorio")
BancoEstatistica <- trocandoNomeColuna("Como você avalia a qualidade de ensino?","ensino")
BancoEstatistica <- trocandoNomeColuna("O atendimento nas secretarias são bons?","secretarias")
BancoEstatistica <- trocandoNomeColuna("No geral a relação professor aluno era boa e favorecia o processo de ensino-aprendizagem?","aprendizagem")
BancoEstatistica <- trocandoNomeColuna("No geral os métodos de avaliação são consistentes com os conteúdos apresentados?","avaliacao")
BancoEstatistica <- trocandoNomeColuna("Você se dedica as disciplinas mais de 3 horas por semana fora da sala de aula?","dedicacao")
BancoEstatistica <- trocandoNomeColuna("Você participa intensamente dos trabalhos em classe e fora de classe?","trabalhos")
BancoEstatistica <- trocandoNomeColuna("Você participa de mais de 70% das aulas?","participacao")


attach(BancoEstatistica)
names(BancoEstatistica)

slices <- table(semestre)
lbls <- names(slices)
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls,"-" ,pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices, labels = lbls, col=rainbow(length(lbls)), main="Semestre que está cursando: ")

#Gráfico de pizza de periodo#
slices <- table(periodo)
lbls <- names(slices)
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls,"-", pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices, labels = lbls, col=rainbow(length(lbls)), main="Periodo de estudo:")

#Gráfico de pizza de periodo#
slices <- table(idade)
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

slices <- table(renda)
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

slices <- table(estudo)
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

counts <- table(idade)
barplot(counts, main="Faixa etária dos alunos", xlab="Idade em anos", ylab = "Quantidade de alunos", col = rainbow(15))

counts <- table(participacao)
barplot(counts, main="Você participa de mais de 70% das aulas?", xlab="Sim/Não", ylab = "Quantidade de alunos", col = rainbow(15))

counts <- table(semestre)
barplot(counts, main="Distribuição de alunos por semestre", xlab="Semestre", ylab="Número de alunos", cex.names = 0.8, col = rainbow(15))

#Qual a probabilidade de um estudante de sexo feminino estudar no periódo noturno ou diurno? 
View(prop.table(table(sexo,periodo)))
p <- as.numeric(as.character(factor(prop.table(table(sexo,periodo)))))
#solução:
((p[1]+p[3])*100)

#Qual probabilidade de um estudante de renda acima de 6 a 8 salários participar de 70% das aulas?
View(prop.table(table(renda,participacao)))
p <- as.numeric(as.character(factor(prop.table(table(renda,participacao)))))
#solução
p[8]*100

#Qual a probabilidade de um aluno acima de 20 até 24 anos trabalhar e estudar acima de 6 a 9 horas?
View(prop.table(table(idade,trabalho,estudo)))
p <- as.numeric(as.character(factor(prop.table(table(idade,trabalho,estudo)))))
#solução:
p[26]*100

#Qual a probabilidade de um aluno que avalia laboratório da universidade ruim e avalia qualidade de ensino bom do sexo masculino?
View(prop.table(table(sexo,ensino,laboratorio)))
p <- as.numeric(as.character(factor(prop.table(table(sexo,ensino,laboratorio)))))
p[14]*100