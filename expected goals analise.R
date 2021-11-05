# baixar a base  ####
library(readr)
library(tidyverse)
expected <- read_csv("expected.csv", col_types = cols(X6 = col_skip()))

##### dados ####
table(expected$resultado_) # para verificar se deu certo, tem que estar 50% para cada lado(empates excluídos)

# box plot ####

library(ggplot2)
library(RColorBrewer)

ggplot(data = expected, mapping = aes(x = expected, y = resultado_)) +
  geom_boxplot(fill='#A4A4A4', color="black") + coord_flip() + 
  xlab("Expected Goals") + ylab("Resultado") 




# médias ####

expected %>%
  group_by(resultado_) %>%
  summarise(media_X = mean(expected))

### correlations #####
options(scipen = 1000)
cor.test(expected$expected, expected$resultado)
library(polycor) #pacote exigido
polyserial(expected$expected, expected$resultado)

# logit ####

logit1 <- glm(resultado ~ expected, family = binomial(link = "logit"), data = expected)
summary(logit1)
summary(logit1, odds_ratios = TRUE)
library(sjPlot)
tab_model(logit1, show.ci = F, auto.label = T, show.se = T, collapse.se = T, wrap.labels = 60, p.style = "stars")

#% = (OR -1) * 100
(3.78-1)*100

library(coefplot)
coefplot(logit1, intercept = F)





# CURIOSIDADES: para ver a maior vitória e a maior derrota ####

soh_vitoria <- expected %>%
  filter(resultado_ == "vitoria") # primeiro só as vitórias
unique(soh_vitoria$resultado_)#para ver se deu certo
summary(soh_vitoria$expected) # ver o mínimo


soh_derrotas<- expected%>%
  filter(resultado_ == "derrota") # primeiro só as vitórias
unique(soh_derrotas$resultado_)#para ver se deu certo
summary(soh_derrotas$expected) # ver o máximo

#daí eu procurei na tabela qual é o jogo e peguei o time vencedor com menor valor e o perdedor com o maior 


# as simulações ZELIG para todos os dados ####
#Zelig

library(Zelig)
z.out1 <- zelig(resultado ~  expected , model = "logit", data = expected, cite = FALSE)
summary(z.out1, odds_ratios = TRUE) # para comparar
tab_model(logit1, show.ci = F, auto.label = T, show.se = T, 
          collapse.se = T, wrap.labels = 60, p.style = "stars") # só para comparar

# deu certo a comparação, now as simulações abaixo:
#   1     2         3         4       5      
#  Min. 1st Qu.  Median   3rd Qu.   Max. 
# 0.120   0.635   1.365   1.910   3.860 

ggplot(data = expected, mapping = aes(x = expected)) +
  geom_boxplot(fill='#A4A4A4', color="black") + coord_flip() + 
  xlab("Expected Goals") + coord_flip()


x.out1 <- Zelig::setx(z.out1,expected = 0.12)
s.out1 <- sim(z.out1, x = x.out1)
summary(s.out1)
plot(s.out1)

x.out2 <- Zelig::setx(z.out1,expected = 0.635)
s.out2 <- sim(z.out1, x = x.out2)
summary(s.out2)
plot(s.out2)

x.out3 <- Zelig::setx(z.out1,expected = 1.365)
s.out3 <- sim(z.out1, x = x.out3)
summary(s.out3)
plot(s.out3)

x.out4 <- Zelig::setx(z.out1,expected = 1.910)
s.out4 <- sim(z.out1, x = x.out4)
summary(s.out4)
plot(s.out4)

x.out5 <- Zelig::setx(z.out1,expected = 3.860)
s.out5 <- sim(z.out1, x = x.out5)
summary(s.out5)
plot(s.out5)



### fase da competição ####

#grupos

grupos <- expected%>%
  filter(fase == "grupos") # primeiro só as vitórias
unique(grupos$fase)#para ver se deu certo



logit2 <- glm(resultado ~ expected, family = binomial(link = "logit"), data = grupos)
tab_model(logit2, show.ci = F, auto.label = T, show.se = T, collapse.se = T, wrap.labels = 60, p.style = "stars")
#% = (OR -1) * 100
(4.93-1)*100
coefplot(logit2, intercept = F)

# mata mata
matamata <- expected%>%
  filter(fase == "matamata") # primeiro só as vitórias
unique(matamata$fase)#para ver se deu certo



logit3 <- glm(resultado ~ expected, family = binomial(link = "logit"), data = matamata)
tab_model(logit3, show.ci = F, auto.label = T, show.se = T, collapse.se = T, wrap.labels = 60, p.style = "stars")
coefplot(logit3, intercept = F)





