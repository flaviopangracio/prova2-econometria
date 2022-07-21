## Instalando e carregando pacotes
install.packages("readxl")
install.packages("plm")
install.packages("lmtest")
install.packages("reactable")

library(readxl)
library(plm)
library(lmtest)
library(reactable)


## Extraíndo base de dados

dados <- read_excel(
  "dados/Prova 2 de ECO457-2022.xls",
  sheet = "Planilha1"
)

dados


## Preparação da base de dados
# Definindo variáveis do PIB, KF e KH em logaritmo.

dados["lpib"] <- log(dados["PIB"])
dados["lkf"] <- log(dados["KF"])
dados["lkh"] <- log(dados["KH"])

# Criando dummies para regiões do Brasil

dados$no <- ifelse(dados$Região == 1,1,0)
dados$ne <- ifelse(dados$Região == 2,1,0)
dados$co <- ifelse(dados$Região == 3,1,0)
dados$se <- ifelse(dados$Região == 4,1,0)
dados$sul <- ifelse(dados$Região == 5,1,0)


# Criando painél para estimação de modelos com dados em painél:
# Atributo individual: *UF*
# Atributo temporal: *Ano*

painel <- pdata.frame(dados, index=c("UF", "Ano"))

painel

## Estimando e comparando modelos

### Definição da fórmula do modelo

# Log do PIB, explicado pelo log do capital físico, pelo log do capital humano e pelas $(n - 1)$ dummies de região (Sudeste como base):

formula <- lpib ~ lkh + lkf + ne + no + co + sul


### Modelo Pooled

reg.pooled <- plm(formula, data=painel, model="pooling")
summary(reg.pooled)


### Modelo de efeitos fixos
# Considerando as unidades de tempo

reg.ef <- plm(formula, data=painel, model="within", effect=c("time"))
summary(reg.ef)

### Modelo de efeitos aleatórios

reg.ea <- plm(formula, data=painel, model="random")
summary(reg.ea)


### Pooled x Efeitos Fixos
# Realizando o teste F de Chow para comparar os modelos de efeitos fixos com o Pooled.
# $H_0$: há igualdade nos interceptos e nas inclinações para todos os indivíduos, caracterizando o modelo de dados agrupados (pooled).

pFtest(reg.ef, reg.pooled)

# Como $p < 0.05$, rejeitamos $H_0$ e assumimos que o modelo de efeitos fixos é melhor do que o modelo Pooled nesse caso.

### Pooled x Efeitos aleatórios
# Teste Lagrange Multiplier de (Breusch-Pagan) para painéis balanceados;
# Compara as estimativas entre os modelos Pooled e de efeitos aleatórios.
# $H_0$: implica que o modelo de dados agrupados (pooled) é preferível.

plmtest(reg.pooled, type="bp")

## Como $p < 0.05$, rejeitamos $H_0$ e assumimos que o modelo de efeitos aleatórios é melhor para este caso, comparado ao Pooled.

### Efeitos fixos x Efeitos aleatórios
## Teste de Hausman para definir entre modelo de efeitos fixos ou modelo de efeitos aleatórios.
## $H_0$: $\alpha_i$ não são correlacionados com $X_{it}$
phtest(reg.ef,reg.ea)
## Como $p < 0.05$, rejeitamos $H_0$ e assumimos que assumimos que $\alpha_i$ são correlacionados com $X_{it}$, pressuposto do modelo de efeitos fixos.

## Pelos testes, o modelo de efeitos fixos foi escolhido como melhor modelo para os dados em painel acima. O que significa que os valores dos interceptos para cada regressão ($\alpha_i$) variam de acordo com o efeito de cada indivíduo ("UF") e que os coeficientes de declividade (das variáveis independentes “lkf” e “lkh”) para cada equação são os mesmos para cada UF.

## Testes

### Teste de heterocedasticidade dos resíduos

bptest(reg.ef)

## Como $p > 0.05$, não podemos rejeitar $H_0$, e portanto não podemos dizer que há heterocedasticidade resudual no modelo.

### Teste de dependência cross-sectional
pcdtest(reg.ef, test="cd")
## Como $p > 0.05$, não podemos rejeitar $H_0$, ou seja, não há indícios de correlação cross-sectional.

### Teste de autocorrelação serial
pbgtest(reg.ef)

## Como $p < 0.05$, rejeitamos $H_0$ e assumimos que há autocorrelação serial(efeito tempo).

## Correção

## Podemos promover a estimação com erros-padrão robustos a correlação cruzada:
summary(reg.ef, vcov = function(x) vcovHC(x, method = "arellano"))