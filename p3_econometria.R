## Instalando e carregando pacotes
install.packages("readxl")
install.packages("plm")
install.packages("lmtest")

library(readxl)
library(plm)
library(lmtest)

## Extraíndo dados para um dataframe
df <- read_excel(
  "dados/Prova 2 de ECO457-2022.xls",
  sheet = "Planilha1"
)

## Definindo variáveis do PIB, KF e KH em logaritmo.
df["lpib"] <- log(df["PIB"])
df["lkf"] <- log(df["KF"])
df["lkh"] <- log(df["KH"])

## Criando dummies para regiões do Brasil
df$no <- ifelse(df$Região == 1,1,0)
df$ne <- ifelse(df$Região == 2,1,0)
df$co <- ifelse(df$Região == 3,1,0)
df$se <- ifelse(df$Região == 4,1,0)
df$sul <- ifelse(df$Região == 5,1,0)

## Preparando dados para análise de regressão com dados em painel
## Atributo individual: UF
## Atributo temporal: Ano
df = pdata.frame(df, index=c("UF", "Ano"))

## Log do PIB,explicado pelo log do capital físico,
## pelo log do capital humano
## e pelas dummies de região.
formula <- lpib~lkf+lkh+se+no+ne+co+sul

## Estimando modelo Pooled
reg.pooled <- plm(formula, data=df, model="pooling")

## Estimando modelo de efeitos fixos
reg.ef <- plm(formula, data=df, model="within", effect = c("individual"))

## Estimando modelo de efeitos aleatórios
reg.ea <- plm(formula, data=df, model="random", random.method="walhus")

## Realizando o teste F de Chow para
## comparar os modelos de efeitos fixos com o Pooled.
pFtest(reg.ef, reg.pooled)
######################################################
#         F test for individual effects

# data:  formula
# F = 1407.2, df1 = 23, df2 = 105, p-value < 2.2e-16
# alternative hypothesis: significant effects
######################################################

## H0: há igualdade nos interceptos e nas inclinações para todos os indivíduos,
## caracterizando o modelo de dados agrupados (pooled).
## Como o p-valor < 0.05, rejeitamos H0 e assumimos
## que o modelo de efeitos fixos é melhor do que o modelo Pooled nesse caso.

## Teste Lagrange Multiplier de (Breusch-Pagan) para painéis balanceados;
## Compara as estimativas entre os modelos Pooled e de efeitos aleatórios.
plmtest(reg.pooled, type="bp")
#########################################################################
#         Lagrange Multiplier Test - (Breusch-Pagan) for balanced panels
# 
# data:  formula
# chisq = 96.327, df = 1, p-value < 2.2e-16
# alternative hypothesis: significant effects
#########################################################################

## H0: implica que o modelo de dados agrupados (pooled) é preferível.
## Como p=valor < 0.05, concluímos que o modelo de efeitos aleatórios
## é melhor para este caso, comparado ao Pooled.

## Teste de Hausman para definir entre modelo de efeitos fixos
## ou modelo de efeitos aleatórios.
phtest(reg.ef,reg.ea)
######################################################
#        Hausman Test
#
#data:  formula
#chisq = 66.813, df = 3, p-value = 2.054e-14
#alternative hypothesis: one model is inconsistent
######################################################

## H0: αi não são correlacionados com Xit
## Como p-valor < 0.05, rejeitamos H0 e assumimos que
## αi são correlacionados com Xit, pressuposto do modelo de efeitos fixos.


## Pelos testes, o modelo de efeitos fixos foi escolhido como melhor modelo para os dados em painel acima.
## O que significa que os valores dos interceptos para cada regressão
## (αi) variam de acordo com o efeito de cada indivíduo ("UF") e que
## os coeficientes de declividade (das variáveis independentes “lkf” e “lkh”)
## para cada equação são os mesmos para cada UF.


summary(reg.ef)
######################################################
# Balanced Panel: n = 27, T = 5, N = 135
# 
# Residuals:
#        Min.     1st Qu.      Median     3rd Qu.        Max. 
# -7.3002e-02 -2.1932e-02 -1.3197e-15  1.8610e-02  7.8798e-02 
# 
# Coefficients: (1 dropped because of singularities)
#      Estimate Std. Error t-value Pr(>|t|)    
# lkf  0.023062   0.011555  1.9958  0.04855 *  
# lkh  0.931732   0.080029 11.6425  < 2e-16 ***
# no  -0.040800   0.039366 -1.0364  0.30239    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Total Sum of Squares:    0.61462
# Residual Sum of Squares: 0.12908
# R-Squared:      0.78998
# Adj. R-Squared: 0.73197
# F-statistic: 131.65 on 3 and 105 DF, p-value: < 2.22e-16
######################################################

## Os resultados da estimação indicam que o log do capital humano tem impacto
## significativo e positivo no log do PIB.
## Apesar de um p-valor próximo de 0.05, ainda aceitamos que o impacto de LKF é
## estatisticamente diferente de 0.
## Além disso, algumas dummies de região apresentaram 
## relação linear perfeita com outras variáveis e
## por isso foram removidas do modelo automaticamente.
## A dummy "no" não foi significativa, pois p-valor > 0.05.

## Teste de heterocedasticidade dos erros
bptest(reg.ef)
######################################################
#         studentized Breusch-Pagan test
# 
# data:  reg.ef
# BP = 10.285, df = 6, p-value = 0.1132
######################################################

## Como p-valor > 0.05, não podemos rejeitar H0,
## e portanto não podemos dizer que há heterocedasticidade no modelo.

## Teste de dependência cross-sectional
pcdtest(reg.ef, test="cd")

######################################################
#        Pesaran CD test for cross-sectional dependence in panels
#
# data:  lpib ~ lkf + lkh + se + no + ne + co + sul
# z = 12.282, p-value < 2.2e-16
# alternative hypothesis: cross-sectional dependence
######################################################

## A dependência cross-sectional se apresenta em panieis
## com longas séries de tempo.
## A hipótese nula é de que os resíduos
## através dos indivíduos não estão correlacionados.
## No nosso caso, como p < 0.05,
## rejeitamos H0, e dizemos que há
## autocorrelação entre os resíduos através do indivíduos.

## Teste de autocorrelação serial
pbgtest(reg.ef)
#################################################################################
#         Breusch-Godfrey/Wooldridge test for serial correlation in panel models
# 
# data:  formula
# chisq = 28.872, df = 5, p-value = 2.457e-05
# alternative hypothesis: serial correlation in idiosyncratic errors
#################################################################################
## É importante destacar que este teste é recomendável para paineis Longos
## também podemos avaliar se há efeitos individuais ou de tempo. O teste analisa
## se há efeitos observados de tempo, ou seja, H0 assume a não correlação entre
## os erros da mesma unidade. Rejeitar H0 indica que há correlação entre os erros, 
## ou seja, há o efeito tempo.
## Como p < 0.05, rejeitamos H0, e assumimos que há efeito tempo.

## Ao tentar ajustar a autocorrelação do modelo, recebi o erro:
# system is computationally singular: reciprocal condition number
## Que significa que a matriz não é invertível ou tem determinante muito próximo de 0.
## Sendo assim, eliminiei as dummies de região do modelo, para conseguir corrigir.

formula2 <- lpib~lkf+lkh
reg.ef2 <- plm(formula2, data=df, model="within", effect = c("individual"))

summary(reg.ef2, vcov = function(x) vcovHC(x, method = "arellano"))
# Balanced Panel: n = 27, T = 5, N = 135
# 
# Residuals:
#        Min.     1st Qu.      Median     3rd Qu.        Max. 
# -0.07329311 -0.02233214 -0.00010179  0.01873924  0.07841512 
# 
# Coefficients:
#     Estimate Std. Error t-value Pr(>|t|)    
# lkf 0.022502   0.010161  2.2145  0.02894 *  
# lkh 0.938982   0.077502 12.1156  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Total Sum of Squares:    0.61462
# Residual Sum of Squares: 0.1304
# R-Squared:      0.78783
# Adj. R-Squared: 0.73178
# F-statistic: 133.631 on 2 and 26 DF, p-value: 2.0908e-14

## Verificamos que o beta de lkh ficou ainda maior
## após correção de autocorrelação.