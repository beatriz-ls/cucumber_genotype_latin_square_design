# bibliotecas

library(agridat) # banco de dados
library(dplyr) # manipulação de dados
library(kableExtra) # tabelas
library(car)      # teste de homogeneidade
library(nortest)  # teste de normalidade
library(lattice)  # gráfico de residuos

# dados ------------------------------------------------------------------------

data <- agridat::bridges.cucumber

str(data)

################################################################################
##### analise dos dados de Tifton ##############################################
################################################################################

dt_tifton <- data %>% 
  filter(loc == "Tifton") %>%
  select(-loc)

# anova ------------------------------------------------------------------------

aov_tifton <- aov(yield ~ gen + row + col, data = dt_tifton)

summary(aov_tifton)

# residuos

res_tifton <- residuals(aov_tifton)

fit_tipton<- fitted(aov_tifton)

# verificando pressupostos -----------------------------------------------------

# 1. Teste de Independência dos Erros

## gráficos de residuos

par(mfrow = c(2, 2))
plot(fit_tipton, res_tifton, main = "Resíduos vs. Ajustados", 
     xlab = "Valores Ajustados", ylab = "Resíduos")
abline(h = 0, col = "red", lty = 2)

acf(res_tifton, main = "Autocorrelação dos Resíduos")

## 2. Teste de Normalidade dos Erros

## Teste de Shapiro-Wilk
shapiro_test <- shapiro.test(res_tifton)
cat("Teste de Shapiro-Wilk: p-valor =", shapiro_test$p.value, "\n")

## hist e qqplot
hist(res_tifton, main = "Histograma dos Resíduos", 
     xlab = "Resíduos", breaks = 10, col = "lightblue")
qqnorm(res_tifton)
qqline(res_tifton, col = "red")

# 3. Homogeneidade de Variâncias (Homoscedasticidade)

## Teste de Levene
levene_test <- leveneTest(yield ~ gen, data = dt_tifton)
cat("Teste de Levene: p-valor =", levene_test$`Pr(>F)`[1], "\n")

# 4. Variação Aleatória (Soma dos efeitos = 0)

## Teste indiretamente observando as médias ajustadas

effects_gen <- coef(aov_tifton)["gen"]
cat("Soma dos efeitos dos genótipos (deve ser ~0):", sum(effects_gen), "\n")

################################################################################
##### analise dos dados de Clemson #############################################
################################################################################

dt_clemson <- data %>% 
  filter(loc == "Clemson") %>%
  select(-loc)

# anova ------------------------------------------------------------------------

aov_tifton <- aov(yield ~ gen + row + col + Error(row/col), data = dt_clemson)

summary(aov_tifton)

# residuos

res_clemson <- residuals(aov_tifton)

fit.clemson <- fitted(aov_tifton)

# verificando pressupostos -----------------------------------------------------

# 1. Teste de Independência dos Erros

## gráficos de residuos

par(mfrow = c(2, 2))
plot(fit.clemson, res_clemson, main = "Resíduos vs. Ajustados", 
     xlab = "Valores Ajustados", ylab = "Resíduos")
abline(h = 0, col = "red", lty = 2)

acf(res_clemson, main = "Autocorrelação dos Resíduos")

## 2. Teste de Normalidade dos Erros

## Teste de Shapiro-Wilk
shapiro_test <- shapiro.test(res_clemson)
cat("Teste de Shapiro-Wilk: p-valor =", shapiro_test$p.value, "\n")

## hist e qqplot
hist(res_clemson, main = "Histograma dos Resíduos", 
     xlab = "Resíduos", breaks = 10, col = "lightblue")
qqnorm(res_clemson)
qqline(res_clemson, col = "red")

# 3. Homogeneidade de Variâncias (Homoscedasticidade)

## Teste de Levene
levene_test <- leveneTest(yield ~ gen, data = dt_clemson)
cat("Teste de Levene: p-valor =", levene_test$`Pr(>F)`[1], "\n")

# 4. Variação Aleatória (Soma dos efeitos = 0)

## Teste indiretamente observando as médias ajustadas

effects_gen <- coef(aov_tifton)["gen"]
cat("Soma dos efeitos dos genótipos (deve ser ~0):", sum(effects_gen), "\n")

################################################################################
##### analise dos dados conjuntos ##############################################
################################################################################

# fazer a anova com todos os dados conjuntos das duas localizações...


