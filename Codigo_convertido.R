#' ---
#' title: "Trabalho_ARL"
#' output: html_document
#' date: "2026-07-14"
#' ---
#' 
#' Carregamento de pacotes + leitura dos dados da População para Análise Exploratória
## ----message=FALSE, warning=FALSE--------------------------------------------

library(ggplot2)
library(tidyverse)
library(openxlsx)
library(readxl)
library(corrplot)
library(leaps) # Usado no método BACKWARD
library(MASS)
library(lmtest)
library(car)

#setwd("C:/Users/Helio/OneDrive/Documents/Analise_regressao_linear/trabalho")

#database <- read.xlsx("~/Downloads/dados_amostra.xlsx")
database <- read.xlsx("dados_trabalho.xlsx")


# Criar taxa de crimes por 100.000 habitantes
database$taxa_crimes <- database$x9 / database$x4 * 100000

# Criando a taxa de médicos por 100.00 habitantes
database$taxa_medicos <- database$x7 / database$x4 * 100000

# Criando a taxa de leitos por 100.00 habitantes
database$taxa_leitos <- database$x8 / database$x4 * 100000

# Transformando a variável (Região geográfica - x16) em factor, maneira do R trabalhar com Dummies
database$x16 <- as.factor(database$x16)

colnames(database) <- c("id", "Cidade", "Sigla", "Área da cidade (mi²)", 
                        "População total", "% pop. 18–34 anos", 
                        "% pop. 65+ anos", "Médicos ativos", 
                        "Leitos hospitalares", "Total de crimes", 
                        "% ensino médio completo", "% bacharéis", 
                        "% abaixo da pobreza", "% desempregados", 
                        "Renda per capita", "Renda total (mi USD)", 
                        "Região geográfica", "Taxa de crimes/100k hab.")

head(database)


#' 
#' Classificação das variáveis
## ----echo=FALSE--------------------------------------------------------------

classificacao <- data.frame(
  Código      = c("x3","x4","x5","x6","x7","x8","x9","x10","x11","x12","x13","x14","x15","x16","taxa_crimes"),
  Descrição   = c("Área da cidade (mi²)","População total","% pop. 18-34 anos",
                  "% pop. 65+ anos","Médicos ativos","Leitos hospitalares",
                  "Total de crimes","% ensino médio completo","% bacharéis",
                  "% abaixo da pobreza","% desempregados","Renda per capita",
                  "Renda total (mi USD)","Região geográfica","Taxa de crimes/100k hab."),
  Tipo        = c("Quantitativa Discreta","Quantitativa Discreta","Quantitativa Contínua","Quantitativa Contínua", "Quantitativa Discreta","Quantitativa Discreta","Quantitativa Discreta","Quantitativa Contínua", "Quantitativa Contínua","Quantitativa Contínua","Quantitativa Contínua","Quantitativa Contínua", "Quantitativa","Qualitativa","Quantitativa Contínua"))

knitr::kable(classificacao, caption = "Classificação das variáveis do estudo")


#' 
#' Análise Descritiva
#' Medidas Resumo
## ----echo=FALSE--------------------------------------------------------------
vars_num <- database[, c("Área da cidade (mi²)", 
                             "População total", 
                             "% pop. 18–34 anos", 
                             "% pop. 65+ anos", 
                             "Médicos ativos", 
                             "Leitos hospitalares", 
                             "Total de crimes", 
                             "% ensino médio completo", 
                             "% bacharéis", 
                             "% abaixo da pobreza", 
                             "% desempregados", 
                             "Renda per capita", 
                             "Renda total (mi USD)", 
                             "Taxa de crimes/100k hab.")]

resumo <- data.frame(
  Variável = names(vars_num),
  N        = sapply(vars_num, function(x) sum(!is.na(x))),
  Média    = sapply(vars_num, mean, na.rm = TRUE),
  Mediana  = sapply(vars_num, median, na.rm = TRUE),
  DP       = sapply(vars_num, sd, na.rm = TRUE),
  Mín      = sapply(vars_num, min, na.rm = TRUE),
  Máx      = sapply(vars_num, max, na.rm = TRUE)
)

resumo$CV_pct <- resumo$DP / resumo$Média * 100

resumo[, 3:8] <- round(resumo[, 3:8], 2)

knitr::kable(resumo, row.names = FALSE,
             caption = "Estatísticas descritivas — amostra de modelagem (n=30)")


#' 
#' Distribuição por Região
## ----echo=FALSE--------------------------------------------------------------

freq_regiao <- table(database$`Região geográfica`)

knitr::kable(
  freq_regiao,
  col.names = c("Região", "Frequência"),
  caption   = "Frequência de cidades por região geográfica — amostra de modelagem"
)


#' 
#' Boxplots
#' Variáveis
## ----fig.width=8, fig.height=5, echo=FALSE-----------------------------------

vars_box <- database%>%
  dplyr::select(`População total`, `Médicos ativos`, `Leitos hospitalares`, `Renda per capita`, `Renda total (mi USD)`, `Taxa de crimes/100k hab.`) %>%
  pivot_longer(everything(), names_to = "variavel", values_to = "valor")


#' 
#' Principais variáveis
## ----fig.width=8, fig.height=5, echo=FALSE-----------------------------------
ggplot(vars_box, aes(x = variavel, y = valor)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  facet_wrap(~variavel, scales = "free", ncol = 3) +
  labs(title = "Boxplots das principais variáveis", x = NULL, y = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_blank(), strip.text = element_text(size = 9))


#' 
#' Região Geográfica
## ----fig.width=8, fig.height=5, echo=FALSE-----------------------------------

ggplot(database, aes(x = `Região geográfica`, y = `Médicos ativos`, fill = `Região geográfica`)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Médicos ativos por região geográfica",
       x = "Região", y = "Médicos ativos") +
  theme_minimal() +
  theme(legend.position = "none")


#' 
#' Taxa de crime por região
## ----fig.width=8, fig.height=5, echo=FALSE-----------------------------------

ggplot(database, aes(x = `Região geográfica`, y = `Taxa de crimes/100k hab.`, fill = `Região geográfica`)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Taxa de crimes por região geográfica",
       x = "Região", y = "Taxa de crimes por 100.000 hab.") +
  theme_minimal() +
  theme(legend.position = "none")


#' 
#' Histogramas
## ----fig.width=10, fig.height=8, echo=FALSE----------------------------------

vars_hist <- database %>%
  dplyr::select(`População total`, `% pop. 18–34 anos`, `% pop. 65+ anos`, 
         `Médicos ativos`, `Leitos hospitalares`, `% ensino médio completo`, 
         `% bacharéis`, `% abaixo da pobreza`, `% desempregados`, 
         `Renda per capita`, `Renda total (mi USD)`, `Taxa de crimes/100k hab.`) %>%
  pivot_longer(everything(), names_to = "variavel", values_to = "valor")

ggplot(vars_hist, aes(x = valor)) +
  geom_histogram(bins = 10, fill = "steelblue", color = "white", alpha = 0.8) +
  facet_wrap(~variavel, scales = "free", ncol = 4) +
  labs(title = "Histogramas das variáveis quantitativas", x = NULL, y = "Frequência") +
  theme_minimal() +
  theme(strip.text = element_text(size = 8))


#' 
#' ### Gráfico de Correlação
#' 
## ----fig.width=9, fig.height=8, echo=FALSE-----------------------------------

vars_cor <- database %>%
  dplyr::select(`População total`, `% pop. 18–34 anos`, `% pop. 65+ anos`, 
         `Médicos ativos`, `Leitos hospitalares`, `% ensino médio completo`, 
         `% bacharéis`, `% abaixo da pobreza`, `% desempregados`, 
         `Renda per capita`, `Renda total (mi USD)`, `Taxa de crimes/100k hab.`)

names(vars_cor) <- c("Pop.total","% 18-34","% 65+","Médicos","Leitos",
                     "% Ens.médio","% Bacharéis","% Pobreza",
                     "% Desemprego","Renda p.c.","Renda total","Taxa crimes")

matriz_cor <- cor(vars_cor, use = "complete.obs")

corrplot(matriz_cor,
         method  = "color",
         type    = "upper",
         addCoef.col = "black",
         number.cex  = 0.6,
         tl.cex  = 0.8,
         tl.col  = "black",
         col     = colorRampPalette(c("#d73027","white","#1a9850"))(200),
         title   = "Matriz de correlação entre as variáveis",
         mar     = c(0, 0, 2, 0))


#' 
#' # Criando os DataFrames
#' 
#' Temos duas hipóteses de trabalho presentes nesta análise:
#' 
#' * **Hipótese I (Oferta de Médicos):** Espera-se que o número de médicos ativos em uma cidade esteja relacionado com:
#'     * População total;
#'     * Número de leitos hospitalares;
#'     * Renda total;
#'     * Região geográfica (efeito de variação regional).
#' 
#' * **Hipótese II (Violência):** A taxa de crimes (por 100.000 habitantes) está associada a:
#'     * Características sócio-demográficas;
#'     * Outros aspectos estruturais da cidade.
#'     
#' Por isso serão segregados as análises de modelos assim como dataframes contendo as variáveis
#' 
#' 
#' Leitura dos dados da amostra (n=300)
## ----message=FALSE, warning=FALSE--------------------------------------------

# Carregando os dados da amostra para a modelgem
database_amostra <- read.xlsx("dados_amostra.xlsx")


# Criar taxa de crimes por 100.000 habitantes
database_amostra$taxa_crimes <- database_amostra$x9 / database_amostra$x4 * 100000

# Criando a taxa de médicos por 100.00 habitantes
database_amostra$taxa_medicos <- database_amostra$x7 / database_amostra$x4 * 100000

# Criando a taxa de leitos por 100.00 habitantes
database_amostra$taxa_leitos <- database_amostra$x8 / database_amostra$x4 * 100000

# Transformando a variável (Região geográfica - x16) em factor, maneira do R trabalhar com Dummies
database_amostra$x16 <- as.factor(database_amostra$x16)

colnames(database_amostra) <- c("id", "Cidade", "Sigla", "Área da cidade (mi²)", 
                        "População total", "% pop. 18–34 anos", 
                        "% pop. 65+ anos", "Médicos ativos", 
                        "Leitos hospitalares", "Total de crimes", 
                        "% ensino médio completo", "% bacharéis", 
                        "% abaixo da pobreza", "% desempregados", 
                        "Renda per capita", "Renda total (mi USD)", 
                        "Região geográfica", "Taxa de crimes/100k hab.")



## ----fig.width=10, fig.height=8----------------------------------------------

# Removendo possíveis valores nulos
df_clean <- na.omit(database_amostra)

# Criando um dataframe para as análises

# PRIMEIRA HIPÓTESE
df_clean1 <- subset(df_clean, select = c(`População total`, `Médicos ativos`, `Leitos hospitalares`, `Renda total (mi USD)`,
                                         `Região geográfica`))

# SEGUNDA HIPÓTESE
df_clean2 <- subset(df_clean, select = c(`Área da cidade (mi²)`, `% pop. 18–34 anos`, `% pop. 65+ anos`, `% ensino médio completo`, 
         `% bacharéis`, `% abaixo da pobreza`, `% desempregados`, 
         `Renda per capita`, `Renda total (mi USD)`, `Região geográfica`, `Taxa de crimes/100k hab.`))


#' 
#' # Modelo Completo
#' 
## ----fig.width=10, fig.height=8----------------------------------------------

# Removendo possíveis valores nulos
df_clean <- na.omit(database_amostra)

# Criando um dataframe para as análises

# PRIMEIRA HIPÓTESE - modelo
modelo_completo1 = lm(`Médicos ativos`~ `População total` + `Leitos hospitalares` + `Renda total (mi USD)` +  `Região geográfica`, data=
                        df_clean1)

# SEGUNDA HIPÓTESE - modelo
modelo_completo2 <- lm(`Taxa de crimes/100k hab.` ~ `Área da cidade (mi²)` + `% pop. 18–34 anos` + `% pop. 65+ anos` + `% ensino médio completo` + `% bacharéis` + `% abaixo da pobreza` + `% desempregados` +
                         `Renda per capita` + `Renda total (mi USD)` + `Região geográfica`, data= df_clean2)


#' 
#' # Box-Cox
#' - Verificando se a variável resposta necessita de alguma transformação
#' 
## ----fig.width=10, fig.height=8----------------------------------------------

# primeira hipótese
bc1 <- boxcox(modelo_completo1, lambda =  seq(-2,2,0.1))

# segunda hipótese
bc2 <- boxcox(modelo_completo2, lambda =  seq(-2,2,0.1))


lambda_otimo1 <- bc1$x[which.max(bc1$y)]
lambda_otimo2 <- bc2$x[which.max(bc2$y)]

# Resultado da transformação Box-Cox
interpretar_lambda <- function(lambda_otimo, nome_modelo){
  
  cat("\n", nome_modelo, "\n")
  cat("Lambda ótimo =", round(lambda_otimo, 4), "\n")
  
  if(lambda_otimo >= 0.75 && lambda_otimo <= 1.25){
    print("Lambda próximo de 1: manter Y original")
    
  } else if(lambda_otimo >= -0.25 && lambda_otimo <= 0.25){
    print("Lambda próximo de 0: aplicar log(Y)")
    
  } else if(lambda_otimo >= 0.25 && lambda_otimo <= 0.75){
    print("Lambda próximo de 0.5: aplicar sqrt(Y)")
    
  } else if(lambda_otimo >= -1.25 && lambda_otimo <= -0.75){
    print("Lambda próximo de -1: aplicar 1/Y")
    
  } else {
    print(paste("Aplicar transformação Box-Cox com lambda =", 
                round(lambda_otimo, 4)))
  }
}


interpretar_lambda(lambda_otimo1, "Modelo 1")
interpretar_lambda(lambda_otimo2, "Modelo 2")


#' 
#' Aplicando a trannsformação de Box-Cox para o Modelo 2
## ----------------------------------------------------------------------------
# Aplicando a transformação 
df_clean2$taxa_crimes_sqrt <- sqrt(df_clean2$`Taxa de crimes/100k hab.`)

# Excluindo a antiga coluna 
df_clean2$`Taxa de crimes/100k hab.`  <- NULL

#' 
#' 
#' # Seleção das Variáveis
#' - Utilizando o **método BACKWARD** para escolha do número de variáveis e variáveis
#' Modelo 1
## ----fig.width=10, fig.height=8----------------------------------------------

# PRIMEIRA HIPÓTESE
# Para a seleção das variáveis iremos utilizar o modelo BACKWARD
# Dessa forma iremos verificar se as variáveis que mais se ajustam são as mesmas da hipótese


# Seleção Automática de Variáveis
all_models1 <- regsubsets(`Médicos ativos` ~ ., data= df_clean1, nvmax= ncol(df_clean1) -1, method = "exhaustive", really.big = TRUE)

# Resumo dos Resultados
resumo1 <- summary(all_models1)
resumo1

# Podemos observar que o melhor modelo é o com 3 variávies (x4, x8, x15)
# Plot do Cp
plot(resumo1$cp,
     xlab= "Número de Variáveis",
     ylab= "Cp de Mallows", 
     type= "b",
     pch= 19,
     col= "blue",
     main= "Seleção de Modelos 1: AIC vs Número de Variáveis"
     )
abline(h= min(resumo1$cp), col= "red", lty= 2)
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")


#' 
#' Modelo 2
## ----fig.width=10, fig.height=8----------------------------------------------

# SEGUNDA HIPÓTESE
# Para a seleção das variáveis iremos utilizar o modelo BACKWARD
# Dessa forma iremos verificar se as variáveis que mais se ajustam são as mesmas da hipótese



# Seleção Automática de Variáveis
all_models2 <- regsubsets(taxa_crimes_sqrt ~ ., data= df_clean2, nvmax= ncol(df_clean2) -1, method = "exhaustive", really.big = TRUE)

# Resumo dos Resultados
resumo2 <- summary(all_models2)
resumo2

# Podemos observar que o melhor modelo é o com 4 variávies (x5, x12, x14, x16)
# Plot do Cp
plot(resumo2$cp,
     xlab= "Número de Variáveis",
     ylab= "Cp de Mallows", 
     type= "b",
     pch= 19,
     col= "blue",
     main= "Seleção de Modelos 2: AIC vs Número de Variáveis"
     )
abline(h= min(resumo2$cp), col= "red", lty= 2)
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")


#' 
#' ## Resumo do Processo
#' 
#' Este documento detalha a análise das regressões lineares desenvolvidas.
#' 
#' 1. **Relação entre médicos ativos e Região**: Verificando a relação linear entre o número de médicos ativos e a região. (A mesma hipótese foi descartada pelo BACKWARD)
#' 
#' 2. **Criação do modelo**: Ajuste inicial do modelo de regressão linear utilizando o método de seleção de variáveis **BACKWARD**.
#' 
#' 3. **Análise dos pressupostos**: Avaliação dos principais pressupostos da regressão linear, incluindo:
#'    - Linearidade;
#'    - Normalidade dos resíduos;
#'    - Homocedasticidade;
#'    - Ausência de multicolinearidade;
#'    - Identificação de observações influentes e pontos de alavancagem.
#' 
#' 4. **Validação do modelo final**: Verificação da adequação do modelo selecionado e interpretação dos resultados obtidos.
#' 
#' # Criando os DataFrames de acordo com o melhor modelo BACKWARD
## ----fig.width=10, fig.height=8----------------------------------------------

# Removendo possíveis valores nulos
df_clean <- na.omit(database)

# Criando um dataframe para as análises

# PRIMEIRA HIPÓTESE
df_clean1 <- subset(df_clean, select = c(`Médicos ativos`, `População total`, `Leitos hospitalares`, `Renda total (mi USD)`))

# SEGUNDA HIPÓTESE
df_clean2 <- subset(df_clean, select = c(`Taxa de crimes/100k hab.`, `% pop. 18–34 anos`, `% abaixo da pobreza`,
                         `Renda per capita`, `Região geográfica`))

# Trasnformando a variável Y
df_clean2$taxa_crimes_sqrt <- sqrt(df_clean2$`Taxa de crimes/100k hab.`)
df_clean2$taxa_crimes <- NULL


#' 
#' # Verificando se na **Hipótese 1** existe relação linear entre a região e o número de médicos ativos
#' Vale ressaltar que o modelo final não obteve uma relação com a região geográfica
#' 
## ----fig.width=10, fig.height=8----------------------------------------------

modelo <- lm(`Médicos ativos` ~ `Região geográfica`, data= database)
summary(modelo)

#' 
#' # **Hipótese 1**
#' - Análise dos modelos & Presupostos
#' 
## ----fig.width=10, fig.height=8----------------------------------------------

# Modelo 
modelo1 <- lm(`Médicos ativos` ~ `População total` + `Leitos hospitalares` + `Renda total (mi USD)`, data= df_clean1)

diagnostico_modelo <- function(modelo){
  
  # 1) Resumo geral
  print(summary(modelo))
  
  # 2) Diagnóstico gráfico
  par(mfrow = c(2,2))
  plot(modelo, which = 1:4)
  
  # 3) Teste de Normalidade (Shapiro-Wilk)
  cat("\n--- Teste de Normalidade (Shapiro-Wilk) ---\n")
  shapiro_res <- shapiro.test(residuals(modelo))
  print(shapiro_res)
  if(shapiro_res$p.value > 0.05) {
    cat("Interpretação: Resíduos seguem distribuição normal (p > 0.05).\n")
  } else {
    cat("Interpretação: Resíduos NÃO seguem distribuição normal (p <= 0.05).\n")
  }
  
  # 4) Teste de Heterocedasticidade (Breusch-Pagan)
  cat("\n--- Teste de Heterocedasticidade (Breusch-Pagan) ---\n")
  bp_res <- lmtest::bptest(modelo)
  print(bp_res)
  if(bp_res$p.value > 0.05) {
    cat("Interpretação: Homocedasticidade confirmada (p > 0.05).\n")
  } else {
    cat("Interpretação: Heterocedasticidade detectada (p <= 0.05).\n")
  }
  
  # 5) Multicolinearidade (VIF)
  cat("\n--- Multicolinearidade (VIF) ---\n")
  tryCatch({
    vif_res <- car::vif(modelo)
    print(vif_res)
    if(any(vif_res > 10)) {
      cat("Interpretação: Problema severo de multicolinearidade (VIF > 10).\n")
    } else if(any(vif_res > 5)) {
      cat("Interpretação: Atenção à multicolinearidade (VIF entre 5 e 10).\n")
    } else {
      cat("Interpretação: Multicolinearidade sob controle (VIF < 5).\n")
    }
  }, error = function(e) {
    cat("VIF não aplicável ou erro na estrutura do modelo.\n")
  })
}

# Executar diagnóstico do modelo 1
diagnostico_modelo(modelo1)

#' 
#' 
#' # **Hipótese 2**
#' - Análise dos modelos & Presupostos
#' 
## ----fig.width=10, fig.height=8----------------------------------------------

# Modelo 2 
modelo2 <- lm(`taxa_crimes_sqrt` ~ `% abaixo da pobreza` + `Renda per capita` + `Região geográfica`, data= df_clean2)


# Executar diagnóstico do modelo 2
diagnostico_modelo(modelo2)

#' 
#' # **Validação do Modelo**
#' 
#' Os modelos finais das duas hipóteses foram ajustados apenas com a amostra de treino (n = 300). A validação é feita aplicando o método descrito na Metodologia à amostra separada de n = 140 cidades, que não tem nenhuma cidade em comum com a amostra de treino.
#' 
#' - Parte da validação ajustada em razão de ajustes na Questão 1 por conta da construção do modelo de regressão incluindo todas as variáveis.
#' 
#' # Validação de modelos
#' 
#' ## Questão 1
#' 
#' A hipótese de trabalho prevê que o número de médicos ativos esteja relacionado com a população total, o número de leitos hospitalares e a renda total, e que também varie segundo a região geográfica. Desta forma, vamos analisar se a variavel região é significativa no modelo a partir dos dados de validação bem como a analise das outras
#' variaveis explicativas do modelo. A validação é dividida em dois tópicos: primeiro uma análise de robustez da região geográfica (`modelo_3`), e depois a validação do modelo final efetivamente selecionado (`modelo1`).
#' 
#' ### Robustez da Região Geográfica (modelo_3)
#' 
#' #### Apuração modelo com todas as variáveis
#' 
## ----fig.width=10, fig.height=8----------------------------------------------

# Modelo alternativo com Região geográfica, testando a hipótese conforme formulada originalmente
modelo_3 <- lm(`Médicos ativos` ~ `População total` + `Leitos hospitalares` + `Renda total (mi USD)` + `Região geográfica`,
               data = df_clean)

# Teste t de cada coeficiente (Estimate, Erro Padrão, valor t, p-valor)
summary(modelo_3)

diagnostico_modelo(modelo_3)


#' 
#' O modelo validado aqui é o `modelo_3` (população total, leitos hospitalares, renda total e região geográfica) na amostra de treino (n=300). Os dados do teste t indicam que as variaveis `Região Geografica 2` não são significantes `Região Geografica 3`, mas a variável `Região Geográfica 4` apresentou nivel de significância.
#' 
#' 
## ----fig.width=10, fig.height=8----------------------------------------------

# Carregando os dados para a validação dos modelos

# Carregando os dados
df_validacao <- read.xlsx("dados_validacao.xlsx")

# Criar taxa de crimes por 100.000 habitantes
df_validacao$taxa_crimes <- df_validacao$x9 / df_validacao$x4 * 100000

# Alterando os nomes das colunas
colnames(df_validacao) <- c("id", "Cidade", "Sigla", "Área da cidade (mi²)",
                        "População total", "% pop. 18–34 anos",
                        "% pop. 65+ anos", "Médicos ativos",
                        "Leitos hospitalares", "Total de crimes",
                        "% ensino médio completo", "% bacharéis",
                        "% abaixo da pobreza", "% desempregados",
                        "Renda per capita", "Renda total (mi USD)",
                        "Região geográfica", "Taxa de crimes/100k hab.")

# Transformando a variável (Região geográfica - x16) em factor
df_validacao[, 17] <- as.factor(df_validacao[, 17])

# Conferindo que não há sobreposição entre a amostra de treino e a de validação
cat("Cidades de validação também presentes na amostra de treino:",
    sum(df_validacao$id %in% database_amostra$id), "de", nrow(df_validacao), "\n")

# Trasnformando a variável Y (Segunda Hipótese)
df_validacao$taxa_crimes_sqrt <- sqrt(df_validacao$`Taxa de crimes/100k hab.`)

# SEGUNDA HIPÓTESE
colunas_interesse2 <- c("taxa_crimes_sqrt", "% pop. 18–34 anos", "% abaixo da pobreza", "Renda per capita", "Região geográfica")
df_val2 <- df_validacao[, colunas_interesse2]


#' 
#' #### Comparação dos coeficientes
#' 
## ----------------------------------------------------------------------------

# Reestimando o modelo_3 (com região) apenas com os 140 dados de validação
modelo_3_validacao <- lm(`Médicos ativos` ~ `População total` + `Leitos hospitalares` + `Renda total (mi USD)` + `Região geográfica`,
                          data = df_validacao)

# Coeficientes lado a lado: amostra de treino (n=300) x amostra de validação (n=140)
coef_comparacao1 <- data.frame(
  Treino_n300    = coef(modelo_3),
  Validacao_n140 = coef(modelo_3_validacao)
)
print(round(coef_comparacao1, 4))


#' 
#' Os coeficientes das três variáveis de porte da cidade (população total, leitos hospitalares e renda total) mantêm o mesmo sinal e ordem de grandeza semelhante entre treino e validação. Já os coeficientes de região são menos estáveis: a região Oeste mantém o sinal positivo nas duas amostras, mas as regiões Centro-Norte e Sul chegam a trocar de sinal. Isso sugere que o efeito específico de cada região não é bem determinado com o tamanho de amostra disponível.
#' 
#' #### Teste F e t de cada modelo
#' 
## ----------------------------------------------------------------------------

summary(modelo_3)
summary(modelo_3_validacao)


#' 
#' Os resultados apresentados teste apresentam desempenho semelhante entre os dois modelos. No entanto, o teste t da variavel `Região geografica 4` apresentou resultado diferente na base de validação, trazendo mais indicios de instabilidade da variável de `Região Geografica`. O teste F global também é significativo nas duas amostras (F=1161,4, p<0,001 no treino; F=613,7, p<0,001 na validação), confirmando que o modelo com região é globalmente significativo em ambas.
#' 
#' #### Avaliação da multicolinearidade
#' 
## ----------------------------------------------------------------------------

cat("--- VIF na amostra de treino (n=300) ---\n")
print(car::vif(modelo_3))

cat("\n--- VIF na amostra de validação (n=140) ---\n")
print(car::vif(modelo_3_validacao))


#' 
#' A multicolinearidade elevada entre população total e renda total, já identificada no diagnóstico do modelo de treino, também está presente na amostra de validação (VIF de 49,1 e 33,1, respectivamente) com ambos bem acima do limite de 10 nas duas amostras. O VIF de leitos hospitalares também sobe para 12,3 (validação), passando a exceder o limite de 10 nessa amostra. Contudo, a região geográfica apresentou VIF baixo e estável nas duas amostras (1,03 em ambas), ou seja, isso indica que a variavel de Região Geográfica não está envolvida diretamente na multicolinearidade do modelo.
#' 
#' ### Validação do Modelo Final (modelo1)
#' 
#' Esta é a validação do modelo efetivamente selecionado pelo método BACKWARD (`modelo1`: população total, leitos hospitalares e renda total, sem região), ou seja, o modelo final adotado para a Hipótese 1. A análise da região feita acima com o `modelo_3` é complementar; é este `modelo1` que é validado quanto à sua capacidade preditiva.
#' 
#' #### Comparação dos coeficientes
#' 
## ----------------------------------------------------------------------------

# Reestimando o modelo1 (modelo final, sem região) apenas com os 140 dados de validação
modelo1_validacao <- lm(`Médicos ativos` ~ `População total` + `Leitos hospitalares` + `Renda total (mi USD)`,
                         data = df_validacao)

# Coeficientes lado a lado: amostra de treino (n=300) x amostra de validação (n=140)
coef_comparacao1_final <- data.frame(
  Treino_n300    = coef(modelo1),
  Validacao_n140 = coef(modelo1_validacao)
)
print(round(coef_comparacao1_final, 4))


#' 
#' Os coeficientes reestimados com as 140 observações de validação mantêm o mesmo sinal dos coeficientes estimados com as 300 observações de treino (população negativa; leitos hospitalares e renda total positivos) e ordem de grandeza semelhante. Isso indica que a relação captada pelo modelo final é consistente entre as duas amostras.
#' 
#' #### Teste F e t de cada modelo
#' 
## ----------------------------------------------------------------------------

summary(modelo1)
summary(modelo1_validacao)


#' 
#' O modelo é altamente significativo nas duas amostras (teste F: 2260,9 no treino, 1169,5 na validação, ambos p<0,001). Os três coeficientes (população total, leitos hospitalares e renda total) são individualmente significativos a 5% nas duas amostras (todos p<0,001); apenas o intercepto deixa de ser significativo na validação (p=0,79), o que não afeta a interpretação prática do modelo.
#' 
#' #### Avaliação da multicolinearidade
#' 
## ----------------------------------------------------------------------------

cat("--- VIF na amostra de treino (n=300) ---\n")
print(car::vif(modelo1))

cat("\n--- VIF na amostra de validação (n=140) ---\n")
print(car::vif(modelo1_validacao))


#' 
#' A multicolinearidade elevada entre população total e renda total, já identificada no diagnóstico do modelo de treino (VIF de 54,9 e 43,9, respectivamente), também está presente na amostra de validação (VIF de 47,3 e 31,6, respectivamente) — ambos bem acima do limite de 10 nas duas amostras. Isso mostra que essa instabilidade não é uma particularidade da amostra de treino, mas uma característica estrutural da relação entre essas duas variáveis explicativas.
#' 
#' #### DFFITS e DFBETAS (amostra de treino)
#' 
#' O DFFITS e o DFBETAS são calculados com o `modelo1`, ajustado apenas na amostra de treino (n = 300), e não na amostra de validação.
#' 
## ----------------------------------------------------------------------------

# n = número de observações e p = número de parâmetros (incluindo intercepto) do modelo de treino
n1 <- nrow(df_clean1)
p1 <- length(coef(modelo1))

# --- DFFITS: influência de cada observação sobre o seu próprio valor ajustado ---
dffits1 <- dffits(modelo1)
limite_dffits1 <- 2 * sqrt(p1 / n1)

pontos_dffits1 <- which(abs(dffits1) > limite_dffits1)

cat("--- DFFITS (Hipótese 1 - modelo1) ---\n")
cat("Limite de referência (2*sqrt(p/n)):", round(limite_dffits1, 4), "\n")
cat("Observações que excedem o limite:", length(pontos_dffits1), "de", n1, "\n\n")

if (length(pontos_dffits1) > 0) {
  cidades_dffits1 <- data.frame(
    Cidade = df_clean$Cidade[pontos_dffits1],
    DFFITS = round(dffits1[pontos_dffits1], 4)
  )
  print(cidades_dffits1[order(-abs(cidades_dffits1$DFFITS)), ])
}

# --- DFBETAS: influência de cada observação sobre cada coeficiente estimado ---
dfbetas1 <- dfbetas(modelo1)
limite_dfbetas1 <- 2 / sqrt(n1)

cat("\n--- DFBETAS (Hipótese 1 - modelo1) ---\n")
cat("Limite de referência (2/sqrt(n)):", round(limite_dfbetas1, 4), "\n")

pontos_dfbetas1 <- which(apply(abs(dfbetas1), 1, max) > limite_dfbetas1)
cat("Observações que excedem o limite em pelo menos um coeficiente:", length(pontos_dfbetas1), "de", n1, "\n\n")

if (length(pontos_dfbetas1) > 0) {
  cidades_dfbetas1 <- data.frame(
    Cidade = df_clean$Cidade[pontos_dfbetas1],
    round(dfbetas1[pontos_dfbetas1, , drop = FALSE], 4)
  )
  print(cidades_dfbetas1)
}

# Observações sinalizadas simultaneamente pelos dois critérios (candidatos mais fortes a influentes)
pontos_comuns1 <- intersect(pontos_dffits1, pontos_dfbetas1)
cat("\nObservações sinalizadas simultaneamente por DFFITS e DFBETAS:", length(pontos_comuns1), "\n")
if (length(pontos_comuns1) > 0) {
  print(df_clean$Cidade[pontos_comuns1])
}


#' 
#' Com o `modelo1` (4 parâmetros), 28 das 300 cidades excedem o limiar de DFFITS e 35 excedem o de DFBETAS em pelo menos um coeficiente, com 27 cidades sinalizadas pelos dois critérios simultaneamente. Los Angeles é a observação mais influente (DFFITS=1,69), seguida por Montgomery (1,50), Baltimore City (1,32), Suffolk (1,32) e Harris (-1,15) — coerente com a multicolinearidade entre população e renda total: cidades muito grandes nessas duas variáveis tendem a ter maior influência sobre os coeficientes.
#' 
#' #### Cálculo do MSPR
#' 
## ----------------------------------------------------------------------------

# Predição dos 140 casos novos utilizando a reta ajustada com os 300 dados de treino (modelo1, modelo final)
previsoes1 <- predict(modelo1, newdata = df_validacao)

# MSPR = soma dos quadrados dos erros de predição (Yi - Yi_previsto) / n* (n* = 140)
mspr1 <- sum((df_validacao$`Médicos ativos` - previsoes1)^2) / nrow(df_validacao)

# MSE da tabela ANOVA do modelo ajustado com os 300 dados de treino
mse_anova1 <- anova(modelo1)["Residuals", "Mean Sq"]

cat("MSE (ANOVA, treino, n=300):", round(mse_anova1, 2), "\n")
cat("MSPR (validação, n* = 140):", round(mspr1, 2), "\n")
cat("Razão MSPR / MSE:", round(mspr1 / mse_anova1, 3), "\n")

# Visualização das predições vs Reais
plot(df_validacao$`Médicos ativos`, previsoes1,
     main = "Valores Reais vs Previstos (Primeira Hipótese - modelo1)",
     xlab = "Valores Reais (Médicos ativos)", ylab = "Valores Previstos")
abline(0, 1, col = "red", lwd = 2) # Linha de perfeição


#' 
#' O MSPR (`r round(mspr1, 2)`) ficou muito próximo do MSE gerado na tabela ANOVA do modelo ajustado com as 300 observações de treino (`r round(mse_anova1, 2)`), com razão de `r round(mspr1/mse_anova1, 3)`. O R² preditivo na amostra de validação é de 0,9406, próximo do R² ajustado de 0,9578 obtido no treino. Isso confirma que o `modelo1`, o modelo final selecionado para a Hipótese 1, mantém boa performance para dados novos.
#' 
#' # Validação da Segunda Hipótese
#' 
#' ## Verificação de consistência dos coeficientes
#' 
## ----------------------------------------------------------------------------

# Reestimando o modelo escolhido apenas com os 140 dados de validação
modelo2_validacao <- lm(taxa_crimes_sqrt ~ `% abaixo da pobreza` + `Renda per capita` + `Região geográfica`,
                         data = df_val2)

# Coeficientes lado a lado: amostra de treino (n=300) x amostra de validação (n=140)
coef_comparacao2 <- data.frame(
  Treino_n300    = coef(modelo2),
  Validacao_n140 = coef(modelo2_validacao)
)
print(round(coef_comparacao2, 5))


#' 
#' Os coeficientes reestimados com as 140 observações de validação mantêm o mesmo sinal dos coeficientes estimados com as 300 observações de treino (pobreza e renda per capita positivos; as três regiões com efeito positivo em relação ao Nordeste de referência). As magnitudes dos efeitos de região variam mais entre as duas amostras (por exemplo, a região Oeste cai de 17,43 para 5,13), indicando que o efeito da região é a parte menos estável do modelo. Ainda assim, a direção geral das conclusões se mantém.
#' 
#' ## Teste F e t de cada modelo
#' 
## ----------------------------------------------------------------------------

summary(modelo2)
summary(modelo2_validacao)


#' 
#' O modelo é globalmente significativo nas duas amostras (teste F: 49,4 no treino, 15,2 na validação, ambos p<0,001), embora o F caia bastante na validação, refletindo a queda de R² já observada. Pobreza e renda per capita permanecem individualmente significativas a 5% nas duas amostras. Já entre as indicadoras de região, apenas a região Sul continua significativa na validação (p<0,001); as regiões Centro-Norte e Oeste, que eram significativas no treino (p<0,001 em ambas), deixam de ser na validação (p=0,32 e p=0,20, respectivamente), indicando que o efeito de região é a parte menos estável do modelo, como já indicado na comparação dos coeficientes.
#' 
#' ## Cálculo do MSPR
#' 
## ----------------------------------------------------------------------------

# Predição dos 140 casos novos utilizando a reta ajustada com os 300 dados de treino
previsoes2 <- predict(modelo2, newdata = df_val2)

# MSPR = soma dos quadrados dos erros de predição (Yi - Yi_previsto) / n* (n* = 140)
mspr2 <- sum((df_val2$taxa_crimes_sqrt - previsoes2)^2) / nrow(df_val2)

# MSE da tabela ANOVA do modelo ajustado com os 300 dados de treino
mse_anova2 <- anova(modelo2)["Residuals", "Mean Sq"]

cat("MSE (ANOVA, treino, n=300):", round(mse_anova2, 4), "\n")
cat("MSPR (validação, n* = 140):", round(mspr2, 4), "\n")
cat("Razão MSPR / MSE:", round(mspr2 / mse_anova2, 3), "\n")

# Visualização das predições vs Reais
plot(df_val2$taxa_crimes_sqrt, previsoes2,
     main = "Valores Reais vs Previstos (Segunda Hipótese)",
     xlab = "Valores Reais (raiz da taxa de crimes)", ylab = "Valores Previstos")
abline(0, 1, col = "red", lwd = 2) # Linha de perfeição


#' 
#' O MSPR (`r round(mspr2, 2)`) ficou próximo do MSE do modelo ajustado com as 300 observações de treino (`r round(mse_anova2, 2)`), com razão de `r round(mspr2/mse_anova2, 3)`. Isso demonstra que o MSE do modelo de treino está alinhado com o apresentado no conjunto de validação.
#' 
#' # Identificação de Pontos Influentes e Discrepantes (DFFITS e DFBETAS) - Hipótese 2
#' 
#' O DFFITS e o DFBETAS são calculados apenas na amostra de treino (n = 300)
#' usada para ajustar o modelo final da Hipótese 2, e não na amostra de
#' validação (o mesmo cálculo para a Hipótese 1, com o `modelo_3`, está na
#' seção "Validação da Primeira Hipótese"):
#' 
## ----------------------------------------------------------------------------

# n = número de observações e p = número de parâmetros (incluindo intercepto) do modelo de treino
n2 <- nrow(df_clean2)
p2 <- length(coef(modelo2))

# --- DFFITS: influência de cada observação sobre o seu próprio valor ajustado ---
dffits2 <- dffits(modelo2)
limite_dffits2 <- 2 * sqrt(p2 / n2)

pontos_dffits2 <- which(abs(dffits2) > limite_dffits2)

cat("--- DFFITS (Hipótese 2) ---\n")
cat("Limite de referência (2*sqrt(p/n)):", round(limite_dffits2, 4), "\n")
cat("Observações que excedem o limite:", length(pontos_dffits2), "de", n2, "\n\n")

if (length(pontos_dffits2) > 0) {
  cidades_dffits2 <- data.frame(
    Cidade = df_clean$Cidade[pontos_dffits2],
    DFFITS = round(dffits2[pontos_dffits2], 4)
  )
  print(cidades_dffits2[order(-abs(cidades_dffits2$DFFITS)), ])
}

# --- DFBETAS: influência de cada observação sobre cada coeficiente estimado ---
dfbetas2 <- dfbetas(modelo2)
limite_dfbetas2 <- 2 / sqrt(n2)

cat("\n--- DFBETAS (Hipótese 2) ---\n")
cat("Limite de referência (2/sqrt(n)):", round(limite_dfbetas2, 4), "\n")

pontos_dfbetas2 <- which(apply(abs(dfbetas2), 1, max) > limite_dfbetas2)
cat("Observações que excedem o limite em pelo menos um coeficiente:", length(pontos_dfbetas2), "de", n2, "\n\n")

if (length(pontos_dfbetas2) > 0) {
  cidades_dfbetas2 <- data.frame(
    Cidade = df_clean$Cidade[pontos_dfbetas2],
    round(dfbetas2[pontos_dfbetas2, , drop = FALSE], 4)
  )
  print(cidades_dfbetas2)
}

# Observações sinalizadas simultaneamente pelos dois critérios (candidatos mais fortes a influentes)
pontos_comuns2 <- intersect(pontos_dffits2, pontos_dfbetas2)
cat("\nObservações sinalizadas simultaneamente por DFFITS e DFBETAS:", length(pontos_comuns2), "\n")
if (length(pontos_comuns2) > 0) {
  print(df_clean$Cidade[pontos_comuns2])
}


#' 
#' As cidades listadas acima ultrapassam os limiares de referência de DFFITS e/ou DFBETAS no modelo da taxa de crimes e devem ser examinadas individualmente. Caso alguma dessas observações corresponda a uma cidade atípica genuína (por exemplo, com renda total muito acima das demais), a recomendação não é excluí-la da amostra, e sim reconhecer a limitação e considerar, para trabalhos futuros, um modelo de regressão robusta que incorpore essa variabilidade em vez de removê-la.
