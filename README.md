# Trabalho: Análise de Regressão Linear

Este projeto contém uma análise exploratória e modelagem estatística focada em duas hipóteses principais sobre dados urbanos.

## Objetivos
* **Hipótese I:** Analisar a oferta de médicos ativos em cidades com base em população, leitos hospitalares, renda e região geográfica.
* **Hipótese II:** Investigar os fatores associados à taxa de criminalidade (por 100.000 habitantes).

## Estrutura do Projeto
* `dados_amostra.xlsx`: Base de dados utilizada para as análises.
* `dados_trabalho.xlsx`: Base de dados completa contendo informações adicionais.
* O processamento inclui limpeza de dados, criação de novas variáveis (taxas por 100k habitantes) e seleção automática de modelos via *Backward Selection* e busca exaustiva[cite: 1, 2].

## Tecnologias Utilizadas
* **Linguagem:** R
* **Principais Pacotes:** `tidyverse`, `ggplot2`, `leaps` (para seleção de variáveis), `corrplot` e `openxlsx`.

## Principais Etapas
1. **Limpeza e Preparação:** Criação de taxas e estruturação dos dataframes para teste das hipóteses.
2. **Análise Descritiva:** Cálculo de estatísticas resumo, boxplots e histogramas para compreensão da distribuição das variáveis.
3. **Seleção de Modelos:** Utilização da função `regsubsets` para identificar o melhor conjunto de variáveis explicativas através do Cp de Mallows.