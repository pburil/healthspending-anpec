# Carregar pacotes necessários
library(dplyr)
library(ggplot2)
library(readxl)
library(scales)
library(kableExtra)
library(ggplot2)
library(plotly)
library(tidyr)
library(ineq)
library(formattable)
library(kableExtra)
library(openxlsx)

# 1. Carregar dados principais
setwd("C:\\Users\\buril\\OneDrive\\Área de Trabalho\\aeae\\documnto bsb\\planilhas\\ANPEC\\INDICADORES SIOPS")

despesas_recursos_proprios <- read_xlsx("Despesas Recursos Próprios por Ano Segundo capitais (2003-2023).xlsx")
receita_transferencia <- read_xlsx("Transferências SUS por ano Segundo capitais (2003-2023).xlsx")
despesas_total <- read_xlsx("Despesa Total por Ano segundo capitais (2003-2023).xlsx")

# 2. Ajustar os dados de despesas sem a correção pela inflação (IPCA)
despesas_final <- left_join(despesas_recursos_proprios, receita_transferencia, by = "Capitais")

despesas_long <- despesas_final %>%
  pivot_longer(cols = starts_with("Recursos_proprios_") | starts_with("Transferencia_"),
               names_to = c(".value", "ano"),
               names_pattern = "(.+)_(\\d+)") %>%
  mutate(ano = as.integer(ano)) %>%
  mutate(
    orcamento_total = Recursos_proprios + Transferencia,  # Sem correção pela inflação
    proporcao_proprio = Recursos_proprios / orcamento_total, # Proporção sem correção
    receita_transferencia = Transferencia / orcamento_total # Proporção sem correção
  )

# 3. Plot do gasto (gráfico de área)
df_plot <- despesas_long %>%
  pivot_longer(cols = c(proporcao_proprio, receita_transferencia),
               names_to = "tipo_gasto",
               values_to = "proporcao")

ggplot(df_plot, aes(x = ano, y = proporcao, fill = tipo_gasto)) +
  geom_area(position = 'stack') +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  facet_wrap(~ Capitais, scales = "free_x") + # Ajusta a largura das facetas
  labs(title = "Proporção das Receitas em Saúde por Capital (2002 - 2022)",
       x = "Ano",
       y = "Proporção da Receita (%)",
       fill = "Tipo de Gasto") +
  scale_fill_discrete(labels = c("proporcao_proprio" = "Recurso próprio",
                                 "receita_transferencia" = "Recurso de Transferência")) +
  theme_minimal() +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 10))

# 4. Ajuste no pivot_longer para criar um valor distinto por ano para as variáveis

estatisticas_transferencias <- despesas_final %>% 
  pivot_longer(cols = starts_with("transferencia_"), 
               names_to = "ano", 
               values_to = "transferencia") %>% 
  mutate(ano = as.integer(gsub("[^0-9]", "", ano))) %>%  # Extrai o ano do nome da variável
  filter(!is.na(ano)) %>%  # Remove linhas com 'ano' inválido
  mutate(transferencia = as.numeric(transferencia)) %>%  # Garantir que a variável 'transferencia' seja numérica
  filter(!is.na(transferencia)) %>% 
  mutate(transferencia_per_capita = transferencia / População_2022)  # Transferência per capita (usando população de 2022)

# 5. Calcular as estatísticas descritivas por capital
estatisticas_por_capital <- estatisticas_transferencias %>%
  group_by(Capitais) %>%  # Agrupar por capital
  summarise(
    total_transferencias = sum(transferencia, na.rm = TRUE),  
    media_transferencias = mean(transferencia, na.rm = TRUE),  
    sd_transferencias = sd(transferencia, na.rm = TRUE),
    transferencia_per_capita = mean(transferencia_per_capita, na.rm = TRUE),  # Média de transferência per capita
    .groups = "drop"
  ) %>%
  arrange(transferencia_per_capita)  # Ordenar em ordem crescente pela transferência per capita

# 6. Exibir as primeiras linhas para verificar o resultado
print(estatisticas_por_capital)

#exportando excel
wb <- createWorkbook()
addWorksheet(wb, "Estatísticas por Capital")

# Exportar a tabela para Excel
writeDataTable(
  wb, 
  sheet = "Estatísticas por Capital", 
  estatisticas_por_capital,
  withFilter = TRUE, # Adicionar filtros na tabela
  tableStyle = "TableStyleMedium2" # Estilo da tabela
)

# Salvar o arquivo Excel
saveWorkbook(wb, "estatisticas_transferencias.xlsx", overwrite = TRUE)
 
# Criar o gráfico
ggplot(estatisticas_transferencias, aes(x = ano, y = transferencia_per_capita, group = Capitais, color = Capitais)) +
  geom_line() +
  facet_wrap(~ Capitais, scales = "free_y", ncol = 5) +  # Painéis por capital
  labs(
    title = "Variação das Transferências per Capita por Capital (2002 - 2022)",
    x = "Ano",
    y = "Transferência per Capita (R$)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Melhorar legibilidade
    strip.text = element_text(size = 6)  # Ajustar tamanho dos títulos dos painéis
  ) +
  scale_color_viridis_d()


#### GINI ##### 
dados_munic <- read_xlsx("MUNICS.xlsx")
dados_munic <- dados_munic %>% filter(!is.na(despesa_asps_2022))


gini_total <- Gini(dados_munic$despesa_asps_2022)
gini_percapita <- Gini(dados_munic$despesa_asps_per_capita_2022)

gini_por_regiao <- dados_munic %>%
  group_by(regiao) %>%
  summarise(
    gini_total = Gini(despesa_asps_2022, na.rm = TRUE),
    gasto_medio = mean(despesa_asps_2022),
    gini_percapita = Gini(despesa_asps_per_capita_2022, na.rm = TRUE),
    gasto_medio_per_capita = mean(despesa_asps_per_capita_2022),
    gasto_percapita = sum(despesa_asps_2022)/sum(populacao)
  )


gini_por_quartil <- dados_munic %>%
  group_by(quartil_PIB) %>%
  summarise(
    gini_total = Gini(despesa_asps_2022, na.rm = TRUE),
    gasto_medio = mean(despesa_asps_2022),
    gini_percapita = Gini(despesa_asps_per_capita_2022, na.rm = TRUE),
    gasto_medio_per_capita = mean(despesa_asps_per_capita_2022),
    gasto_percapita = sum(despesa_asps_2022)/sum(populacao)
  )

# Gerar tabelas em HTML com kableExtra
html_tabela_gini_regiao <- kable(gini_por_regiao, format = "html") %>%
  kable_styling(full_width = FALSE)

html_tabela_gini_por_quartil <- kable(gini_por_quartil, format = "html") %>%
  kable_styling(full_width = FALSE)

# Salvar tabelas como arquivos HTML
write(html_tabela_gini_regiao, file = "gini_regiao.html")
write(html_tabela_gini_por_quartil, file = "gini_por_quartil.html")


table(dados_munic$quartil_PIB)

ggplot(dados_munic, aes(x = despesa_asps_2022)) +
  geom_histogram(aes(y = ..density..), 
                 bins = 30, 
                 fill = "#69b3a2",  # Cor suave
                 color = "black",   # Borda preta para contraste
                 alpha = 0.7) +     # TransparÃªncia para suavizar o visual
  geom_density(color = "darkblue", 
               size = 1) +          # Linha de densidade suave
  scale_x_log10(labels = label_number_si()) +  # TransformaÃ§Ã£o logarÃ­tmica e formataÃ§Ã£o de nÃºmeros legÃ­veis
  labs(
    title = "Distribuição do Gasto Total em Sáude por Município no Brasil",
    subtitle = "Dados transformados em escala logaritmica para melhor visualização",
    x = "Gasto Total em ASPS (Log10)", 
    y = "Densidade de Frequência"
  ) +
  theme_minimal(base_size = 14) +   # Tema minimalista com fonte maior
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),  # Centraliza o tÃ­tulo e o coloca em negrito
    plot.subtitle = element_text(hjust = 0.5),             # Centraliza o subtÃ­tulo
    axis.text.x = element_text(angle = 45, vjust = 0.5),   # Rotaciona os rÃ³tulos do eixo X
    axis.text.y = element_text(size = 12),                 # Aumenta o tamanho do texto do eixo Y
    panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "grey80")  # Grelha mais leve
  ) +
  theme(
    plot.margin = margin(1, 1, 1, 1, "cm")  # Ajuste das margens
  )

ggplot(dados_munic, aes(x = regiao, y = despesa_asps_2022)) +
  geom_boxplot(
    aes(fill = regiao)
  ) +
  coord_cartesian(ylim = c(0, 1e8)) +  # Limita o eixo Y para focar nos dados principais
  scale_y_continuous(labels = label_number_si()) +  # FormataÃ§Ã£o do eixo Y para nÃºmeros legÃ­veis
  labs(
    title = "Boxplot do Gasto Total em Saúde por Região no Brasil",
    subtitle = "Valores limitados para excluir outliers extremos",
    x = "Região", 
    y = "Gasto Total em ASPS"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "none"
  ) +
  scale_fill_brewer(palette = "Set2")


############## Apenas as Capitais #########################

#### Filtrar Dados das Capitais #####
ids_capitais <- c(
  120040, 270430, 160030, 130260, 292740, 230440, 
  530010, 320530, 520870, 211130, 510340, 500270, 
  310620, 150140, 250750, 410690, 261160, 221100, 
  330455, 240810, 431490, 110020, 140010, 420540, 
  355030, 280030, 172100
)

dados_capitais <- dados_munic %>% filter(cod_ibge %in% ids_capitais)

# Calcular o Gini total e per capita para as capitais
gini_total_capitais <- Gini(dados_capitais$despesa_asps_2022, na.rm = TRUE)
gini_percapita_capitais <- Gini(dados_capitais$despesa_asps_per_capita_2022, na.rm = TRUE)

# Calcular as estatísticas para as capitais, por região
gini_por_regiao_capitais <- dados_capitais %>%
  group_by(regiao) %>%
  summarise(
    gini_total = Gini(despesa_asps_2022, na.rm = TRUE),
    gasto_medio = mean(despesa_asps_2022, na.rm = TRUE),
    gini_percapita = Gini(despesa_asps_per_capita_2022, na.rm = TRUE),
    gasto_medio_per_capita = mean(despesa_asps_per_capita_2022, na.rm = TRUE),
    gasto_percapita = sum(despesa_asps_2022, na.rm = TRUE) / sum(populacao, na.rm = TRUE)
  )

# Calcular as estatísticas para as capitais, por quartil de PIB
gini_por_quartil_capitais <- dados_capitais %>%
  group_by(quartil_PIB) %>%
  summarise(
    gini_total = Gini(despesa_asps_2022, na.rm = TRUE),
    gasto_medio = mean(despesa_asps_2022, na.rm = TRUE),
    gini_percapita = Gini(despesa_asps_per_capita_2022, na.rm = TRUE),
    gasto_medio_per_capita = mean(despesa_asps_per_capita_2022, na.rm = TRUE),
    gasto_percapita = sum(despesa_asps_2022, na.rm = TRUE) / sum(populacao, na.rm = TRUE)
  )

# Calcular o Gini por município (cada capital)
gini_por_municipio <- dados_capitais %>%
  group_by(cod_ibge, nome_municipio) %>%
  summarise(
    gini_total = Gini(despesa_asps_2022, na.rm = TRUE),
    gini_percapita = Gini(despesa_asps_per_capita_2022, na.rm = TRUE),
    .groups = 'drop'
  )

# Gerar tabelas em HTML com kableExtra
html_tabela_gini_por_regiao_capitais <- kable(gini_por_regiao_capitais, format = "html", caption = "Gini por Região para as Capitais") %>%
  kable_styling(full_width = FALSE)

html_tabela_gini_por_quartil_capitais <- kable(gini_por_quartil_capitais, format = "html", caption = "Gini por Quartil de PIB para as Capitais") %>%
  kable_styling(full_width = FALSE)

html_tabela_gini_por_municipio <- kable(gini_por_municipio, format = "html", caption = "Gini por Município para as Capitais") %>%
  kable_styling(full_width = FALSE)

# Salvar as tabelas como arquivos HTML
write(html_tabela_gini_por_regiao_capitais, file = "gini_por_regiao_capitais.html")
write(html_tabela_gini_por_quartil_capitais, file = "gini_por_quartil_capitais.html")
write(html_tabela_gini_por_municipio, file = "gini_por_municipio_capitais.html")


#### Visualizações Ajustadas para Capitais ####
# Histograma
ggplot(dados_capitais, aes(x = despesa_asps_2022)) +
  geom_histogram(aes(y = ..density..), 
                 bins = 30, 
                 fill = "#69b3a2", 
                 color = "black", 
                 alpha = 0.7) +
  geom_density(color = "darkblue", size = 1) +
  scale_x_log10(labels = label_number_si()) +
  labs(
    title = "Distribuição do Gasto Total em Saúde pelas Capitais do Brasil",
    subtitle = "Dados transformados em escala logarítmica para melhor visualização",
    x = "Gasto Total em ASPS (Log10)", 
    y = "Densidade de Frequência"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, vjust = 0.5),
    axis.text.y = element_text(size = 12),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "grey80")
  )

# Boxplot
ggplot(dados_capitais, aes(x = regiao, y = despesa_asps_2022)) +
  geom_boxplot(aes(fill = regiao), 
               color = "black",  # Borda preta nas caixas
               outlier.shape = 16,  # Forma dos outliers (círculo)
               outlier.colour = "red") +  # Cor dos outliers
  scale_fill_brewer(palette = "Set2") +  # Cores suaves
  scale_y_continuous(labels = label_number_si()) +  # Formatação legível do eixo Y
  labs(
    title = "Boxplot do Gasto Total em Saúde por Região",
    x = "Região", 
    y = "Gasto Total em ASPS"
  ) +
  theme_minimal(base_size = 14) +  # Tema minimalista
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),  # Título centralizado
    plot.subtitle = element_text(hjust = 0.5),  # Subtítulo centralizado
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotacionar rótulos do eixo X
    axis.text.y = element_text(size = 12),  # Tamanho do texto no eixo Y
    legend.position = "none"  # Remover legenda
  )
