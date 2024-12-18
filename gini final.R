# Carregar pacotes necess�rios
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
library(knitr)

# 1. Carregar dados principais
setwd("C:\\Users\\buril\\OneDrive\\�rea de Trabalho\\aeae\\documnto bsb\\planilhas\\ANPEC\\INDICADORES SIOPS")

despesas_recursos_proprios <- read_xlsx("Despesas Recursos Pr�prios por Ano Segundo capitais (2003-2023).xlsx")
receita_transferencia <- read_xlsx("Transfer�ncias SUS por ano Segundo capitais (2003-2023).xlsx")
despesas_total <- read_xlsx("Despesa Total por Ano segundo capitais (2003-2023).xlsx")

# 2. Ajustar os dados de despesas sem a corre��o pela infla��o (IPCA)
despesas_final <- left_join(despesas_recursos_proprios, receita_transferencia, by = "Capitais")

despesas_long <- despesas_final %>%
  pivot_longer(cols = starts_with("Recursos_proprios_") | starts_with("Transferencia_"),
               names_to = c(".value", "ano"),
               names_pattern = "(.+)_(\\d+)") %>%
  mutate(ano = as.integer(ano)) %>%
  mutate(
    orcamento_total = Recursos_proprios + Transferencia,  # Sem corre��o pela infla��o
    proporcao_proprio = Recursos_proprios / orcamento_total, # Propor��o sem corre��o
    receita_transferencia = Transferencia / orcamento_total # Propor��o sem corre��o
  )

# 3. Plot do gasto (gr�fico de �rea)
df_plot <- despesas_long %>%
  pivot_longer(cols = c(proporcao_proprio, receita_transferencia),
               names_to = "tipo_gasto",
               values_to = "proporcao")

df_plot$Capitais <- factor(df_plot$Capitais, 
                           levels = c("Campo Grande", "Cuiaba", "Goiania",       # Centro-Oeste
                                      "Aracaju", "Fortaleza", "Joao Pessoa", "Maceio", "Natal", "Recife", "Salvador", "Sao Luis", "Teresina", # Nordeste
                                      "Belem", "Boa Vista", "Macapa", "Manaus", "Palmas", "Porto Velho", "Rio Branco", # Norte
                                      "Belo Horizonte", "Rio de Janeiro", "Sao Paulo", "Vitoria", # Sudeste
                                      "Curitiba", "Florianopolis", "Porto Alegre")) # Sul

# Criando o gr�fico
ggplot(df_plot, aes(x = ano, y = proporcao, fill = tipo_gasto)) +
  geom_area(position = 'stack') +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  facet_wrap(~ Capitais, scales = "free_x", ncol = 5) + # Ajusta o n�mero de colunas
  labs(title = "Participa��o no gasto por fonte por Capital (2002 - 2022)",
       x = "Ano",
       y = "Propor��o (%)",
       fill = "Tipo de Fonte") +
  scale_fill_discrete(labels = c("proporcao_proprio" = "Recurso pr�prio",
                                 "receita_transferencia" = "Recurso de Transfer�ncia")) +
  theme_minimal() +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8), # Inclina e reduz o tamanho do texto
        axis.title.x = element_text(size = 10),
        strip.text = element_text(size = 10),
        panel.spacing = unit(0.5, "lines")) # Aumenta o espa�o entre os pain�is

# 4. Ajuste no pivot_longer para criar um valor distinto por ano para as vari�veis

estatisticas_transferencias <- despesas_final %>% 
  pivot_longer(cols = starts_with("transferencia_"), 
               names_to = "ano", 
               values_to = "transferencia") %>% 
  mutate(ano = as.integer(gsub("[^0-9]", "", ano))) %>%  # Extrai o ano do nome da vari�vel
  filter(!is.na(ano)) %>%  # Remove linhas com 'ano' inv�lido
  mutate(transferencia = as.numeric(transferencia)) %>%  # Garantir que a vari�vel 'transferencia' seja num�rica
  filter(!is.na(transferencia)) %>% 
  mutate(transferencia_per_capita = transferencia / Popula��o_2022)  # Transfer�ncia per capita (usando popula��o de 2022)

# 5. Calcular as estat�sticas descritivas por capital
estatisticas_por_regiao <- estatisticas_transferencias %>%
  group_by(regiao) %>%  # Agrupar por capital
  summarise(
    total_transferencias = sum(transferencia, na.rm = TRUE),  
    media_transferencias = mean(transferencia, na.rm = TRUE),  
    sd_transferencias = sd(transferencia, na.rm = TRUE),
    transferencia_per_capita = mean(transferencia_per_capita, na.rm = TRUE),  # M�dia de transfer�ncia per capita
    .groups = "drop"
  ) %>%
  arrange(transferencia_per_capita)  # Ordenar em ordem crescente pela transfer�ncia per capita

# 6. Exibir as primeiras linhas para verificar o resultado
print(estatisticas_por_regiao)

#exportando excel
wb <- createWorkbook()
addWorksheet(wb, "Estat�sticas por Regi�o")

# Exportar a tabela para Excel
writeDataTable(
  wb, 
  sheet = "Estat�sticas por Regi�o", 
  estatisticas_por_regiao,
  withFilter = TRUE, # Adicionar filtros na tabela
  tableStyle = "TableStyleMedium2" # Estilo da tabela
)

# Salvar o arquivo Excel
saveWorkbook(wb, "estatisticas_transferencias.xlsx", overwrite = TRUE)
 
# Criar o gr�fico
estatisticas_transferencias$Capitais <- factor(estatisticas_transferencias$Capitais, 
                           levels = c("Campo Grande", "Cuiaba", "Goiania",       # Centro-Oeste
                                      "Aracaju", "Fortaleza", "Joao Pessoa", "Maceio", "Natal", "Recife", "Salvador", "Sao Luis", "Teresina", # Nordeste
                                      "Belem", "Boa Vista", "Macapa", "Manaus", "Palmas", "Porto Velho", "Rio Branco", # Norte
                                      "Belo Horizonte", "Rio de Janeiro", "Sao Paulo", "Vitoria", # Sudeste
                                      "Curitiba", "Florianopolis", "Porto Alegre")) # Sul

ggplot(estatisticas_transferencias, aes(x = ano, y = transferencia_per_capita, group = Capitais, color = Capitais)) +
  geom_line() +
  facet_wrap(~ Capitais, scales = "free_y", ncol = 5, labeller = label_wrap_gen()) +  # Corrigido par�ntese
  labs(
    title = "Receitas de transfer�ncias per Capita por Capital (2002 - 2022)", 
    x = "Ano",
    y = "Transfer�ncia per Capita (R$)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Melhorar legibilidade dos r�tulos no eixo x
    strip.text = element_text(size = 10),  # Ajustar tamanho dos t�tulos dos pain�is
    panel.spacing = unit(0.8, "lines")  # Espa�amento entre os pain�is
  ) +
  scale_color_viridis_d() +
  guides(color = "none")  # Remove a legenda lateral das capitais

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
                 alpha = 0.7) +     # Transparência para suavizar o visual
  geom_density(color = "darkblue", 
               size = 1) +          # Linha de densidade suave
  scale_x_log10(labels = label_number_si()) +  # Transformação logarítmica e formatação de números legíveis
  labs(
    title = "Distribui��o do Gasto Total em S�ude por Munic�pio no Brasil",
    subtitle = "Dados transformados em escala logaritmica para melhor visualiza��o",
    x = "Gasto Total em ASPS (Log10)", 
    y = "Densidade de Frequ�ncia"
  ) +
  theme_minimal(base_size = 14) +   # Tema minimalista com fonte maior
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),  # Centraliza o título e o coloca em negrito
    plot.subtitle = element_text(hjust = 0.5),             # Centraliza o subtítulo
    axis.text.x = element_text(angle = 45, vjust = 0.5),   # Rotaciona os rótulos do eixo X
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
  scale_y_continuous(labels = label_number_si()) +  # Formatação do eixo Y para números legíveis
  labs(
    title = "Boxplot do Gasto Total em Sa�de por Regi�o no Brasil",
    subtitle = "Valores limitados para excluir outliers extremos",
    x = "Regi�o", 
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

# Calcular as estat�sticas para as capitais, por regi�o
gini_por_regiao_capitais <- dados_capitais %>%
  group_by(regiao) %>%
  summarise(
    gini_total = Gini(despesa_asps_2022, na.rm = TRUE),
    gasto_medio = mean(despesa_asps_2022, na.rm = TRUE),
    gini_percapita = Gini(despesa_asps_per_capita_2022, na.rm = TRUE),
    gasto_medio_per_capita = mean(despesa_asps_per_capita_2022, na.rm = TRUE),
    gasto_percapita = sum(despesa_asps_2022, na.rm = TRUE) / sum(populacao, na.rm = TRUE)
  )

# Calcular as estat�sticas para as capitais, por quartil de PIB
gini_por_quartil_capitais <- dados_capitais %>%
  group_by(quartil_PIB) %>%
  summarise(
    gini_total = Gini(despesa_asps_2022, na.rm = TRUE),
    gasto_medio = mean(despesa_asps_2022, na.rm = TRUE),
    gini_percapita = Gini(despesa_asps_per_capita_2022, na.rm = TRUE),
    gasto_medio_per_capita = mean(despesa_asps_per_capita_2022, na.rm = TRUE),
    gasto_percapita = sum(despesa_asps_2022, na.rm = TRUE) / sum(populacao, na.rm = TRUE)
  )

# Calcular o Gini por munic�pio (cada capital)
gini_por_municipio <- dados_capitais %>%
  group_by(cod_ibge, munic) %>%
  summarise(
    gini_total = Gini(despesa_asps_2022, na.rm = TRUE),
    gini_percapita = Gini(despesa_asps_per_capita_2022, na.rm = TRUE),
    .groups = 'drop'
  )

# Gerar tabelas em LaTeX com knitr
latex_tabela_gini_por_regiao_capitais <- kable(gini_por_regiao_capitais, format = "latex", caption = "Gini por Regi�o para as Capitais") %>%
  kable_styling(full_width = FALSE)

latex_tabela_gini_por_quartil_capitais <- kable(gini_por_quartil_capitais, format = "latex", caption = "Gini por Quartil de PIB para as Capitais") %>%
  kable_styling(full_width = FALSE)

latex_tabela_gini_por_municipio <- kable(gini_por_municipio, format = "latex", caption = "Gini por Munic�pio para as Capitais") %>%
  kable_styling(full_width = FALSE)

# Salvar as tabelas como arquivos LaTeX
write(latex_tabela_gini_por_regiao_capitais, file = "gini_por_regiao_capitais.tex")
write(latex_tabela_gini_por_quartil_capitais, file = "gini_por_quartil_capitais.tex")
write(latex_tabela_gini_por_municipio, file = "gini_por_municipio_capitais.tex")

#### Visualiza��es Ajustadas para Capitais ####
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
    title = "Distribui��o do Gasto Total em Sa�de pelas Capitais do Brasil",
    subtitle = "Dados transformados em escala logar�tmica para melhor visualiza��o",
    x = "Gasto Total em ASPS (Log10)", 
    y = "Densidade de Frequ�ncia"
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
               outlier.shape = 16,  # Forma dos outliers (c�rculo)
               outlier.colour = "red") +  # Cor dos outliers
  scale_fill_brewer(palette = "Set2") +  # Cores suaves
  scale_y_continuous(labels = label_number_si()) +  # Formata��o leg�vel do eixo Y
  labs(
    title = "Boxplot do Gasto Total em ASPS por Regi�o",
    x = "Regi�o", 
    y = "Gasto Total em ASPS"
  ) +
  theme_minimal(base_size = 14) +  # Tema minimalista
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),  # T�tulo centralizado
    plot.subtitle = element_text(hjust = 0.5),  # Subt�tulo centralizado
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotacionar r�tulos do eixo X
    axis.text.y = element_text(size = 12),  # Tamanho do texto no eixo Y
    legend.position = "none"  # Remover legenda
  )