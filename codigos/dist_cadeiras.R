################################################################################
################################################################################

##### Distribuicao de cadeiras na Camara dos Deputados brasileira

# Limpa o environment, se necessario
#rm(list = ls())

# Pacotes
library(dplyr)
library(sidrar)
library(janitor)
library(readr)
library(stringr)
library(googlesheets4)
library(ggplot2)
library(tidyr)
library(forcats)

################################################################################

##### Importacao de dados relevantes

### Populacao (Censo-IBGE)

# 2010
populacao2010 <- get_sidra(136, geo = c("State")) |> 
  clean_names() |> 
  filter(ano == 2010,
         cor_ou_raca == "Total") |> 
  group_by(unidade_da_federacao) |> 
  summarise(pop2010 = sum(valor, na.rm = T)) |> 
  ungroup() |> 
  select(unidade_da_federacao, pop2010) |> 
  mutate(uf = toupper(unidade_da_federacao))

# 2022
populacao2022 <- get_sidra(4709, geo = c("State")) |> 
  clean_names() |> 
  filter(variavel == "População residente") |> 
  select(unidade_da_federacao, valor) |> 
  rename(pop2022 = valor) |> 
  mutate(uf = toupper(unidade_da_federacao))

### Eleitorado atual (2025, TSE)

# # Baixa o arquivo de eleitorado atual
# download.file(
#   "https://cdn.tse.jus.br/estatistica/sead/odsele/perfil_eleitorado/perfil_eleitorado_ATUAL.zip",
#   "dados/eleitorado_atual.zip")
# 
# # Descomprime o arquivo
# unzip(
#   "eleitorado_atual.zip",
#   exdir = "dados/eleitorado_atual")
#
# # Apaga o arquivo zipado
#file.remove("dados/eleitorado_atual.zip")

# Importa o arquivo de eleitorado
eleitorado <- read_csv2("dados/eleitorado_atual/perfil_eleitorado_ATUAL.csv",
                        locale = locale(encoding = "latin1")) |> 
  # Soma o n. de eleitores por UF
  group_by(SG_UF) |> 
  summarise(eleitorado2025 = sum(QT_ELEITORES_PERFIL, na.rm = T)) |> 
  ungroup() |> 
  clean_names()

### Distribuicao de cadeiras

# Atual
magnitude <- read_csv(
  "https://github.com/GV-CEPESP/cepespdata/raw/refs/heads/main/tabelas_auxiliares/dados/vagas_depfed_aux.csv",
  locale = locale(encoding = "latin1")) |> 
  select(NM_UE, SG_UF, VAGAS_2022) |> 
  clean_names() |> 
  rename(cadeiras_atual = vagas_2022)

# Proposta aprovada pela Camara dos Deputados
proposta_camara <- tibble(
  sg_uf = magnitude$sg_uf,
  cadeiras_atual = magnitude$cadeiras_atual) |> 
  mutate(
    dist_camara = case_when(
      sg_uf == "CE" ~ cadeiras_atual + 1,
      sg_uf == "GO" ~ cadeiras_atual + 1,
      sg_uf == "MG" ~ cadeiras_atual + 1,
      sg_uf == "PR" ~ cadeiras_atual + 1,
      sg_uf == "AM" ~ cadeiras_atual + 2,
      sg_uf == "MT" ~ cadeiras_atual + 2,
      sg_uf == "RN" ~ cadeiras_atual + 2,
      sg_uf == "PA" ~ cadeiras_atual + 4,
      sg_uf == "SC" ~ cadeiras_atual + 4,
      T ~ cadeiras_atual)) |> 
  select(sg_uf, dist_camara)

### Unifica todas as informacoes
dados <- populacao2010 |> 
  left_join(populacao2022) |> 
  left_join(magnitude, by = c("uf" = "nm_ue")) |> 
  left_join(eleitorado, by = "sg_uf") |> 
  left_join(proposta_camara, by = "sg_uf") |> 
  select(sg_uf, pop2010, pop2022, eleitorado2025, cadeiras_atual, dist_camara)

################################################################################

##### Calculo da distribuicao de cadeiras por diferentes metodos

### Funcao para distribuir cadeiras

maiores_medias_camara <- function(banco, n_cadeiras, populacao){
  
  ### Calcula os valores da primeira distribuicao de cadeiras
  tabela <- banco |> 
    mutate(
      # Quociente populacional nacional
      quociente = sum({{populacao}})/n_cadeiras,
      # Quociente populacional estadual
      dist1 = trunc({{populacao}}/quociente),
      # Trava as cadeiras no maximo
      vagas_qp = ifelse(dist1 > 70, 70, dist1),
      vagas_media = 0)
  
  ### Descobre quais UFs atingiriam no maximo 8 cadeiras
  
  # Descobre o n. de cadeiras ainda a serem distribuidas
  cadeiras_a_distribuir <- n_cadeiras - sum(tabela$vagas_qp)
  
  while(cadeiras_a_distribuir > 0){
    
    tabela <- tabela |> 
      mutate(
        # Calcula as medias
        sobra_media = {{populacao}}/(vagas_qp + 1 + vagas_media),
        # Retira SP da disputa pelas sobras
        sobra_media = ifelse(sg_uf == "SP", 0, sobra_media),
        # Da a cadeira da rodada para aquele que tem a maior media
        vagas_media = ifelse(sobra_media == max(sobra_media), 
                             vagas_media + 1, vagas_media))
    
    # Checa o n. de cadeiras que faltam distribuir
    cadeiras_a_distribuir <- n_cadeiras - sum(tabela$vagas_qp) - sum(tabela$vagas_media, na.rm = T)
    
  }
  
  # Cria vetor com as UFs que ficam em no maximo 8 cadeiras
  limite_70 <- tabela |> 
    mutate(dist_final = vagas_qp + vagas_media) |> 
    filter(dist_final >= 70) |> 
    pull(sg_uf)
  
  # Cria vetor com as UFs que ficam em no maximo 8 cadeiras
  limite_8 <- tabela |> 
    mutate(dist_final = vagas_qp + vagas_media) |> 
    filter(dist_final <= 8) |> 
    pull(sg_uf)
  
  ### Distribui novamente as cadeiras, fixando as UFs com 8 ou 70
  
  # Primeira distribuicao
  tabela <- banco |> 
    mutate(
      # Quociente populacional nacional
      quociente = sum({{populacao}})/n_cadeiras,
      # Quociente populacional estadual
      dist1 = trunc({{populacao}}/quociente),
      # Trava as cadeiras no maximo
      vagas_qp = ifelse(sg_uf %in% limite_70, 70, dist1),
      # Trava as cadeiras no minimo
      vagas_qp = ifelse(sg_uf %in% limite_8, 8, vagas_qp),
      vagas_media = 0)
  
  # Verifica quantas cadeiras a serem distribuidas
  cadeiras_a_distribuir <- n_cadeiras - sum(tabela$vagas_qp)
  
  # Enquanto ainda houver cadeiras a serem distribuidas
  while(cadeiras_a_distribuir > 0){
    
    tabela <- tabela |> 
      mutate(
        # Calcula as medias
        sobra_media = {{populacao}}/(vagas_qp + 1 + vagas_media),
        # Retira os estados com 8 ou 70 cadeiras da distribuicao
        sobra_media = ifelse(sg_uf %in% limite_70, 0, sobra_media),
        sobra_media = ifelse(sg_uf %in% limite_8, 0, sobra_media),
        # Da a cadeira da rodada para aquele que tem a maior media
        vagas_media = ifelse(sobra_media == max(sobra_media), 
                             vagas_media + 1, vagas_media))
    
    # Checa o n. de cadeiras que faltam distribuir
    cadeiras_a_distribuir <- n_cadeiras - sum(tabela$vagas_qp) - sum(tabela$vagas_media, na.rm = T)
    
  }
  
  # Calcula a distribuicao final, somando aquelas do quociente e da media
  tabela <- tabela |> 
    mutate(nova_dist = vagas_qp + vagas_media) |> 
    select(sg_uf, nova_dist)
  
  return(tabela)
  
}

# Guarda o resultado de diferentes estimativas numa lista
estimativas <- list(
  # N. cadeiras: 513 | Pop. ref: Censo-2010
  maiores_medias_camara(dados, 513, pop2010) |> 
    rename(dist_513_2010 = nova_dist),
  # N. cadeiras: 513 | Pop. ref: Censo-2022
  maiores_medias_camara(dados, 513, pop2022) |> 
    rename(dist_513_2022 = nova_dist),
  # N. cadeiras: 513 | Pop. ref: Eleitorado-2025
  maiores_medias_camara(dados, 513, eleitorado2025) |> 
    rename(dist_513_2025 = nova_dist),
  # N. cadeiras: 531 | Pop. ref: Censo-2010
  maiores_medias_camara(dados, 531, pop2010) |> 
    rename(dist_531_2010 = nova_dist),
  # N. cadeiras: 531 | Pop. ref: Censo-2022
  maiores_medias_camara(dados, 531, pop2022) |> 
    rename(dist_531_2022 = nova_dist),
  # N. cadeiras: 531 | Pop. ref: Eleitorado-2025
  maiores_medias_camara(dados, 531, eleitorado2025) |> 
    rename(dist_531_2025 = nova_dist))

# Une os resultados das estimativas em uma tabela
estimativas <- estimativas[[1]] |> 
  left_join(estimativas[[2]]) |> 
  left_join(estimativas[[3]]) |> 
  left_join(estimativas[[4]]) |> 
  left_join(estimativas[[5]]) |> 
  left_join(estimativas[[6]])  

################################################################################

##### Avalia a proporcionalidade dos resultados

### Organiza os dados

# Unifica os resultados das estimativas
final <- left_join(dados, estimativas)

# Calcula as proporcoes de cadeiras em cada distribuicao, e nas populacoes
final_proporcoes <- final |>
  mutate(
    across(
      c(-sg_uf), 
      function(x) round_half_up(100*x/sum(x, na.rm = T), 2), 
      .names = "prop_{.col}"))

### Calcula metricas de desproporcionalidade

# Funcao para calculo do indice de Gallagher
gallagher <- function(prop_pop, prop_cadeiras){
  
  sqrt((1/2)*(sum((prop_cadeiras-prop_pop)^2)))
  
}

# Calcula diferentes metricas de desproporcionalidade
metricas <- final_proporcoes |> 
  summarise(
    # Indice de Gallagher
    lsq_atual = gallagher(prop_cadeiras_atual, prop_pop2022),
    lsq_camara = gallagher(prop_dist_camara, prop_pop2022),
    lsq_513_elei = gallagher(prop_dist_513_2025, prop_pop2022),
    lsq_513_2022 = gallagher(prop_dist_513_2022, prop_pop2022),
    lsq_531_elei = gallagher(prop_dist_531_2025, prop_pop2022),
    lsq_531_2022 = gallagher(prop_dist_531_2022, prop_pop2022))

# Reorganiza a tabela de resultados de desproporcionalidade
metricas_final <- tibble(
  resultado = as.vector(t(metricas)[,1]),
  medidas = names(metricas)) |> 
  mutate(
    estimativa = str_remove_all(medidas, "^(.*?)_"),
    medida = str_extract(medidas, "^(.*?)_"),
    medida = str_remove(medida, "_")) |> 
  select(medida, estimativa, resultado) |> 
  arrange(medida, resultado) 

################################################################################

##### Comparacao da desproporcionalidade por diferentes metodos

metricas_final |> 
  # Cria labels para a variavel estimativa, para melhorar viz
  mutate(estimativa = factor(
    estimativa,
    levels = c("513_2022", "513_elei", "atual", "531_2022", "camara", "531_elei"),
    labels = c("Censo-2022\n(Cadeiras: 513)", "Eleitorado-2025\n(Cadeiras: 513)",
               "Atual\n(Cadeiras: 513)", "Censo-2022\n(Cadeiras: 531)",
               "Nova regra Câmara\n(Cadeiras: 531)", "Eleitorado-2025\n(Cadeiras: 531)")),
    resultado = round_half_up(resultado, 2)) |> 
  ggplot() +
  geom_point(aes(x = estimativa, y = resultado)) +
  geom_line(aes(x = estimativa, y = resultado, group = "valor")) +
  geom_label(
    aes(x = estimativa, y = resultado + 0.5, label = resultado),
    size = 2.2) +
  scale_y_continuous(limits = c(0, 7)) +
  labs(
    title = "Desproporcionalidade na distribuição de cadeiras\nna Câmara dos Deputados - simulações",
    y = "Índice de Gallagher\n(Desproporcionalidade)",
    caption = "Fonte: elaborado por Mesquita e Gelape (2025),\ncom base em dados do IBGE, TSE e Câmara dos Deputados.") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 8), 
    plot.caption = element_text(hjust = 0.5, size = 4), 
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 6),
    axis.text = element_text(size = 5))
#ggsave("relatorios/figuras/lsq_comparacao.png", width = 4.5, height = 3)

################################################################################

##### Perda/ganho de cadeiras por diferentes metodos

# Calcula a diferenca de cadeiras em cada metodo
diferencas <- final |> 
  select(sg_uf, cadeiras_atual, dist_camara, dist_513_2022, dist_531_2022) |> 
  pivot_longer(
    cols = c(starts_with("dist")),
    names_to = "tipo",
    values_to = "cadeiras_novas") |> 
  mutate(tipo = str_remove(tipo, "dist_"),
         diferenca = cadeiras_novas - cadeiras_atual)

# Plota o grafico
diferencas |> 
  # Elimina as UF que nao tem diferencas
  filter(diferenca != 0) |> 
  # Categoriza os que perderam/ganharam para colorir as barras
  mutate(sg_uf = fct_rev(sg_uf),
         ganho = ifelse(diferenca < 0, "Perda", "Ganho"),
         ganho = factor(ganho, levels = c("Perda", "Ganho"))) |> 
  # Cria labels para os rotulos
  mutate(tipo = factor(
    tipo,
    levels = c("atual", "513_2022", "camara", "531_2022"),
    labels = c("Distribuição\natual",  
               "Censo-2022\n(Cadeiras: 513)", "Nova regra Câmara\n(Cadeiras: 531)", 
               "Censo-2022\n(Cadeiras: 531)"))) |> 
  ggplot() +
  geom_col(aes(x = diferenca, y = sg_uf, fill = ganho), 
           color = "black", linewidth = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.3) +
  facet_wrap(~ tipo, nrow = 1) +
  scale_x_continuous(breaks = seq(-4, 5, 1)) +
  scale_fill_manual(values = c("darkred", "darkgreen")) +
  guides(fill = "none") +
  labs(
    title = "Diferença no n. de cadeiras sob diferentes regras - simulações",
    x = "N. de cadeiras",
    caption = "Fonte: elaborado por Mesquita e Gelape (2025),\ncom base em dados do IBGE, TSE e Câmara dos Deputados.") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 8), 
    plot.caption = element_text(hjust = 0.5, size = 4),
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 6),
    axis.text = element_text(size = 5),
    strip.text = element_text(size = 6),
    panel.grid.major = element_line(linewidth = 0.2),
    panel.grid.minor = element_blank()) 
#ggsave("relatorios/figuras/diferencas_cadeiras.jpg", width = 4, height = 3)

################################################################################

##### Avalia a proporcionalidade dos resultados somente daqueles com 8 < M < 70

# Calcula proporcoes de cadeiras atual e do PLP, com base nesse universo de UF
final_prop_variaveis <- final |>
  select(sg_uf, pop2022, cadeiras_atual, dist_camara) |> 
  mutate(
    cadeiras_atual = case_when(
      cadeiras_atual == 8 ~ NA,
      cadeiras_atual == 70 ~ NA,
      T ~ cadeiras_atual),
    dist_camara = case_when(
      dist_camara == 8 ~ NA,
      dist_camara == 70 ~ NA,
      T ~ dist_camara),
    across(
      c(-sg_uf), 
      function(x) round_half_up(100*x/sum(x, na.rm = T), 2), 
      .names = "prop_{.col}"))

## Calcula desproporcionalidade nas UFs que 8 < M < 70

# Atual
desp_atual <- final_prop_variaveis |> 
  select(sg_uf, cadeiras_atual, pop2022) |> 
  filter(!is.na(cadeiras_atual)) |> 
  mutate(prop_pop2022 = 100*pop2022/sum(pop2022),
         prop_cadeiras_atual = 100*cadeiras_atual/sum(cadeiras_atual)) |> 
  summarise(desproporcionalidade = gallagher(prop_cadeiras_atual, prop_pop2022)) |> 
  mutate(n_cadeiras = 513, marco = "Atual", serie = NA)

# PLP
desp_nova <- final_prop_variaveis |> 
  select(sg_uf, dist_camara, pop2022) |> 
  filter(!is.na(dist_camara)) |> 
  mutate(prop_pop2022 = 100*pop2022/sum(pop2022),
         prop_dist_camara = 100*dist_camara/sum(dist_camara)) |> 
  summarise(desproporcionalidade = gallagher(prop_dist_camara, prop_pop2022)) |> 
  mutate(n_cadeiras = 531, marco = "Nova regra", serie = NA)

# Calcula a desproporcionalidade para intervalo entre 500-650 cadeiras
tabela_variaveis <- NULL

for(cadeiras in 500:650){
  
  # Calcula a distribuicao de cadeiras
  indice <- maiores_medias_camara(dados, cadeiras, pop2022) |> 
    # Restringe as UFs com 8 < M < 70
    filter(nova_dist > 8 & nova_dist < 70) |> 
    left_join(final_prop_variaveis, by = "sg_uf") |> 
    mutate(
      prop_pop2022 = 100*pop2022/sum(pop2022),
      prop_distribuicao = 100*nova_dist/sum(nova_dist)) |> 
    summarise(desp = gallagher(prop_distribuicao, prop_pop2022)) |> 
    pull()
  
  # Faz uma tabela da iteracao
  iteracao <- tibble(
    n_cadeiras = cadeiras,
    desproporcionalidade = indice)
  
  # Empilha entre iteracoes
  tabela_variaveis <- bind_rows(tabela_variaveis, iteracao)
  
}

# Plota o grafico
tabela_variaveis |> 
  mutate(
    # Cria variavel mostrando o n. de cadeiras da distribuicao atual e do PLP
    marco = case_when(
      n_cadeiras == 513 ~ "Metod. de distribuição: PLP/TSE",
      n_cadeiras == 531 ~ "Metod. de distribuição: PLP/TSE",
      T ~ NA),
    serie = "simulada") |> 
  bind_rows(desp_atual, desp_nova) |> 
  ggplot() +
  geom_line(aes(x = n_cadeiras, y = desproporcionalidade, group = serie),
            color = "darkgrey") +
  geom_point(aes(x = n_cadeiras, y = desproporcionalidade, 
                 color = marco, shape = marco), size = 2) +
  geom_hline(yintercept = 0.2400956, linetype = "dotted") +
  scale_x_continuous(breaks = seq(500, 650, 10)) +
  scale_y_continuous(limits = c(0.0, 1.8)) +
  scale_color_manual(values = c("darkblue", "purple", "darkorange"), na.translate = F) +
  scale_shape_manual(values = c(15, 16, 17), na.translate = F) +
  labs(title = "Desproporcionalidade pelo n. de cadeiras - Simulações",
       subtitle = "Excluindo UFs que teriam 8 ou 70 cadeiras",
       x = "N. de cadeiras", y = "Desproporcionalidade\n(Índice de Gallagher)",
       caption = "Fonte: elaborado por Mesquita e Gelape (2025),\ncom base em dados do IBGE, TSE e Câmara dos Deputados.") +
  guides(color = guide_legend(title = NULL),
         shape = guide_legend(title = NULL)) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 8), 
    plot.subtitle = element_text(hjust = 0.5, size = 6), 
    plot.caption = element_text(hjust = 0.5, size = 4),
    axis.title = element_text(size = 6),
    axis.text = element_text(size = 5),
    panel.grid.major = element_line(linewidth = 0.25),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.box.spacing = unit(0.25, "cm"),
    legend.key.height = unit(0.5, "cm"),
    legend.key.width = unit(0.5, "cm"),
    legend.text = element_text(size = 5),
    legend.margin = margin(c(0, 0, 0, 0)))
#ggsave("relatorios/figuras//desproporcionalidade_cadeiras.png", width = 4, height = 3)
