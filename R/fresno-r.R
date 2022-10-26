

# trabalho final ----------------------------------------------------------

# Instruções

# Escolha uma base de dados que você ache interessante!
# Defina um objetivo da análise.
# Defina um público alvo: a comunicação depende de quem vai ver.
# Defina qual formato você quer gerar.

# Pontos de avaliação: Reprodutibilidade, efetividade da comunicação, atributos estéticos, engajamento na leitura.

# O resultado deverá ser entregue em um relatório ou apresentação à sua escolha. A submissão deve ser feita pelo classroom, subindo um arquivo .zip  ou o link de um repositório no GitHub contendo
# Arquivo .qmd (gostaríamos de conseguir rodá-lo, ou seja, se ele for reprodutível é melhor)
# Códigos auxiliares em R (se existirem)
# O output final (em HTML, PDF, Word, etc). Você pode escolher o formato do trabalho final.
# O relatório/apresentação deve conter uma breve descrição da base de dados.
# o objetivo da análise
# os resultados da sua análise, com explicações/interpretações das visualizações construídas
# uma conclusão tirada a partir da análise


# carregando os pacotes ---------------------------------------------------

# devtools::install_github('charlie86/spotifyr') #Instalando o pacote spotifyr
library(spotifyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggrepel)
library(ggpubr)
library(gghighlight)

# ref de trabalho: https://leticialopesdias.github.io/trabalhoCursoRelatorios/docs/
# ref: https://curso-r.github.io/202107-visualizacao/trabalhos_finais/Tales_Gomes/index.html#Tidytuesday
# ref: https://curso-r.github.io/202103-visualizacao/trabalhos_finais/Ariane_Hayana/index.html
# ref: https://github.com/curso-r/202103-visualizacao/tree/master/trabalhos_finais/Ariane_Hayana
# ref: https://curso-r.github.io/202103-visualizacao/trabalhos_finais/Brunna_Escouto/index.html#16
# ref: https://github.com/curso-r/202103-visualizacao/tree/master/trabalhos_finais/Brunna_Escouto
# ref: https://curso-r.github.io/202103-visualizacao/trabalhos_finais/Rafael_Rocha/index.html#10
# ref: https://github.com/curso-r/202103-visualizacao/tree/master/trabalhos_finais/Rafael_Rocha


# tratativa das bases de dados --------------------------------------------

fresno <- readr::read_rds(file = "./dados/fresno.rds")

fresno_musicas <- fresno |>
  dplyr::transmute(
    musica = track_name,
    numero_musica = track_number,
    album = album_name,
    acustica = acousticness,
    dancabilidade = danceability,
    energia = energy,
    instrumentalidade = instrumentalness,
    ao_vivo = liveness,
    discurso = speechiness,
    ritmo = tempo,
    valencia = valence,
    polaridade = sqrt(valencia^2 + energia^2),
    modo = mode,
    tempo_ritmico = time_signature,
    duracao = duration_ms,
    key_name = key_name,
    mode_name = mode_name,
    key_mode = key_mode,
    data_lancamento = album_release_date,
    id = track_id
  ) |>
  dplyr::mutate(
    data_lancamento = lubridate::as_date(data_lancamento)
  ) |>
  dplyr::arrange(
    data_lancamento
  )

fresno_toptracks <- readr::read_rds(file = "./dados/fresno_toptracks.rds")


# construção dos gráficos -------------------------------------------------


# 1. Duração das músicas

fresno_musicas |>
  dplyr::select(musica, numero_musica, album, duracao, data_lancamento) |>
  dplyr::mutate(
    duracao = duracao/60/1000) |>
  pivot_longer(
    cols = "duracao",
    names_to = "caracteristica",
    values_to = "valor"
    ) |>
  dplyr::mutate(
    album = paste0(album, " (", lubridate::year(fresno_musicas$data_lancamento), ")"),
    album = forcats::fct_reorder(album,
                                 desc(data_lancamento))
  ) |>
  ggplot(aes(x = valor,
             y = album,
             color = "#008B8B",
             fill = "#2f3231")) +
  scale_fill_manual(values=c("#2f3231")) +
  scale_color_manual(values = "#008B8B") +
  geom_boxplot(show.legend = FALSE) +
  geom_jitter(show.legend = FALSE, width = 2, size = 1.9, alpha = 0.3, color = "white") +
  labs(
    y = "Álbuns \n",
    x = "\n Duração (min)",
    title = "Duração das músicas",
    subtitle = "Distribuição do tempo das músicas em seus respectivos discos \n",
    color = "Atributo"
  ) +
  theme(
    axis.text = element_text(face = "italic", colour = "white"),
    axis.text.x = element_text(size = 8, colour = "white"),
    axis.title.x = element_text(size = 8, colour = "white"),
    axis.title.y = element_text(size = 8, colour = "white"),
    plot.title = element_text(
      hjust = 0,
      size = 18,
      face = "bold",
      colour = "white"),
    plot.subtitle = element_text(
      hjust = 0,
      size = 12,
      colour = "white"),
    plot.title.position = "plot",
    axis.text.y = element_text(size = 10),
    panel.background = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.background = element_rect(fill = "#2f3231", color = "transparent"),
    legend.background = element_rect(fill = "#2f3231", color = "transparent"),
    legend.box.background = element_rect(fill = "#2f3231", color = "transparent")
  ) +
  xlim(0, 8)

# 2. Polaridade nos álbuns

fresno_musicas |>
  dplyr::mutate(
    ano_lancamento = lubridate::year(fresno_musicas$data_lancamento),
    album = paste0(album, " (", lubridate::year(fresno_musicas$data_lancamento), ")"),
    album = forcats::fct_reorder(album,
                                 data_lancamento)
  ) |>
  dplyr::select(musica, album, ano_lancamento, valencia, energia, polaridade) |>
ggplot2::ggplot(ggplot2::aes(x = energia, y = valencia)) +
  ggplot2::geom_point(size = 2, alpha = 0.3) +
  #scale_color_gradient(low = "white", high = "#008B8B") +
  labs(
    y = "Energia \n",
    x = "\n Valência",
    title = "Sentimento das músicas",
    subtitle = "Distribuição das músicas através da valência e energia \n"
  ) +
  theme(
    axis.text = element_text(face = "italic", colour = "white"),
    axis.text.x = element_text(size = 6, colour = "white"),
    axis.title.x = element_text(size = 12, colour = "white"),
    axis.text.y = element_text(size = 6),
    axis.title.y = element_text(size = 12, colour = "white"),
    plot.title = element_text(
      hjust = 0,
      size = 18,
      face = "bold",
      colour = "white"),
    plot.subtitle = element_text(
      hjust = 0,
      size = 12,
      colour = "white"),
    plot.title.position = "plot",
    plot.background = element_rect(fill = "#2f3231", color = "transparent"),
    panel.border = element_rect(color = "#3b3f3e", fill = NA, size = 0.2),
    panel.background = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.spacing = unit(.5, "lines"),
    legend.background = element_rect(fill = "#2f3231", color = "transparent"),
    legend.box.background = element_rect(fill = "#2f3231", color = "transparent"),
    strip.background = element_rect(fill = "#3b3f3e"),
    strip.text = element_text(color = "white", size = 8),

  ) +
  gghighlight(label_key = musica,
              polaridade >= mean(fresno_musicas$polaridade),
              unhighlighted_params = list(color = "white")) +
  geom_point(color = "#008B8B", size = 4, alpha = 0.3) +
  facet_wrap(~ album)

# 3. Análise da Dançabilidade

fresno_musicas |>
  dplyr::mutate(
    ano_lancamento = as.character(lubridate::year(fresno_musicas$data_lancamento)),
    numero_musica = forcats::fct_reorder(as.character(numero_musica),
                                         numero_musica)
  ) |>
  dplyr::select(musica, numero_musica, album, ano_lancamento, dancabilidade) |>
  dplyr::filter(!dancabilidade == 0) |>
  ggplot2::ggplot(ggplot2::aes(x = numero_musica, y = dancabilidade)) +
  ggplot2::geom_col(color = "#3b3f3e", fill = "#008B8B", alpha = 0.8) +
  scale_y_continuous(limits = c(0,1), expand = c(0, 0)) +
  facet_grid(~ ano_lancamento) +
  labs(
    y = "Dançabilidade \n",
    x = "\n Ordem das faixas",
    title = "Aposta no ritmo e na dança",
    subtitle = "Evolução da 'dançabilidade' nas músicas \n"
  ) +
  theme(
    axis.text.x = element_text(size = 6, colour = "white"),
    axis.title.x = element_text(size = 10, colour = "white"),
    axis.text.y = element_text(size = 6, colour = "white"),
    axis.title.y = element_text(size = 10, colour = "white"),
    plot.title = element_text(
      hjust = 0,
      size = 18,
      face = "bold",
      colour = "white"),
    plot.subtitle = element_text(
      hjust = 0,
      size = 12,
      colour = "white"),
    plot.title.position = "plot",
    plot.background = element_rect(fill = "#2f3231", color = "transparent"),
    panel.border = element_rect(color = "#3b3f3e", fill = NA, size = 0.2),
    panel.background = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.spacing = unit(.1, "lines"),
    legend.background = element_rect(fill = "#2f3231", color = "transparent"),
    legend.box.background = element_rect(fill = "#2f3231", color = "transparent"),
    strip.background = element_rect(fill = "#3b3f3e"),
    strip.text = element_text(color = "white", size = 8)
  )

# 4. Tablela do ritmo

fresno_musicas |>
  dplyr::mutate(
    ano_lancamento = as.character(lubridate::year(fresno_musicas$data_lancamento)),
    numero_musica = forcats::fct_reorder(as.character(numero_musica),
                                         numero_musica),
    album = paste0(album, " (", lubridate::year(fresno_musicas$data_lancamento), ")"),
    album = forcats::fct_reorder(album,
                                 desc(data_lancamento))
  ) |>
  dplyr::select(musica, numero_musica, album, ano_lancamento, ritmo, instrumentalidade, discurso) |>
  dplyr::group_by(album) |>
  dplyr::summarise(
    ritmo_medio = mean(ritmo)
  ) |>
  ggplot2::ggplot(ggplot2::aes(y = album, x = ritmo_medio)) +
  ggplot2::geom_col(color = "#3b3f3e", fill = "#008B8B", alpha = 0.8, width = 0.8) +
  scale_x_continuous(limits = c(0,200), expand = c(0, 0)) +
  labs(
    x = "\n BPM",
    title = "Manutenção das batidas por minuto",
    subtitle = "Evolução do 'ritmo' nas músicas \n"
  ) +
  theme(
    axis.text.x = element_text(size = 6, colour = "white"),
    axis.title.x = element_text(size = 10, colour = "white"),
    axis.text.y = element_text(size = 10, colour = "white"),
    axis.title.y = element_blank(),
    plot.title = element_text(
      hjust = 0,
      size = 18,
      face = "bold",
      colour = "white"),
    plot.subtitle = element_text(
      hjust = 0,
      size = 12,
      colour = "white"),
    plot.title.position = "plot",
    plot.background = element_rect(fill = "#2f3231", color = "transparent"),
    panel.background = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.spacing = unit(.1, "lines"),
    legend.background = element_rect(fill = "#2f3231", color = "transparent"),
    legend.box.background = element_rect(fill = "#2f3231", color = "transparent"),
    strip.background = element_rect(fill = "#3b3f3e"),
    strip.text = element_text(color = "white", size = 8)
  )

# 5. As principais músicas da Fresno

fresno_toptracks |>
  dplyr::mutate(lancamento = lubridate::year(lancamento),
                album = paste0(album, " (", lancamento, ")")) |>
  dplyr::select(musica, popularidade, album) |>
  dplyr::arrange(desc(popularidade)) |>
  dplyr::rename(
    "Música" = musica,
    "Popularidade" = popularidade,
    "Álbum" = album
  )

