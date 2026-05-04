# 🎸 O que aconteceu com a banda Fresno? (projFresnoR)

[![R](https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white)](https://www.r-project.org/)
[![Quarto](https://img.shields.io/badge/Quarto-447099?style=for-the-badge&logo=quarto&logoColor=white)](https://quarto.org/)

**Trabalho de Conclusão do curso de Relatórios e Apresentações (curso-R)** desenvolvido em R e Quarto para análise da discografia da banda Fresno.

O projeto busca responder questões centrais sobre a evolução da banda: transformando dados musicais do Spotify em *insights* sobre a estruturação dos álbuns, mudanças na composição, preferências do público e a estética sonora ao longo do tempo.

### Funcionalidades e Itens Técnicos

* **Extração de Dados (ETL):** Consumo automatizado de metadados e características de áudio da discografia da banda via API do Spotify, utilizando o pacote `spotifyR`.
* **Análise Estrutural e Estética:** Avaliação de como a banda organizou seus trabalhos ao longo do tempo e identificação de mudanças na construção e na estética das músicas.
* **Mapeamento de Preferências:** Análise focada em identificar possíveis pontos de preferência do público considerando a obra completa da banda.
* **Reporting e Reprodutibilidade:** Toda a análise de dados, estatísticas e visualizações foram construídas de forma integrada e reprodutível utilizando o ecossistema **Quarto** no R.
* **Gestão de Dados:** Organização de diretórios estruturada, com os dados consumidos salvos e centralizados no diretório `dados/` para facilitar a replicação do estudo.

### Tecnologias utilizadas

* **Linguagem:** R
* **Relatórios e Apresentações:** Quarto
* **Extração de Dados (API):** `spotifyR`
