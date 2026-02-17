# 🌿 ZHOUSE  - DarwinCore 

[![R](https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white)](https://www.r-project.org/)
[![Darwin Core](https://img.shields.io/badge/Standard-Darwin_Core-75B375?style=for-the-badge&logoColor=black)](#)
[![Licença](https://img.shields.io/badge/License-MIT-yellow?style=for-the-badge)](https://opensource.org/licenses/MIT)
[![OpenAI Codex](https://img.shields.io/badge/AI_Assisted_by-Codex-00A67E?style=for-the-badge&logo=openai&logoColor=white)](#)
[![MiniMax](https://img.shields.io/badge/Optimized_by-MiniMax-E73562?style=for-the-badge&logo=minimax&logoColor=white)](#)


Script R para extração, validação e padronização de dados taxonômicos para o formato Darwin Core (DwC).

## 📝 Descrição

Este script processa uma planilha de dados de biodiversidade e gera um output padronizado conforme o formato Darwin Core, amplamente utilizado em repositórios de dados biológicos. O processo inclui validação de nomes científicos utilizando bases de dados taxonômicas brasileiras (florabr, faunabr) e globais (taxadb/GBIF).

## ✨ Funcionalidades

- **Leitura de dados**: Processa planilhas Excel com registros de espécies
- **Validação taxonômica**: Utiliza múltiplas bases de dados para validar nomes científicos
  - 🌺 florabr: Flora brasileira validada
  - 🐆 faunabr: Fauna brasileira validada
  - 🌍 taxadb/GBIF: Base de dados global
- **🧬 Enriquecimento de dados**: Adiciona informações taxonômicas completas
  - Reino, filo, classe, ordem, família, gênero
  - Epíteto específico e infraspecífico
  - Autoridade científica
  - Status taxonômico
- **🛡️ Classificação de ameaça**: Integra dados de conservação
  - MMA Portaria 148/2022 (Lista Espécies Ameaçadas Brasil)
  - BASE_ZHOUSE.xlsx (status local)
  - IUCN Red List (opcional, via API)
- **Geração de output**: Cria arquivos Excel no padrão Darwin Core
- **Relatório de auditoria**: Gera planilha com todas as decisões de validação

## 📂 Estrutura de Arquivos Esperados

```
project/
├── zhouse_dwc_2026-02-16.R    # Script principal
├── BASE_ZHOUSE.xlsx           # Dados de entrada (obrigatório)
├── Template_lista_especies.xlsx # Template de saída (opcional)
├── docs/
│   └── criterio_species_brasil.md # Critérios MMA (opcional)
├── data/
│   └── florabr/
│       └── 393.422/
│           └── CompleteBrazilianFlora.rds # Flora local (opcional)
└── outputs/
    ├── dwc_zhouse.xlsx              # Output principal
    └── dwc_zhouse_auditoria.xlsx    # Relatório de auditoria
```

## 🛠️ Requisitos

### 📦 Pacotes R Necessários

```r
install.packages(c(
  "readxl",      # Leitura de Excel
  "dplyr",       # Manipulação de dados
  "stringr",     # Manipulação de strings
  "stringi",     # Operações de texto
  "tidyr",       # Transformação de dados
  "purrr",       # Programação funcional
  "tibble",      # Data frames alternativos
  "writexl",     # Escrita de Excel
  "florabr",     # Flora brasileira
  "faunabr",     # Fauna brasileira
  "taxadb",      # Banco taxonômico
  "rredlist"    # IUCN Red List (opcional)
))
```

### 📥 Dados de Entrada

O arquivo `BASE_ZHOUSE.xlsx` deve conter as seguintes colunas obrigatórias:

| Coluna | Descrição |
|--------|------------|
| Operacao | Localidade/Projeto da ocorrência |
| Projeto | Nome do projeto associado |
| Grupo-alômico (Floravo | Grupo taxon/Fauna/Fungos) |
| Nome cientifico | Nome científico da espécie |
| Nome popular | Nome vernacular |
| Status conservacao nacional | Status de ameaça |
| Nativa BR | Se a espécie é nativa do Brasil (Sim/Não) |
| Endemica BR | Se a espécie é endêmica do Brasil (Sim/Não) |

## 🛠️ Configuração

### 🗂️ Caminhos de Arquivos

Os caminhos podem ser modificados no início do script:

```r
input_path <- "BASE_ZHOUSE.xlsx"
template_path <- "Template_lista_especies.xlsx"
criteria_path <- "docs/criterio_species_brasil.md"
output_dir <- "outputs"
```

### 🔑 API IUCN (Opcional)

Para utilizar a consulta à IUCN Red List, configure a chave API:

```r
iucn_key <- "SUA_CHAVE_API"  # Obtain from https://apiv3.iucnredlist.org/api/v3/token
```

## 🚀 Uso

1. Configure os caminhos dos arquivos de entrada
2. Execute o script:

```r
source("zhouse_dwc_2026-02-16.R")
```

3. Verifique os outputs gerados na pasta `outputs/`

## 📤 Output

### 📗 dwc_zhouse.xlsx

Arquivo principal no formato Darwin Core com as seguintes colunas:

- **datasetName**: Nome do dataset
- **institutionCode**: Código da instituição (ZHOUSE)
- **taxonID**: Identificador único do táxon
- **scientificName**: Nome científico completo
- **taxonRank**: Rank taxonômico (species, subspecies, variety, genus, etc.)
- **scientificNameAuthorship**: Autoridade do nome
- **kingdom**: Reino (Animalia, Plantae, Fungi)
- **phylum**: Filo
- **class**: Classe
- **order**: Ordem
- **family**: Família
- **genus**: Gênero
- **specificEpithet**: Epíteto específico
- **infraspecificEpithet**: Epíteto infraspecífico
- **vernacularName**: Nome popular
- **establishmentMeans**: Origem (native, introduced, cultivated, etc.)
- **taxonomicStatus**: Status taxonômico (accepted, synonym)
- **status**: Status de conservação
- **statusSource**: Fonte do status de conservação
- **criteria**: Critérios IUCN (se disponível)
- **locality**: Localidade da ocorrência
- **stateProvince**: Estado brasileiro
- **license**: Licença (CC-BY-NC)
- **rightsHolder**: Detentor dos direitos

### 🕵️ dwc_zwater_auditoria.xlsx

Planilha com múltiplas abas:

- **auditoria**: Log completo de todas as validações
  - originalName: Nome original
  - queryName: Nome utilizado na consulta
  - groupType: Grupo taxonômico
  - validator: Base de dados utilizada
  - matchType: Tipo de correspondência (exact, corrected, not_found)
  - finalScientificName: Nome científico final
  - decisionReason: Motivo da decisão
  - Informações taxonômicas completas

- **nao_resolvidos**: Registros não resolvidos ou com problemas
  - Espécies não encontradas nas bases de dados
  - Nomes com marcadores de incerteza (cf., aff.)
  - Nomes inválidos ou sinônimos

## 🧠 Detalhes do Processamento

### 🔄 Pipeline de Validação

1. **Pré-processamento**: Limpeza de nomes científicos
   - Remoção de caracteres especiais
   - Normalização de espaços
   - Detecção de marcadores de incerteza (cf., aff., sp., spp.)

2. **Validação Flora**: Consulta ao florabr
   - Verificação ortográfica
   - Retorno de nomes aceitos
   - Extração de informações taxonômicas

3. **Validação Fauna**: Consulta ao faunabr e taxadb
   - Verificação ortográfica
   - Resolução de sinônimos
   - Extração de informações taxonômicas

4. **Decisão Final**: Priorização de resultados
   - Nomes aceitos têm prioridade sobre sinônimos
   - Correspondências exatas são preferidas
   - Marcadores de incerteza preservados quando necessário

### ⚖️ Lógica de Status de Conservação

1. MMA Portaria 148/2022 (prioridade mais alta)
2. BASE_ZHOUSE.xlsx (dados locais)
3. IUCN Red List (via API, se configurado)

## 🐛 Troubleshooting

### Erro: "Missing columns"

Verifique se o arquivo de entrada contém todas as colunas obrigatórias listadas acima.

### Erro: "object 'genus' not found"

Este erro foi corrigido na versão 2.0. Certifique-se de estar usando a versão mais recente.

### Dados não encontrados nas bases

Algumas espécies podem não estar presentes nas bases de dados utilizadas. Estes registros aparecem na aba "nao_resolvidos" do relatório de auditoria.

## 🤝 Contribuição

Para contribuir com o desenvolvimento:

1. Fork o repositório
2. Crie uma branch para sua feature
3. Commit suas alterações
4. Push para a branch
5. Abra um Pull Request

## 📜 Licença

CC-BY-NC (Attribution-NonCommercial)

## ✍️ Autor

Rogerio Nunes Oliveira

## Versão

2.0 - 2026-02-16

## 🔖 Notas Técnicas

### 📝 Funções Auxiliares Principais

- `clean_scientific_name()`: Limpeza de nomes científicos
- `canonical_name()`: Extração do nome canônico
- `parse_rank_from_name()`: Detecção do rank taxonômico
- `normalize_group()`: Classificação em flora/fauna/fungos
- `ensure_cols()`: Garante colunas obrigatórias

### ⚠️ Tratamento de Nomes Problemáticos

- Nomes com cf., aff., gr.: Marcadores preservados
- sp., spp., sp. nov.: Identificados como placeholders
- Híbridos: Não tratados nativamente (versão atual)
- Autores com parênteses: Preservados na autoridade
