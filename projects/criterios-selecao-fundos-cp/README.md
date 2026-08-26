# Critérios de seleção — fundos de crédito privado

Pipeline quantitativo para seleção de fundos high grade, com janela estrutural de 36 meses.

## Setup inicial

Ao baixar o repositório em uma nova máquina, restaure o ambiente do `renv` e execute:

```r
source("projects/criterios-selecao-fundos-cp/00_setup_projeto.R")
```

Esse script apenas valida pacotes e arquivos de entrada. Ele não executa a análise.

## Como executar

Premissa operacional: abra a pasta raiz `desempenho-ativos` no Positron antes de rodar este pipeline. O diretório de trabalho da sessão R deve ser a raiz `desempenho-ativos`, não a pasta `projects/criterios-selecao-fundos-cp`.

A partir da raiz `desempenho-ativos`, rode:

```r
source("projects/criterios-selecao-fundos-cp/00_run_all.R")
```

O runner cria os diretórios necessários e executa seis etapas independentes:

1. importação dos históricos e validação do de-para;
2. retornos, histórico completo e elegibilidade na janela comum de 36 meses;
3. métricas individuais e correlações;
4. score de qualidade e preparação da aprovação quantitativa;
5. redundância, comparação com a carteira atual e diagnósticos de clusters;
6. gráficos e planilha final.

Cada script limpa a sessão, importa seus próprios insumos e exporta seus resultados. Portanto, as etapas também podem ser executadas manualmente, desde que os arquivos intermediários das etapas anteriores existam.

Os pacotes são carregados pelo `.Rprofile`; os scripts não repetem chamadas de `library()`.

## Metodologia atual

- o score usa exatamente 36 meses completos e comuns a todos os fundos;
- o histórico completo é preservado para diagnósticos e visualizações;
- métricas são convertidas em notas de 0 a 100 por z-score robusto e função logística;
- pesos: retorno 30%, consistência 25%, risco 20% e custo 25%;
- correlação, redundância e clusters não entram no score de qualidade;
- quartis são descritivos e a aprovação quantitativa ainda será calibrada;
- não existe limite automático de fundos por cluster nem tamanho fixo de shortlist.

Para comparar candidatos com posições existentes, preencha `data/config/fundos_carteira_atual.csv` usando exatamente os nomes da coluna `nome_plot`.

## Estrutura

- `data/input/`: exportações necessárias para reproduzir a análise;
- `data/config/`: decisões manuais versionadas, especialmente o de-para;
- `data/intermediate/`: CSVs e RDS recriados pelo pipeline e ignorados pelo Git;
- `output/figures/`: gráficos finais versionados;
- `output/reports/`: planilhas finais versionadas;
- `legacy/`: versões anteriores preservadas para consulta.

Os scripts canônicos mantêm a metodologia da versão de 36 meses. A abordagem paralela de `selecao_credito.R` permanece em `legacy/` e não entra silenciosamente no score atual.
