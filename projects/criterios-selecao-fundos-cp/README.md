# Critérios de seleção — fundos de crédito privado

Pipeline quantitativo para seleção de fundos high grade, com janela estrutural de 36 meses.

## Como executar

Abra o projeto `desempenho-ativos` para carregar o ambiente do `.Rprofile` e, a partir desta pasta, rode:

```r
source("00_run_all.R")
```

O runner cria os diretórios necessários e executa, no mesmo ambiente, as cinco etapas:

1. importação dos históricos e validação do de-para;
2. retornos, excessos e elegibilidade de 36 meses;
3. métricas, correlações e clusters;
4. score, quartis e shortlist;
5. gráficos e planilha final.

## Estrutura

- `data/input/`: exportações necessárias para reproduzir a análise;
- `data/config/`: decisões manuais versionadas, especialmente o de-para;
- `data/intermediate/`: CSVs e RDS recriados pelo pipeline e ignorados pelo Git;
- `output/figures/`: gráficos finais versionados;
- `output/reports/`: planilhas finais versionadas;
- `legacy/`: versões anteriores preservadas para consulta.

Os scripts canônicos mantêm a metodologia da versão de 36 meses. A abordagem paralela de `selecao_credito.R` permanece em `legacy/` e não entra silenciosamente no score atual.
