# Critérios de seleção — fundos de crédito privado

Pipeline quantitativo para seleção de fundos high grade, com janela comum de 36 meses para todos os pilares.

## Setup inicial

Ao baixar o repositório em uma nova máquina, restaure o ambiente do `renv` e execute:

```r
source("projects/criterios-selecao-fundos-cp/00_setup_projeto.R")
```

Esse script apenas valida pacotes e arquivos de entrada. Ele não executa a análise.

O cadastro operacional é `data/input/cadastro.csv` (separador `;`, UTF-8 e vírgula decimal). As colunas `Fundo` e `Taxa de Administração` alimentam a etapa 1; a taxa é uma fração anual (por exemplo, `0,007` = 0,7% a.a.). As demais colunas ficam disponíveis para consulta, mas não entram no score atual. A planilha `analise_quantitativa_fundos_high_grade.xlsx` não é mais insumo do pipeline.

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
4. score de qualidade e aprovação quantitativa;
5. redundância, comparação com a carteira atual e diagnósticos de clusters;
6. gráficos e planilha final.

Cada script limpa a sessão, importa seus próprios insumos e exporta seus resultados. Portanto, as etapas também podem ser executadas manualmente, desde que os arquivos intermediários das etapas anteriores existam.

Os pacotes são carregados pelo `.Rprofile`; os scripts não repetem chamadas de `library()`.

## Metodologia atual

- todos os pilares, correlações e elegibilidade usam exatamente 36 meses encerrados e comuns a todos os fundos;
- consistência: hit rate mensal em 36 observações, hit rate de 6 meses em 31 janelas móveis e hit rate de 12 meses em 25 janelas móveis; cada janela de 6 ou 12 meses usa excesso composto sobre CDI;
- dentro do pilar de consistência, os pesos são 20% mensal, 20% para 6 meses e 60% para 12 meses;
- o calendário do CDI define os limites de cada mês; o mês mais recente só entra
  depois que houver observação no mês seguinte;
- o histórico completo é preservado para diagnósticos e visualizações;
- métricas são convertidas em notas de 0 a 100 por z-score robusto e função logística;
- pesos: retorno 30%, consistência 25%, risco 20% e custo 25%;
- correlação, redundância e clusters não entram no score de qualidade;
- aprovação com margem exige nota final de pelo menos 58, nenhum red flag
  absoluto e nenhum pilar abaixo de 30;
- a zona cinzenta abrange notas de 52 a abaixo de 58 e também fundos com nota
  superior que apresentem algum pilar abaixo de 30;
- quartis permanecem apenas descritivos;
- não existe limite automático de fundos por cluster nem tamanho fixo de shortlist.

Para comparar candidatos com posições existentes, preencha `data/config/fundos_carteira_atual.csv` usando exatamente os nomes da coluna `nome_plot`.

Para registrar a diligência, preencha `data/config/avaliacao_qualitativa.csv`.
Os status aceitos são: `Aprovado qualitativamente`,
`Aprovado com limite ou condição`, `Pendente de informações` e
`Reprovado qualitativamente`.

Para corrigir correspondências de nomes, preencha
`data/config/de_para_fundos_revisao.csv` com as colunas `nome_xlsx` e
`nome_quantum`. O campo `nome_xlsx` permanece como chave interna legada e deve corresponder exatamente à coluna `Fundo` do CSV. O pipeline lê essas decisões e nunca sobrescreve o arquivo;
as sugestões automáticas são exportadas separadamente em `data/intermediate/`.

## Estrutura

- `data/input/`: `cadastro.csv` e históricos atualizados de cotas e benchmarks necessários para reproduzir a análise;
- `data/config/`: decisões manuais versionadas, especialmente o de-para;
- `data/intermediate/`: CSVs e RDS recriados pelo pipeline e ignorados pelo Git;
- `output/figures/`: gráficos finais versionados;
- `output/reports/`: planilhas finais versionadas;
- `legacy/`: versões anteriores preservadas para consulta.

A saída canônica é `output/reports/analise_high_grade.xlsx`. A aba `Ranking` apresenta posição, score e quartil, seguida das notas dos pilares, métricas objetivas e contribuição de cada uma das nove métricas em pontos do score final (pp): nota da métrica × peso dentro do pilar × peso do pilar. As nove contribuições somam o score; os totais por pilar são subtotais e não devem ser somados novamente. A exportação valida essa reconciliação.

A antiga aba Ranking passa a se chamar `Diligência e Redundância`, preservando os alertas, a fila qualitativa e os diagnósticos de correlação sem repetir métricas e notas. As quatro abas `Pilar` (retorno, consistência, risco e custo) permanecem com valor, cálculo, z-score, nota, peso, contribuição, nota e quartil do pilar. `Todos os Fundos` acrescenta o CNPJ como texto de 14 dígitos, lido do `cadastro.csv`. Os 53 identificadores foram migrados por correspondência exata de nomes da planilha cadastral legada, com validação dos dígitos verificadores; o XLSX legado continua fora dos insumos do pipeline. O `Dicionário` define os campos; a abertura é auditável e não altera o score.

Os scripts canônicos mantêm 36 meses comuns para todos os pilares e correlações. Histórico mais longo para testes de robustez não altera o score canônico. A abordagem paralela de `selecao_credito.R` permanece em `legacy/` e não entra silenciosamente no score atual.
