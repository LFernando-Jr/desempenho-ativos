# Contexto do usuário

Fernando trabalha com alocação e análise de investimentos em um Multi-Family Office.
Neste projeto, atua como responsável pela metodologia de seleção e acompanhamento
de fundos de crédito privado high grade.

## Forma de trabalhar

- R é a linguagem principal para análise e automação.
- Positron é o IDE principal.
- Git/GitHub são usados para versionamento.
- Excel e PowerPoint são outputs relevantes quando necessário.
- Notion é usado principalmente como planner, índice de materiais, checklist
  operacional e consolidador de processos.

## Preferências de código

- Priorizar código legível, modular e reproduzível.
- Evitar `pacote::função`; conflitos de funções são tratados pelo `.Rprofile`.
- Usar `%>%` como pipe.
- Usar `=` para atribuição quando apropriado.
- Evitar mudanças desnecessárias fora do escopo solicitado.
- Antes de alterar arquitetura, estrutura de pastas ou metodologia, explicar
  o problema identificado e a mudança proposta.
- Dados intermediários/reproduzíveis não precisam necessariamente ser versionados.
- Outputs finais relevantes, gráficos e planilhas finais podem permanecer no Git.

## Como abordar análises

- Não assumir que a metodologia existente está correta.
- Separar claramente:
  1. fatos observados;
  2. hipóteses;
  3. escolhas metodológicas;
  4. julgamento qualitativo.
- Procurar inconsistências de lógica antes de sugerir melhorias adicionais.
- Não otimizar métricas apenas para melhorar ranking histórico.
- Evitar falsa precisão.
- Preferir critérios economicamente interpretáveis.
- Ao identificar um problema, avaliar o impacto antes de propor refatoração.

## Projeto

Objetivo: desenvolver uma metodologia robusta e reproduzível para seleção e
acompanhamento de fundos de crédito privado high grade.

O projeto combina:
- análise quantitativa de desempenho;
- risco e consistência;
- características da carteira;
- custos e liquidez;
- análise qualitativa;
- red flags;
- construção de ranking/shortlist.

O ranking é uma ferramenta de apoio à decisão, não deve substituir julgamento
de investimento.

## Regra importante

Antes de editar arquivos:
1. entender a estrutura atual;
2. identificar o problema;
3. verificar dependências;
4. propor a mudança;
5. só então implementar, salvo quando a alteração for trivial e explicitamente solicitada.

## Notion — fonte de contexto do projeto

A página canônica do projeto no Notion é:

**Critérios de Seleção — Fundos de Crédito Privado**
https://app.notion.com/p/Projeto-Sele-o-Quantitativa-de-Fundos-High-Grade-3ada309842d0815191f0cc5c4e59539b?v=36693ee40b7842b7b82faa19da3f86e2&source=copy_link

Use essa página como fonte de contexto institucional do projeto:
- metodologia;
- decisões já tomadas;
- premissas;
- dúvidas em aberto;
- documentação e referências.

Antes de rediscutir uma decisão metodológica, consulte essa página.

Não altere conteúdo no Notion sem solicitação explícita do usuário.