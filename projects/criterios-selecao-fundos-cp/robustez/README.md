# Diagnóstico de históricos mais longos

Esta branch compara hit rates calculados em 48, 60 e 72 meses, cada horizonte numa janela comum apenas aos fundos com histórico completo naquele prazo. A versão oficial continua a usar os mesmos 36 meses para todos os 47 fundos e pesos de consistência 20/20/60.

Execute `scripts/07_robustez_consistencia.R` depois das etapas 2 a 5. O script grava `comparacao_consistencia_janelas_longas.csv` e `resumo_consistencia_janelas_longas.csv` nesta pasta. Ele não modifica o ranking ou a planilha canônicos.

Para isolar a extensão da janela, as notas dos hit rates longos usam a mediana, MAD e fallback da calibração oficial de 36 meses e 47 fundos. O score de teste troca apenas a contribuição de consistência. Ranks oficial e de teste são comparados **dentro do mesmo subconjunto de cada horizonte**, nunca entre universos diferentes.

## Resultado da rodada encerrada em agosto de 2026

| Horizonte testado | Fundos com histórico completo | Excluídos dos 47 | Maior mudança de posição no subconjunto | Mudanças de aprovação | Mudanças de fronteira |
| --- | ---: | ---: | ---: | ---: | ---: |
| 48 meses | 39 | 8 | 5 | 0 | 1 |
| 60 meses | 36 | 11 | 5 | 0 | 1 |
| 72 meses | 34 | 13 | 5 | 3 | 5 |

Em 72 meses, Sparta Max Advisory, JGP Corporate FEEDER II e Sparta Top passam da zona cinzenta para aprovado com margem no cenário de teste. Essa é uma **sensibilidade**, não uma nova aprovação oficial. Os horizontes de 48 e 60 meses não alteram a aprovação quantitativa de nenhum fundo elegível ao respectivo teste; há uma mudança de fronteira em cada um.

**Decisão:** manter 36 meses completos e comuns como metodologia oficial. Exigir 72 meses retiraria 13 fundos da comparação e misturaria efeito da extensão temporal com a seleção de um universo menor. O resultado mais longo fica documentado como diagnóstico; não altera o score, a régua nem a fila de diligência. Não será aberta investigação adicional dos três casos apenas por este teste.

Janelas móveis sobrepostas não representam observações independentes. A comparação de posições é sempre feita dentro do mesmo subconjunto em cada horizonte, e não entre universos de tamanhos diferentes.
