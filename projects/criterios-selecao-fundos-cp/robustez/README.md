# Diagnóstico de históricos mais longos

Esta branch compara hit rates calculados em 48, 60 e 72 meses, cada horizonte numa janela comum apenas aos fundos com histórico completo naquele prazo. A versão oficial continua a usar os mesmos 36 meses para todos os 47 fundos e pesos de consistência 20/20/60.

Execute `scripts/07_robustez_consistencia.R` depois das etapas 2 a 5. O script grava `comparacao_consistencia_janelas_longas.csv` e `resumo_consistencia_janelas_longas.csv` nesta pasta. Ele não modifica o ranking ou a planilha canônicos.

Para isolar a extensão da janela, as notas dos hit rates longos usam a mediana, MAD e fallback da calibração oficial de 36 meses e 47 fundos. O score de teste troca apenas a contribuição de consistência. Ranks oficial e de teste são comparados **dentro do mesmo subconjunto de cada horizonte**, nunca entre universos diferentes.

Uma divergência relevante exige entender qual fundo ou período a causou; não é regra automática para substituir a metodologia de 36 meses. Janelas móveis sobrepostas não representam observações independentes.
