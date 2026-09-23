# Fichas visuais dos fundos

Uma ficha PNG por fundo cadastrado. O arquivo `indice.csv` relaciona nome, posição no ranking (quando houver), status quantitativo, meses completos e nome da ficha.

Cada ficha contém: (1) excesso acumulado sobre o CDI; (2) drawdown desse excesso; (3) excesso anualizado em janelas móveis de 36 meses completos e consecutivos do histórico; e (4) histograma do excesso diário, com densidade estimada. A curva móvel, a assimetria e o excesso de curtose são diagnósticos: não entram no score ou na régua de aprovação. Fundos sem 36 meses consecutivos exibem essa indisponibilidade no painel.

Os 47 fundos com score usam a mesma janela completa de 36 meses. Os seis fundos sem score têm apenas os meses completos disponíveis dentro dessa janela; lacunas ficam visíveis nas linhas. Por isso, suas trajetórias não devem ser comparadas diretamente às dos fundos com score. A densidade é uma estimativa suavizada dos retornos observados, não uma previsão de probabilidades futuras.

As fichas são regeneradas pela Etapa 6 (`scripts/06_exportacao.R`) após a atualização das bases. A planilha final contém a aba `Dicionário`, com definição, unidade, período e papel das colunas de cada aba.
