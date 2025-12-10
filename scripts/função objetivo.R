
# Pacotes -----------------------------------------------------------------

library(stats)

# Premissas ---------------------------------------------------------------

rendimento_carteira_ate_agora = 0.3371  # 33,71%
rendimento_cdi_ate_agora      = 0.3570  # 35,70%
cdi_futuro                    = 0.1002  # DI para os próximos 180 dias (10,02% ao ano)
tempo_passado_anos            = as.numeric((Sys.Date() - as.Date("2021-01-04"))/360)  # Aproximadamente 3,21 anos desde o início
tempo_futuro_anos             = 180 / 360  # Aproximadamente 6 meses


# Função objetivo ---------------------------------------------------------

# Função para calcular a diferença entre o objetivo (superar o CDI acumulado total) e o resultado com a rentabilidade R
diferenca_rentabilidade = function(R) {
  valor_final_com_R     = (1 + rendimento_carteira_ate_agora) * (1 + R)^tempo_futuro_anos
  valor_final_cdi_total = (1 + rendimento_cdi_ate_agora) * (1 + cdi_futuro)^tempo_futuro_anos
  return(valor_final_com_R - valor_final_cdi_total)
}

# Encontrar o zero da função ----------------------------------------------

# Encontrar a rentabilidade R que faz a função diferenca_rentabilidade ser zero
rentabilidade_necessaria = uniroot(diferenca_rentabilidade, c(-1, 1), tol = 1e-10)$root  # Ajustando o intervalo e a tolerância

# Resultados --------------------------------------------------------------

rentabilidade_necessaria

R_mensal   = 100*((1 + rentabilidade_necessaria)^(1/12) - 1) # rentabilidade mensal desejada 

CDI_mensal = 100*((1 + cdi_futuro)^(1/12) - 1) # rentabilidade mensal do cdi  

rentabilidade_necessaria/cdi_futuro * 100 # rentabilidade desejada como percentual do cdi

100*((1 + rentabilidade_necessaria)/(1 + cdi_futuro) - 1 ) # rentabilidade desejada como cdi+
