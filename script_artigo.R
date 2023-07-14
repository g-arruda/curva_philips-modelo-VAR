library(tidyverse)
library(patchwork) # coloca um grafico abaixo do outro
library(vars)
library(urca) #Teste de raiz
library(ggfortify) #autoplot
library(stargazer) #tabela latex
options(scipen=999) ### Remove a notação cientifica



##### Importando os dados-----
desemprego <- read.csv("dados/desemprego.csv",
                       sep = ",", dec = ",",
                       col.names = c("data", "tx_desemprego"))[,-1] %>%
  ts(start = c(2012,03), frequency = 12) %>%
  window(start = c(2012, 03), end = c(2022, 05))


inf <- read.csv("dados/ipca.csv", sep = ",")[,-1] %>%
  round(digits = 3) %>%
  ts(start = c(2000,01), frequency = 12) %>%
  window(start = c(2012, 03), end = c(2022, 05))

exp_inf <- readxl::read_xlsx("dados/exp_inf.xlsx", sheet = 2)[,-1] %>%
  ts(start = c(2002,04), frequency = 12) %>%
  window(start = c(2012, 03), end = c(2022, 05))


#### trabalhando o Cambio

cambio_raw <- read.csv("dados/cambio.csv", sep = ",")


cambio <- cambio_raw %>%
  mutate(media = rowMeans(cambio_raw[,c("compra", "venda")]),
         retorno = c(NA, diff(log(rowMeans(cambio_raw[,c("compra", "venda")]))))) %>%
  dplyr::select(-Date) %>%
  ts(start = c(2000,04), frequency = 12) %>%
  window(start = c(2012, 03), end = c(2022, 05))



# Juntando

dados_raw <- ts.intersect(cambio[,"retorno"], desemprego, exp_inf, inf)
colnames(dados_raw) <- c("Repasse Cambial", "Desemprego", "exp.inflação", "Inflação")

sd(dados_raw[,1])

#Tabela com as estatistica descritiva dos dados
xtable::xtable(psych::describe(dados_raw))




###### Visualizando -----

#Var.Cambial
autoplot(cambio[,4]) + theme_bw() + theme(axis.text = element_text(size = 13,
                                                                   colour = "black"))
#ggsave(filename = "repasse-cambial.png")



autoplot(desemprego) + theme_bw() +
  theme(axis.text = element_text(size = 13, colour = "black"))
#ggsave(filename = "desemprego.png")



autoplot(inf) + theme_bw() +
  theme(axis.text = element_text(size = 13, colour = "black"))
#ggsave(filename = "IPCA.png")


autoplot(exp_inf) + theme_bw() + 
  theme(axis.text = element_text(size = 13, colour = "black"))
#ggsave(filename = "exp-inf.png")


##### TESTANDO RAIZ UNITARIA -------------------



df_desemprego_1 <- ur.df(dados_raw[,"Desemprego"], type = "drift",
                       selectlags = "BIC")
summary(df_desemprego_1)  ### valor teste > tau3 -- nao-estacionaria

df_desemprego_2 <- ur.df(dados_raw[,"Desemprego"], type = "trend",
                       selectlags = "BIC")
summary(df_desemprego_2)



df_desemprego_l1 <- ur.df(diff(dados_raw[,"Desemprego"]), type = "trend",
                          selectlags = "BIC")
summary(df_desemprego_l1) ### I(1), estacionaria após 1 dff -- teste < tau2


df_cambio <- ur.df(dados_raw[,"Repasse Cambial"], type = "trend",
                   selectlags = "BIC") 
summary(df_cambio) ### Estacionario

df_ipca <- ur.df(dados_raw[,"Inflação"], type = "trend",
                 selectlags = "BIC")
summary(df_ipca) ### Estacionária


df_exp_inf <- ur.df(dados_raw[,"exp.inflação"], type = "trend",
                    selectlags = "BIC")
summary(df_exp_inf) #### é rejeitada ao nivel de 5%



# notamos que apenas o desemprego é nao estacionario e por
# isso vamos usar a variavel com uma defasagem



##### FUNÇÕES DO MODELO E TESTES --------------------

###### Funções q cria modelo ---------
criar_var <- function(dados, p=1, type="both"){
  modelos <- list()
  
  for(i in 1:p){
    modelos[[i]] <- VAR(dados, p=i,season = 12, type = type)
  }
  
  nomes <- character(length = p)
  for(i in 1:p){
    nomes[i] <- paste("VAR.",i, sep = "")
  }
  
  names(modelos) <- nomes
  return(modelos)
}


######## Função teste de correlação serial dos resíduos ------
teste_serial <- function(modelo, lag = 16, type){
  teste <- list()
  for(i in 1:length(modelo)){
    teste[[i]] <- serial.test(modelo[[i]], lags.pt = lag, type = type)
  }
  
  nomes <- character(length = length(modelo))
  for(i in 1:length(modelo)){
    nomes[i] <- paste("VAR.",i, sep = "")
  }
  
  names(teste) <- nomes
  return(teste)
}


####### Função de Normalidade dos resíduos -------
teste_normalidade <- function(modelo){
  teste <- list()
  for(i in 1:length(modelo)){
    teste[[i]] <- normality.test(modelo[[i]])$jb.mul$JB
  }
  
  nomes <- character(length = length(modelo))
  for(i in 1:length(modelo)){
    nomes[i] <- paste("VAR.",i, sep = "")
  }
  
  names(teste) <- nomes
  return(teste)
}


########## Função de Hetero ---------------
teste_hetero <- function(modelo, lag = 5){
  teste <- list()
  for(i in 1:length(modelo)){
    teste[[i]] <- arch.test(modelo[[i]], lags.multi = lag)$arch.mul
  }
  
  nomes <- character(length = length(modelo))
  for(i in 1:length(modelo)){
    nomes[i] <- paste("VAR.",i, sep = "")
  }
  
  names(teste) <- nomes
  return(teste)
}



### MODELO VAR COM DESEMPREGO DIFERENCIADO ----
###### Diferenciando o desemprego ##########

desemprego_diff <- diff(dados_raw[,c("Desemprego")])
dados_desemp_diff <- ts.intersect(dados_raw[,c("Repasse Cambial", "Inflação", "exp.inflação")], desemprego_diff)
colnames(dados_desemp_diff) <- c("Repasse Cambial", "Inflação", "exp.inflação", "Desemprego")
dados_desemp_diff <- dados_desemp_diff[,c("Repasse Cambial", "Desemprego", "exp.inflação", "Inflação")]


head(dados_desemp_diff)
ggplot2::autoplot(dados_desemp_diff, facets = T, ylab = "") +theme_bw()


###### Modelo  ------
VARselect(dados_desemp_diff,type = "const")$selection


var_desemp_diff <- criar_var(dados_desemp_diff, p = 8, type = "both")
resumo<- summary(var_desemp_diff$VAR.2)
coef(var_desemp_diff$VAR.2)



#stargazer(var_desemp_diff$VAR.2$varresult)


#stability(var_desemp_diff$VAR.2) %>% plot()
roots(var_desemp_diff$VAR.2)




###### serial -----
serial_desemp_diff <- teste_serial(var_desemp_diff, type = "PT.asymptotic")
serial_desemp_diff #VAR(3), VAR(4), VAR (5), VAR(6) não apresenta correlação nos resíduos




###### Normalidade dos resíduos ----
normalidade_desemp_diff <- teste_normalidade(var_desemp_diff)
normalidade_desemp_diff
# nenhum modelo até p=8 apresenta normalidade nos resíduos



###### teste de Hetero ----
hetero_desemp_diff <- teste_hetero(var_desemp_diff)
hetero_desemp_diff$VAR.2$p.value
# VAR(5), VAR(6), VAR(7), VAR(8) nao apresenta Hetero



####### IMPULSO RESPOSTA -----
### aqui vamos usar o VAR (2)

var_desemp_diff_irf <- VAR(dados_desemp_diff, p=2,
                           type = "both", season = 12)


irf_diff_cambio <- irf(var_desemp_diff_irf,
                       impulse = "Repasse.Cambial",
                       ortho = T, boot = T, n.ahead = 20)

irf_diff_inf <-irf(var_desemp_diff_irf,
                   impulse = "Inflação",
                   ortho = T, boot = T, n.ahead = 20)

irf_diff_desemprego <- irf(var_desemp_diff_irf,
                           impulse = "Desemprego",
                           ortho = T, boot = T, n.ahead = 20)

irf_diff_exp.infl <- irf(var_desemp_diff_irf,
                         impulse = "exp.inflação",
                         ortho = T, boot = T, n.ahead = 20)


### FUNÇÃO QUE EXTRAI OS DADOS DE IRF() PARA PLOTAR NO GGPLOT
tsp.var.irf <- function(irf){
  
  if (class(irf) %in% "varirf") {
  } else{
    stop("Only 'varirf' class object from vars::irf()")
  }
  
  
  fortify <- function(data){
    result <- vector(mode = "list",length = 0L)
    for (d in 1:length(data)) {
      result[[length(result)+1]]<- tibble::tibble(imp = names(data)[d],
                                                  lag = 0:{nrow(data[[d]])-1},
                                                  tibble::as_tibble(data[[d]]))
    }
    data <- tidyr::unnest(tibble::tibble(result),cols = result)
    return(data)}
  
  data_irf <- fortify(irf$irf)
  data_lower <- fortify(irf$Lower)
  data_upper <- fortify(irf$Upper)
  suppressMessages(
    plot_data <- tibble::add_column(data_irf,type = "mean") %>%
      dplyr::full_join(tibble::add_column(data_lower,type = "lower")) %>%
      dplyr::full_join(tibble::add_column(data_upper,type = "upper")) %>%
      tidyr::pivot_longer(cols = -c(imp,lag,type)) %>%
      dplyr::mutate(imp = paste(imp),
                    name = paste(name)))
  
  plot <- ggplot2::ggplot(plot_data) +
    ggplot2::geom_line(ggplot2::aes(x = lag,value,
                                    lty = type),show.legend = F) +
    ggplot2::facet_grid(cols = ggplot2::vars(imp),
                        rows = ggplot2::vars(name),
                        scales = "free") +
    ggplot2::scale_linetype_manual(values = c("lower"=2,
                                              "upper"=2,
                                              "mean"=1)) +
    ggplot2::geom_hline(yintercept = 0,lty = 1) +
    ggplot2::scale_x_continuous(labels = as.integer) +
    ggplot2::labs(x="", y="") +
    ggplot2::theme_bw() +
    #ggplot2::theme(panel.spacing = unit(1, "lines")) +
    ggplot2::theme(strip.text.y = element_text(size = 6.5))+
    ggplot2::scale_color_manual(values = c(lower="red",
                                           upper="red",
                                           mean="black"))
  return(plot)
}



tsp.var.irf(irf_diff_cambio)
#ggsave(filename = "irf_cambio.png", width = 10, height = 11, units = "cm")


tsp.var.irf(irf_diff_desemprego)
#ggsave(filename = "irf_desemp.png", width = 10, height = 11, units = "cm")


tsp.var.irf(irf_diff_exp.infl)
#ggsave(filename = "irf_exp.infl.png", width = 10, height = 11, units = "cm")


tsp.var.irf(irf_diff_inf)
#ggsave(filename = "irf_infl.png", width = 10, height = 11, units = "cm")



####### Decomposição da variancia dos erros -----

fevd <- fevd(var_desemp_diff_irf, n.ahead = 10)

## Função que serpara pega os valores da fevd() e trabalha os dados para usar o ggplot
fevd_ggplot <- function(var_model, n=6){
  fevd <- fevd(var_model, n.ahead = n)
  
  lista <- list()
  
  for (i in 1:length(fevd)) {
     lista[[i]] <- fevd[[i]] %>%
       as.tibble() %>%
       mutate(periodo = seq(1,n),
              id = names(fevd[i])) %>%
       pivot_longer(names_to = "var",
                    cols = c("Repasse.Cambial",
                             "Desemprego",
                             "exp.inflação",
                             "Inflação"))
     names(lista)[i] <- names(fevd[i])
  }
  
  return(lista)
}

list_fevd <- fevd_ggplot(var_desemp_diff_irf,n=10)


p1 <- ggplot(list_fevd[[1]], aes(x=factor(periodo), y=value,fill=var)) +
  geom_bar(stat = "identity") +
  labs(x="lag", y="Percentual", title = "Repasse Cambial") +
  scale_y_continuous(expand = c(0,0))+
  scale_fill_grey(name="")+
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 13, colour = "black"),
    axis.title = element_text(size = 13, colour = "black"),
    axis.line.x.bottom = element_line(color = 'black'),
    axis.line.y.left   = element_line(color = 'black'),
    panel.border       = element_blank())

p2 <- ggplot(list_fevd[[2]], aes(x=factor(periodo), y=value,fill=var)) +
  geom_bar(stat = "identity") +
  labs(x="lag", y="Percentual", title = "Desemprego") +
  scale_y_continuous(expand = c(0,0))+
  scale_fill_grey(name="")+
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 13, colour = "black"),
    axis.title = element_text(size = 13, colour = "black"),
    axis.line.x.bottom = element_line(color = 'black'),
    axis.line.y.left   = element_line(color = 'black'),
    panel.border       = element_blank())




p3 <- ggplot(list_fevd[[3]], aes(x=factor(periodo), y=value,fill=var)) +
  geom_bar(stat = "identity") +
  labs(x="lag", y="Percentual", title = "Expectativa de inflação") +
  scale_y_continuous(expand = c(0,0))+
  scale_fill_grey(name="")+
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 13, colour = "black"),
    axis.title = element_text(size = 13, colour = "black"),
    axis.line.x.bottom = element_line(color = 'black'),
    axis.line.y.left   = element_line(color = 'black'),
    panel.border       = element_blank())

p4 <- ggplot(list_fevd[[4]], aes(x=factor(periodo), y=value,fill=var)) +
  geom_bar(stat = "identity") +
  labs(x="lag", y="Percentual", title = "Inflação") +
  scale_y_continuous(expand = c(0,0))+
  scale_fill_grey(name="")+
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 13, colour = "black"),
    axis.title = element_text(size = 13, colour = "black"),
    axis.line.x.bottom = element_line(color = 'black'),
    axis.line.y.left   = element_line(color = 'black'),
    panel.border       = element_blank())


p4 + p3 + p2 + p1 + plot_layout(ncol = 1)
#ggsave(filename = "decomp-error.png", width = 23, height = 20, units = "cm")

