library(imfr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidysynth)
library(WDI)

# PARÂMETROS
aa = 2010
fin = 2020
ai = '2010-01'
maxi = '2021-10'
comp = '2019-01'

# COLETANDO DADOS

##### IFM #####
# CPI
CPI = imf_data(
  database_id = 'CPI',
  indicator = c('PCPI_PC_PP_PT'),
  country = "all",
  start = aa,
  end = current_year(),
  freq = "M",
  return_raw = FALSE,
  print_url = FALSE,
  times = 3)

colnames(CPI)[3] = c('PCPI_IX')

remov = CPI %>% filter(year_month == ai,
                is.na(PCPI_IX)) %>% select(iso2c)
remov = as.vector(remov$iso2c)
remov = unique(c(remov,unique(CPI[is.na(CPI$PCPI_IX),1])))

for (i in remov) {
  CPI = CPI[!grepl(i,CPI$iso2c),]
}
for (i in unique(CPI$iso2c)) {
  if(sum(CPI$iso2c==i & CPI$year_month==ai)==0||sum(CPI$iso2c==i & CPI$year_month==maxi)==0){
    CPI = CPI[!grepl(i,CPI$iso2c),]
  }
}

CPI$PCPI_IX = CPI$PCPI_IX/100
for (i in unique(CPI$iso2c)) {
  for (j in 1:dim(CPI[CPI$iso2c == i,])[1]) {
    if (j == 1) {
      CPI[CPI$iso2c == i,3][1] = 1*(1+CPI[CPI$iso2c == i,3][1])
    } else {
      CPI[CPI$iso2c == i,3][j] = CPI[CPI$iso2c == i,3][j-1]*(1+CPI[CPI$iso2c == i,3][j])
    }
  }
}

##### WB #####
coleta = c('NY.GDP.PCAP.KD','NV.AGR.TOTL.ZS','NY.GNS.ICTR.ZS','GB.DOD.TOTL.GDP.ZS',
           'NE.RSB.GNFS.ZS','SL.UEM.TOTL.ZS','SE.SEC.CUAT.UP.ZS','SE.PRM.UNER.ZS',
           'EN.ATM.CO2E.KD.GD','AG.LND.FRST.ZS','SI.POV.GINI','SI.POV.MDIM',
           'SP.DYN.LE00.IN','SP.DYN.IMRT.IN','SP.POP.DPND')
variaveis = c('PIBPC','VADDAG','POUP','BC','DESEMP','EMCOMP','CFESC',
              'CO2','FLOREST','DESISOL','POBR','EXPVID','MORTINF','DEPEND')

wdi_dat <- WDI(indicator = coleta, start = aa, end = fin, extra = TRUE) 
colnames(wdi_dat)[4:17] = variaveis

#unindo dados
wdi_dat$chave = paste(wdi_dat$iso2c,wdi_dat$year,sep = '-')
CPI$chave = paste(CPI$iso2c,substr(CPI$year_month,1,4),sep = '-')
data = left_join(CPI,wdi_dat,by = 'chave')
data = data[,c(1:3,6:21,23,24,27)]
colnames(data)[1:5] = c('iso2c','ano_mes','IPC','país','ano')
data$data = as.Date(paste('01',substr(data$ano_mes,6,7),substr(data$ano_mes,1,4),sep = '/'),
                    format = '%d/%m/%Y')
data = data %>% select(iso2c, data, ano, país,region,capital,income,IPC,PIBPC,VADDAG,
                POUP,BC,DESEMP,EMCOMP,CFESC,CO2,FLOREST,DESISOL,POBR,EXPVID,MORTINF,DEPEND)
data$ref = 0
data$PIBPC = log(data$PIBPC)

#CORRIGINDO IDC
for (i in unique(data$iso2c)) {
  data$IPC[data$iso2c==i] = (data$IPC[data$iso2c==i]/data$IPC[data$iso2c==i][123])*100
  data$ref[data$iso2c==i] = 1:length(data$ref[data$iso2c==i])
}

d = data.frame(table(data$iso2c))
d = d[d$Var1=="BR",2]
data = data[data$ref<=d,]
colnames(data)[4] = 'pais'
data = data[,-19]

# Remover sujos
verif = data.frame()
j=0
for (i in unique(data$iso2c)) {
  j=j+1
  verif[j,1] = i
  verif[j,2:16] = colSums(is.na(data[data$iso2c==i,]))[8:22]
}
verif$exc = rowSums(verif==142)
remov = as.vector(verif$V1[verif$exc>0])

for (i in remov) {
  data = data[!grepl(i,data$iso2c),]
}


#Controle Sintético

dados = data[data$ref>=73,]
for (i in unique(dados$iso2c)) {
  for (j in c(19,15)) {
    dados[dados$iso2c==i,j] = mean(data[data$iso2c==i,j], na.rm = T)
  }
}
dados$ref = dados$ref-72

verif = data.frame()
j=0
for (i in unique(dados$iso2c)) {
  j=j+1
  verif[j,1] = i
  verif[j,2:16] = colSums(is.na(dados[dados$iso2c==i & dados$ref <=51,]))[8:22]
}
verif$exc = rowSums(verif==51)
remov = as.vector(verif$V1[verif$exc>0])
for (i in remov) {
  dados = dados[!grepl(i,dados$iso2c),]
}

sintetico = dados %>% synthetic_control(outcome = IPC,
                                        unit = iso2c,
                                        time = data,
                                        i_unit = "BR",
                                        i_time = as.Date('01/03/2020', format = '%d/%m/%Y'),
                                        generate_placebos = T) %>%
  generate_predictor(time_window = seq(as.Date('01/01/2016', format = '%d/%m/%Y'), length =17, by = 'month'), 
                     PIBPC = mean(PIBPC, na.rm = T),
                     DEPEND = mean(DEPEND, na.rm = T),
                     EXPVID = mean(EXPVID, na.rm = T)) %>%
  generate_predictor(time_window = seq(as.Date('01/06/2017', format = '%d/%m/%Y'), length =34, by = 'month'), 
                     PIBPC_2 = mean(PIBPC, na.rm = T),
                     DEPEND_2 = mean(DEPEND, na.rm = T),
                     EXPVID_2 = mean(EXPVID, na.rm = T)) %>%
  generate_predictor(time_window = seq(as.Date('01/01/2016', format = '%d/%m/%Y'), length =51, by = 'month'), 
                     EMCOMP = mean(EMCOMP, na.rm = T),
                     DESISOL = mean(DESISOL, na.rm = T)) %>%
  generate_weights(optimization_window = seq(as.Date('01/01/2016', format = '%d/%m/%Y'), length =51, by = 'month'),
                   margin_ipop = 0.02, sigf_ipop = 7, bound_ipop = 6) %>%
  generate_control()

sintetico %>% plot_trends()+
  labs(
    title = 'Controle sintético e real',
    caption = 'Fonte: IFM e Banco mundial. Elaboração: Evânio Marques',
    x = 'Data',
    y = 'Índice de Preços ao Consumidor'
  )

sintetico %>% grab_signficance() %>% filter(type == 'Treated')

sintetico %>% plot_differences()+
  labs(
    title = 'Impacto da gestão administrativa sobre os preços',
    caption = 'Fonte: IFM e Banco mundial. Elaboração: Evânio Marques',
    x = 'Data',
    y = 'Diferença real e sintético'
  )

sintetico %>% plot_weights()

pesos = sintetico[[7]][[2]]
colnames(pesos) = c('iso2c','pesos')
pesos = unique(left_join(pesos, dados[!is.na(dados$pais),c(1,4)], by = 'iso2c'))
pesos$pesos = round(pesos$pesos*100,digits = 2)
pesos = pesos[order(pesos$pesos, decreasing = TRUE),]
pesos$pais = with(pesos,reorder(pais, pesos))
pesos = pesos %>% filter(pesos>=0.5)

ggplot(pesos, aes(x = pais, y = pesos)) +
  geom_col(fill = "#112446") +
  labs(
    x = "Países",
    y = "Pesos",
    title = "Pesos por Países",
    caption = "Fonte: IFM e Banco Mundial. Elaboração: Evânio Marques."
  ) +
  coord_flip() +
  theme_minimal()+
  geom_text(aes(label = pesos), size = 4, colour = 'black', nudge_x = 0, nudge_y = 1)
  

sintetico %>% grab_balance_table()

sintetico %>% plot_placebos() +
  labs(
    title = 'Impacto da gestão administrativa sobre os preços',
    caption = 'Fonte: IFM e Banco mundial. Elaboração: Evânio Marques',
    x = 'Data',
    y = 'Diferença real e sintético'
  )

sintetico %>% plot_mspe_ratio() +
  labs(
    title = 'Taxa de Post / Pré MSPE por Placebo',
    caption = 'Fonte: IFM e Banco mundial. Elaboração: Evânio Marques',
    y = 'Post / Pré MSPE'
    )

sintetico %>% grab_signficance()