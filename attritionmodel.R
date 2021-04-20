##############################################
#####    EMPLOYEE ATTRITTION MODEL     #######
##############################################

# Libs ----
library(dplyr)
library(readr)
library(h2o)
library(highcharter)
library(ggplot2)
library(htmltools)
library(tidyverse)
library(magrittr)
library(stringr)
library("ggpubr")
library(summarytools)


# FUNÇÕES ----
## Esta função é usada para filtrar uma variável retirando os atributos que não desejamos. ----
'%!in%' <- function(x,y)!('%in%'(x,y))


# CARREGAR DADOS ----
## Carrega os dados do excel ----
df_attrition <-
  readxl::read_xlsx('data/Dataset.xlsx') %>% 
  mutate(Attrition_numeric = ifelse(Attrition == 'No',0,1) )

# VARIAVEIS ----
## Variavel para armazenar colunas factor ----
columns_fact <- c('Attrition', 'BusinessTravel', 'Department',
                  'EducationField', 'Gender', 'JobRole', 'MaritalStatus',
                  'Over18', 'OverTime','EmployeeCount', 'StandardHours', 'Attrition_numeric')

## Data de atualização da analise ----
data_atual = Sys.Date();

# Encoding genérico para renderizações ----
encoding <- "UTF-8"



# DF ANALISE ESTATISTICA BASICA ----
## numerica ----
df_attrition.num <-  df_attrition %>% 
                        select(-columns_fact) %>%
                        summarytools::descr()

## texto ----
df_attrition.char <- df_attrition %>% 
                        select(columns_fact)



# GRAFICOS ----
## Graficos de densidade ----
charts <- list()
columns <- 
  data.frame(nome_colunas=character()) %>%  
  add_row(nome_colunas = names(df_attrition)) %>% 
  filter(nome_colunas  %!in% columns_fact)

for (i in 1:length(columns$nome_colunas)) {
  
  x <- columns$nome_colunas[i]
  
  p<-  
    df_attrition %>% 
    ggplot(aes(x=.data[[x]], fill=Attrition)) +
    geom_density(alpha=0.4)+
    labs(title = x,x=NULL,y=NULL)+
    theme(legend.direction="horizontal",
          legend.position = "bottom")

    charts[[i]] <- p
  }


#  VERIFICAR AS VARIÁVEIS MAIS IMPORTANTES PARA O ATTRITION RANDOM FOREST ----

## Converter variaveis do tipo factor para numerica ----
df_attrition$OverTime <- as.numeric(as.factor(df_attrition$OverTime))
df_attrition$JobRole <- as.numeric(as.factor(df_attrition$JobRole))
df_attrition$BusinessTravel <- as.numeric(as.factor(df_attrition$BusinessTravel))
df_attrition$Department <- as.numeric(as.factor(df_attrition$Department))
df_attrition$EducationField <- as.numeric(as.factor(df_attrition$EducationField))
df_attrition$Gender <- as.numeric(as.factor(df_attrition$Gender))
df_attrition$MaritalStatus <- as.numeric(as.factor(df_attrition$MaritalStatus))

## Lista que receberá os resultados de ml que serão disponibilizados em relatório ----
ml <- list()

## Iniciar a JVM do H2O no seu computador ----
h2o::h2o.init(max_mem_size = "4g")

## Garantir que a JVM do H2O não está com qualquer dado em memória antes de começar as análises ----
h2o::h2o.removeAll()

## Colocar os dados no h2o ----
df_attrition_h2o <- h2o::as.h2o(df_attrition %>% dplyr::select(-c("EmployeeNumber")))

## Dividir dados em amostra de treinamento e teste ----
split_h2o_initial <- h2o.splitFrame(data = df_attrition_h2o, ratios = 0.80, seed = 1234)
train_h2o_initial <- h2o.assign(split_h2o_initial[[1]], "train") # 80%
test_h2o_initial  <- h2o.assign(split_h2o_initial[[2]], "test")  # 20%

## Definir a variável a ser prevista (y) e as variáveis que podem explicar o turnover (x) ----
y <- "Attrition_numeric"
x <- setdiff(names(train_h2o_initial), y)


## Modelo random forest ----
rf_model <- h2o::h2o.randomForest(
  model_id = "importancia_variaveis_rf",
  x = x, 
  y = y,
  training_frame = train_h2o_initial,
  ntrees = 80,
  mtries = -1, 
  sample_rate = 0.7,
  seed = 123,
  balance_classes = FALSE, 
  max_depth = 5,
  stopping_rounds = 3,
  #stopping_metric = 'misclassification',
  stopping_tolerance = 1e-3)

## Obter a importância das variáveis ----
ml$var_imp <- h2o.varimp(rf_model)

## Gráfico que mostra a importância das variáveis----
h6 <- highchart() %>%
  hc_add_series(ml$var_imp$percentage*100, name = "", color = '#64AEEF') %>%
  hc_chart(type = "bar") %>% 
  hc_xAxis(categories = ml$var_imp$variable) %>%
  hc_yAxis(title = list(text = "Percentage"), labels = list(format = "{value}%")) %>%
  hc_chart(zoomType = "xy") %>%
  hc_legend(enabled = FALSE)


# Render relatorio html ----
rmarkdown::render(
  input = "Attrition Model.Rmd",
  encoding = "UTF-8", clean = T,
  output_file = paste0("Relatorio_Attrition_",data_atual,".html"))



