# bibliotecas

library(agridat) # banco de dados
library(dplyr) # manipulação de dados
library(kableExtra) # tabelas
library(ggplot2) # criação de gráficos
library(desplot) # criação de gráficos
library(RColorBrewer) # estética dos gráficos

# dados ------------------------------------------------------------------------

data <- agridat::bridges.cucumber

str(data)

# qualidade dos dados ----------------------------------------------------------

sum(is.na(data))

levels(data$loc)

levels(data$gen)

# EDA --------------------------------------------------------------------------

glimpse(data)

summary(data)

## tabela descrevendo cada variável

Variável <- c("loc",
              "gen",
              "row",
              "col",
              "yield")

Desçrição <- c("As duas localidades envolvidas nesta pesquisa são Clemson e Tifton.", 
          "Os quatro genótipos de pepino: Dasher, Guardian, Sprint, Poinsett.", 
          "O identificador da linha da observação.",
          "O identificador da coluna da observação.",
          "Peso dos frutos comercializáveis por parcela.")

data.frame(Variável, Desçrição) %>% 
  kbl() %>% 
  kable_styling(bootstrap_options = c("hover", "striped", "bordered"))

## gráfico dos quadrados latinos

palette <- brewer.pal(9, "YlGnBu")

desplot <- desplot(data = data,
                   form = yield ~ row * col | loc,
                   text = yield,
                   cex = 1,
                   strip.cex = 1,
                   show.key = F,
                   col.regions = palette,
                   main = "Os quadrados latinos por locação")
## gráfico boxplot

palette_gen <- brewer.pal(4, "Dark2")

boxplot <- ggplot(data, aes(x = gen, y = yield, colour = gen, fill = gen)) +
            geom_boxplot(alpha = 0.5) +
            stat_boxplot(geom = "errorbar") +
            geom_point() +
            facet_wrap(~loc) +
            scale_colour_manual(values = palette_gen) +
            scale_fill_manual(values = palette_gen) +
            theme_classic() +
            theme(legend.position = "none",
                  strip.text = element_text(size = 12),
                  strip.background = element_rect(fill = "grey90"),
                  plot.title = element_text(face = "bold", size = 13, vjust = 2)) +
            labs(x = "Genótipo",
                 y = "Produção (Peso)",
                 title = "Produção por Genótipos de Pepino em Clemson e Tifton")




