# Initializing ====
#' This code is based upon the great work of Henrique Martins
#' https://www.linkedin.com/in/henriquecastror/
#' Post that this was based on:
#' https://henriquemartins.net/post/2020-08-18-betas/
#' 
#' The originality here refers to the creation of a gif showing the betas over
#' time, with annotations and markers to enhance compreension

# Reading libraries
library(BatchGetSymbols)
library(dplyr)
library(gganimate)
library(ggplot2)
library(gifski)
library(plotly)
library(roll)
library(tidyquant) 
library(tidyr)

# Creating MGLU ====
# defining period
first.date  <- "2015-01-01"
last.date   <- "2020-08-18"
freq.data   <- 'daily'

# getting data
ibov <- BatchGetSymbols(tickers = "^BVSP",
                        first.date = first.date,
                        last.date = last.date,
                        thresh.bad.data = 0.5,
                        freq.data = freq.data)

asset_mglu <- BatchGetSymbols(tickers = "MGLU3.SA",
                         first.date = first.date,
                         last.date = last.date,
                         thresh.bad.data = 0.5,
                         freq.data = freq.data)

ret_ibov <- ibov$df.tickers  %>%
  tq_transmute(
    select = price.adjusted,
    mutate_fun = periodReturn,
    period = 'daily',
    col_rename = 'return',
    type = 'log'
  )

ret_asset_mglu <- asset_mglu$df.tickers  %>%
  tq_transmute(
    select = price.adjusted,
    mutate_fun = periodReturn,
    period = 'daily',
    col_rename = 'return',
    type = 'log'
  )

# joining data
ret_mglu <- ret_ibov %>% 
  left_join(ret_asset_mglu, by = "ref.date")

# creating variance
window <- 230
ret_mglu$var <- roll_cov(ret_mglu$return.x, ret_mglu$return.x, width = window)
ret_mglu$cov <- roll_cov(ret_mglu$return.x, ret_mglu$return.y, width = window)
ret_mglu$beta <- ret_mglu$cov / ret_mglu$var

# excluding missings
ret_mglu <- subset(ret_mglu, ret_mglu$beta != "NA" )

# Creating VVAR ====
# defining period
first.date  <- "2015-01-01"
last.date   <- "2020-08-18"
freq.data   <- 'daily'

# getting data
ibov <- BatchGetSymbols(tickers = "^BVSP",
                        first.date = first.date,
                        last.date = last.date,
                        thresh.bad.data = 0.5,
                        freq.data = freq.data)

asset_vvar <- BatchGetSymbols(tickers = "VVAR3.SA",
                              first.date = first.date,
                              last.date = last.date,
                              thresh.bad.data = 0.5,
                              freq.data = freq.data)

ret_ibov <- ibov$df.tickers  %>%
  tq_transmute(
    select = price.adjusted,
    mutate_fun = periodReturn,
    period = 'daily',
    col_rename = 'return',
    type = 'log'
  )

ret_asset_vvar <- asset_vvar$df.tickers  %>%
  tq_transmute(
    select = price.adjusted,
    mutate_fun = periodReturn,
    period = 'daily',
    col_rename = 'return',
    type = 'log'
  )

# joining data
ret_vvar <- ret_ibov %>% 
  left_join(ret_asset_vvar, by = "ref.date")

# creating variances
window <- 230
ret_vvar$var <- roll_cov(ret_vvar$return.x, ret_vvar$return.x, width = window)
ret_vvar$cov <- roll_cov(ret_vvar$return.x, ret_vvar$return.y, width = window)
ret_vvar$beta <- ret_vvar$cov / ret_vvar$var

# excluding missings
ret_vvar <- subset(ret_vvar, ret_vvar$beta != "NA" )


# Joining and pivoting MGLU & VVAR ====

# creating final dataframe: ret_total
ret_total <- ret_mglu %>% 
  inner_join(ret_vvar, by = "ref.date", suffix = c("_MGLU3", "_VVAR3"))

head(ret_total)

# pivoting beta
ret_long <- ret_total %>% 
  pivot_longer(
    cols = starts_with("beta"),
    names_to = "stock",
    names_pattern = "beta_(.*)",
    values_to = "beta"
  )

ret_long

# Creating final gif plot ====
p <- ret_long %>%
  
  # Initial aesthetics
  ggplot(aes(x = ref.date, y = beta, colour = stock)) +
  
  # Creating line geom
  geom_line(size = 0.8) +
  
  # Theme chosen: theme_minimal()
  theme_minimal() +

  # Labeling axis
  labs( y = "", x="", title = "MGLU3 X VVAR3: Comparing Betas Over Time (2016-2020)") +
  
  
  # Choosing to show yintercept '1' (to better compare betas around this line)
  geom_hline(yintercept = 1, color = "black", size = .1) +
  
  # Choosing the colors of lines using ggplot2::scale_color_brewer
  scale_color_brewer(name = "", palette = "Set2") +
  
  # Annotations
  annotate(geom = "point", x = as.Date("2020-01-29"), y = 1.22,
           size = 10, shape = 21, fill = "transparent") +
  
  annotate(geom = "text", x = as.Date("2019-06-29"), y = 0.22,
           label = "In January 29th of 2020, \n VVAR3's Beta surpasses MGLU3's") +
  
  transition_reveal(ref.date)


# Animating into gif object 'p'
p <- animate(p, 
             # end_pause indicates the amount to be paused after ending
             end_pause = 10,
             renderer = gifski_renderer())
p

# Saving gif object 'p'
anim_save("beta_comparison_mglu_vvar.gif", p)
