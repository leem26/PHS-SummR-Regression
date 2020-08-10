library(dplyr)
library(RCurl)
library(gganimate)
library(transformr)
library(magick)
library(gifski)

big.url <- getURL("https://raw.githubusercontent.com/leem26/theFUNStudy/master/data/FUN_data_100000obs.csv")
big.FUN <- read.csv(text = big.url)

set.seed(1)
big.500 <- sample_n(big.FUN, size = 500)

squared_error <- function(param) {
  e <- big.500$A.con * param[1] + param[2]
  es <- sum((e - big.500$Y.con)^2)
  i <<- i + 1
  params_l[[i]] <<- data.frame(i = i, squared_error = es, b1 = param[1], b0 = param[2])
  return(es)
}

i <- 0
params_l <- list()
o <- optim(c(b1 = 1.8, b0 = 5), squared_error, control = list(trace = T))
params <- do.call('rbind', params_l)

### Create animation frame ######################################
record_anim <- lapply(1:nrow(params), function(i) {
  b0 <- params$b0[i]
  b1 <- params$b1[i]
  d <- big.500 %>% 
    mutate(Y.con_pred = b0 + b1 * A.con) %>% 
    mutate(residual = Y.con - Y.con_pred) %>%
    mutate(residual2 = residual^2) %>%
    mutate(residual2z = residual2 %>% scale %>% as.numeric) %>%
    mutate(m_Y.con = mean(Y.con)) %>% 
    mutate(param_b0 = b0, param_b1 = b1, state = i)
  return(d)
}) %>% bind_rows

record_anim <- record_anim %>% 
  mutate(f = sprintf('Y.con = %1.2f A.con + %1.2f', param_b1, param_b0)) %>% 
  mutate(f = ifelse(A.con == min(A.con), f, NA_character_))

### Create animation ##################################

p1 <- ggplot(record_anim) +
  geom_segment(aes(x = A.con, y = Y.con, xend = A.con, yend = Y.con_pred),
               size = 0.8, color = "coral", alpha = 0.2) + 
  geom_point(aes(x = A.con, y = Y.con), shape = 21, size = 1, fill = "NA", color = "black", alpha = 0.6) +
  geom_line(aes(x = A.con, y = Y.con_pred), color = 'royalblue4', size = 0.5) +
  labs(x = "Hours of sleep", y = "FDS Use (times/week)") +
  theme_xaringan(text_font_size = 36, title_font_size = 10) + 
  coord_cartesian(ylim = c(0, 16), xlim = c(0, 12))  + 
  theme(axis.text = element_blank()) +
  transition_states(state, transition_length = 3, state_length = 0, wrap = F) +
  ease_aes('linear')

p1_gif <- animate(p1, fps = 50, nframes = 250, end_pause = 100, res = 300,
                  width = 6, height = 4, units = "in", renderer = gifski_renderer())
p1_gif

anim_save(p1_gif, "ols-animate.gif")
