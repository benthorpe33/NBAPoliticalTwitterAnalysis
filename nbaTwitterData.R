library(tidyverse)
library(rtweet)
library(infer)

lebron <- get_timelines("kingjames", n = 1000)
dame <- get_timelines("dame_lillard", n = 1000)

View(lebron %>% filter(grepl("Black", text, fixed = TRUE)))
View(dame %>% filter(grepl("Black", text, fixed = TRUE)))

vec <- c("kingjames", "dame_lillard")

black_tweets <- function(x, keyword){
        all_tweets <- data.frame()
        for(user in x){
                df <- get_timelines(user, n = 100) %>% filter(grepl(keyword, text, fixed = TRUE))
                all_tweets <- rbind(all_tweets, df)
        }
        return(all_tweets)
}


get_tweets <- function(x){
        all_tweets <- data.frame()
        for(user in x){
                df <- get_timelines(user, n = 30)
                all_tweets <- rbind(all_tweets, df)
        }
        return(all_tweets)
}

twitter_handles <- read_csv("data/NBATwitter.csv")
twitter_handles <- twitter_handles %>%
        filter(Rk != "Rk")

player_stats <- read_csv("data/PlayerStats.csv")
player_stats <- player_stats %>%
        filter(Rk != "Rk")

player_stats <- left_join(player_stats, twitter_handles, by="Player")

player_stats <- player_stats[!duplicated(player_stats$Player),]

player_stats <- player_stats %>% filter(!is.na(Twitter)) %>%
        mutate(G = as.numeric(G)) %>%
        filter(G>5) %>%
        mutate(MP = as.numeric(MP)) %>%
        rename(screen_name=Twitter)

player_tweets <- get_tweets(player_stats$Player)

tweets_with_stats <- left_join(player_tweets, player_stats, by="screen_name") %>%
        mutate(MP = as.numeric(MP))

model <- lm(favorite_count ~ MP, data=tweets_with_stats)
tidy(model)

paste()

words <- 'black, democracy, justice, breonna, taylor, trump, Biden, blm, corona, equality, racism, floyd, president, blacklivesmatter, peace, ahmaud, arbery, salute, stacey, abrams, kamala, harris, pence, vote, country, election, poll, blake, change, equality, barack, obama, voting, disenfranchise, incarcerate, race, racial, education, reform, national, immunity, cops, police, defund, kenosha, equalities, arrest, law, brutality, social, lives, inclusion, ballot, 45'
