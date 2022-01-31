Sys.setenv(BEARER_TOKEN = "AAAAAAAAAAAAAAAAAAAAAJ1RYgEAAAAAIN1Y95cuhf1CJbhuqkpd%2BN8lfjI%3DKxBV7RPRAhV5uB9RugrW2ANroRagmUeLfl92KZthJLvCWZuqBo")

require(httr)
require(jsonlite)
require(tidyverse)

bearer_token <- Sys.getenv("BEARER_TOKEN")
headers <- c(Authorization = sprintf('Bearer %s', bearer_token))

n_day <- as.numeric(Sys.Date() - as.Date("2022-01-01"))

old_tweets <- read_csv("results.csv") %>% filter(day!=n_day)

next_token = T

partial_data <- c()
while (!is.null(next_token)) {
    params <- list(
        query=paste(n_day, "@aleixalbo", "#WordleCAT"),
        max_results=10,
        expansions="author_id",
        tweet.fields="author_id",
        user.fields="username"
    )
    
    if (next_token != T) {
        params <- c(params, next_token=next_token)
    }
    
    end_point <- 'https://api.twitter.com/2/tweets/search/recent'
    
    response <-
        httr::GET(url = end_point,
                  httr::add_headers(.headers = headers),
                  query = params
        )
    
    obj <- httr::content(response, as = "text")
    
    json_data <- fromJSON(obj, flatten = TRUE)
    
    next_token <- json_data$meta$next_token
    users <- json_data$includes$users %>% as_tibble() %>% mutate(name = NULL)
    tweets <- json_data$data %>% as_tibble()
    
    p_tweets <- tweets %>% rename(author = author_id) %>%
        mutate(author = deframe(users)[str_extract(author, paste(users$id, collapse = "|"))]) %>%
        select(author, text)  %>%
        mutate(text = str_extract(tweets$text, "[0-9]{1,3}\ ([0-9]|X)")) %>%
        separate(text, into=c("day", "score"), sep = " ")
    
    partial_data <- c(partial_data, list(p_tweets))
}

p_tweets <- bind_rows(partial_data)

p_tweets <- p_tweets %>% mutate(score = ifelse(score == "X", 7, score)) %>%
    mutate(day = as.integer(day), score=as.integer(score)) 

updated_tweets <- bind_rows(p_tweets, old_tweets)

write_csv(updated_tweets, "results.csv")
