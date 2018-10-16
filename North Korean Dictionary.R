
# --------------------------------------------------------------------------------
# Collect North Korean from the National Institue of Korean Language 
# --------------------------------------------------------------------------------

# import packages
library(httr)
library(rvest)
library(tidyverse)

# Korean Consonants
kcs <- c('ㄱ','ㄴ','ㄷ','ㄹ','ㅁ','ㅂ','ㅅ','ㅇ','ㅈ','ㅊ','ㅋ','ㅌ','ㅍ','ㅎ')
# kcs[9]

for (kc in kcs[9:14]) {
  
  # HTTP request
  res <- POST(url = 'http://stdweb2.korean.go.kr/section/north_list.jsp', 
              body = list(Letter = kc, 
                          TableTemp = 'WORD', 
                          go = 1), 
              encode = 'form')
  
  # check HTTP response
  res$status_code
  
  # check total number of words start with letter
  cnt <- res %>% 
    read_html() %>% 
    html_node(css = 'div.result_title') %>% 
    html_text() %>% 
    str_extract(pattern = '\\d+') %>% 
    as.numeric()
  
  # calculate total pages 
  pages <- ceiling(x = cnt / 10)
  cat('Total number of words :', cnt, '(', pages, 'page )\n')
  
  # create an empty data.frame for the result. 
  words <- data.frame()
  
  # fun for-loop 
  for (page in 1:pages) {
    
    # show current proceeding
    cat('Letter:', kc, ', Current page: ', page, '/', pages, '---> ')
    
    # HTTP request
    res <- POST(url = 'http://stdweb2.korean.go.kr/section/north_list.jsp', 
                body = list(Letter = kc, 
                            TableTemp = 'WORD', 
                            go = page), 
                encode = 'form')
    
    # show HTTP response status code
    cat('Status code :', res$status_code, '\n')
    
    # collect words
    dat <- res %>% 
      read_html() %>% 
      html_nodes(css = 'div.list_table p.exp') %>% 
      html_text() %>% 
      str_trim() 
    
    df <- data.frame(letter = kc, dictionary = dat)
    
    # add dat to words
    words <- rbind(words, df)
    
    # sleep for 1 second
    Sys.sleep(time = 1) 
  }
  
  # check the last element of words
  n <- length(x = words)
  words[n]
  
  # add words to result 
  result <- rbind(result, words)
}

# check frequency of North Korean letters
table(result$letter)


# save as RDS
saveRDS(object = result, file = './data/NKwords.RDS')


# --------------------------------------------------------------------------------
# text data preparation
# --------------------------------------------------------------------------------

# read RDS 
result <- readRDS(file = './data/NKwords.RDS')

# check class
class(x = result)

# print first 10 rows 
head(x = result, n = 10L)

# text preparation using stringr package functions! 
output <- 
  result$dictionary %>% 
  str_remove_all(pattern = '〔[가-힣\\[\\]\\(\\)ː\\-/,\n ]+〕') %>% 
  str_remove_all(pattern = '(\n.+어근\\.)|(〔.+〕)') %>% 
  str_remove_all(pattern = '([『「\\[][\\dⅠⅡ]{1,2}[』」\\]](\n)*)|\n(?=〕)|(【.+】)') %>% 
  str_remove_all(pattern = '(\\(){2}[가-힣ㄱ-ㅎ\\-\\/‘’,… ]+(\\)){2}') %>% 
  str_replace_all(pattern = '\\.\\)', replacement = '); ') %>% 
  str_replace_all(pattern = '(\n)+', replacement = '; ') %>% 
  str_replace_all(pattern = '[』」]; [『「]', replacement = '/') %>% 
  str_replace_all(pattern = '[『』「」]', replacement = '; ') %>% 
  str_replace_all(pattern = '=', replacement = '.동의어:') %>% 
  str_replace_all(pattern = '≒', replacement = '.유의어:') %>% 
  str_replace_all(pattern = '; ; ', replacement = '; ') %>% 
  str_replace_all(pattern = '(\\. (?=[가-힣]))', replacement = ': ') %>% 
  str_split(pattern = '\\.') %>% 
  sapply(FUN = `[`, 1) %>% 
  str_split(pattern = '; ')


# check the location that has more than 3 columns
sapply(X = output, FUN = length) %>% table()
##     3     4     5 
## 60741     1     1 

# # create a data.frame for text preparation
# dt <- data.frame(
#   v1 = sapply(X = output, FUN = `[`, 1), 
#   v2 = sapply(X = output, FUN = `[`, 2), 
#   v3 = sapply(X = output, FUN = `[`, 3),
#   v4 = sapply(X = output, FUN = `[`, 4),
#   v5 = sapply(X = output, FUN = `[`, 5))
# 
# which(x = is.na(x = dt$v5) == FALSE)
# ## [1] 40196
# 
# which(x = is.na(x = dt$v4) == FALSE)
# ## [1] 35468
# 
# loc <- 40196
# dt[loc, ]
# 
# # cleanse text using regex! 
# result$dictionary[loc] %>% 
#   str_remove_all(pattern = '〔[가-힣\\[\\]\\(\\)ː\\-/,\n ]+〕') %>% 
#   str_remove_all(pattern = '(\n.+어근\\.)|(〔.+〕)') %>% 
#   str_remove_all(pattern = '([『「\\[][\\dⅠⅡ]{1,2}[』」\\]](\n)*)|\n(?=〕)|(【.+】)') %>% 
#   str_remove_all(pattern = '(\\(){2}[가-힣ㄱ-ㅎ\\-\\/‘’,… ]+(\\)){2}') %>% 
#   str_replace_all(pattern = '\\.\\)', replacement = '); ') %>% 
#   str_replace_all(pattern = '(\n)+', replacement = '; ') %>% 
#   str_replace_all(pattern = '[』」]; [『「]', replacement = '/') %>% 
#   str_replace_all(pattern = '[『』「」]', replacement = '; ') %>% 
#   str_replace_all(pattern = '=', replacement = '.동의어:') %>% 
#   str_replace_all(pattern = '≒', replacement = '.유의어:') %>% 
#   str_replace_all(pattern = '; ; ', replacement = '; ') %>% 
#   str_replace_all(pattern = '(\\. (?=[가-힣]))', replacement = ': ') %>% 
#   str_split(pattern = '\\.') %>% 
#   sapply(FUN = `[`, 1) %>% 
#   str_split(pattern = '; ')
#
# # search location that has a unique pattern 
# which(x = str_detect(string = result$dictionary, pattern = '\\.\\)'))
# which(x = str_detect(string = result$dictionary, pattern = '[가-힣]「'))


# create a data.table 
NKdic <- data.frame(
  단어 = sapply(X = output, FUN = `[`, 1), 
  품사 = sapply(X = output, FUN = `[`, 2), 
  설명 = sapply(X = output, FUN = `[`, 3))


# save as RDS
saveRDS(object = NKdic, file = './data/NKdictionary.RDS')

