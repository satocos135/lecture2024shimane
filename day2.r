# パッケージのインストール
install.packages('igraph') # ネットワーク表示用
install.packages('openxlsx')  # Excel読み込み用




# getwd()
# setwd('GitHub/lecture2023shimane/')
getwd()
setwd('C:/Users/satoc/Documents/GitHub/lecture2023shimane')

# パッケージの読み込み
library('openxlsx')
library('tidyverse')
library('RMeCab')
library('igraph')

# 関数群の読み込み
source('functions.r', encoding='utf8')

# ファイルの読み込み
# データセットの成形

files = c(
    './data/interview/応用Ⅰ整理データ_1班.xlsx',
    './data/interview/応用Ⅰ整理データ_2班.xlsx',
    './data/interview/応用Ⅰ整理データ_3班.xlsx',
    './data/interview/応用Ⅰ整理データ_4班.xlsx',
    './data/interview/応用Ⅰ整理データ_5班.xlsx'
)

read.xlsx(files[1], sheet='data')
?read.xlsx
for(i in 1:5){
    if(i == 1){
        df = read.xlsx(files[i], sheet='data')
        df$group = i
    }else{
        x = read.xlsx(files[i], sheet='data')
        x$group = i
        df = rbind(df, x)
    }
}

df %>% dim()
df %>% colnames()
table(df$group, df$person)


# 課題1 
# クリーニング
df[is.na(df)] = 0

# 学生のデータを除く

df %>% filter(str_detect(person, '学')) %>% select(person) %>% table()
counted = table(df$person)
excluded = counted[counted < 10] %>% names()

df = df %>% filter(! str_detect(person, '学'))
df = df %>% filter(! person %in% excluded)


# 括弧の部分を削る
df$content %>% str_extract_all('[(（][^)）]+[)）]') 
df$content = df$content %>% str_replace_all('[(（][^)）]+[)）]', '')

# 謎のカタカナ
df$content = df$content %>% str_replace('[ァ-ヴ]+$', '')


# ストップワード
stopwords = c('ん', 'こと', 'それ', 'の', 'よう', 'ところ', '方', 'さん', '何', 'ー',
              'ぉ', 'もの', 'たち', 'あれ', 'とき', '中', '一', 'そこ', 'これ', 'なん',
              'みたい', 'わけ', 'そこ', '時', 'ら', 'そう', 'とこ', '感じ', 'ここ', '日', '年',
              '回', '十', 'ぎゃ')

symbols = '^[0-9０-９,.、。〇～〜~ー\\-!?！？，．…]+$'


# 表記ゆれ
# 子供 -> 子ども
# あたし -> 私
# みなさん -> 皆さん
df$content = df$content %>% str_replace_all('あたし', '私')
df$content = df$content %>% str_replace_all('わたし', '私')
df$content = df$content %>% str_replace_all('子供', '子ども')
df$content = df$content %>% str_replace_all('こども', '子ども')
df$content = df$content %>% str_replace_all('みなさん', '皆さん')
df$content = df$content %>% str_replace_all('笹巻き', '笹巻')



# 上位頻出語 
groups = df %>% group_by(group) %>% summarise(text = paste0(content, collapse=''))
groups = as.data.frame(groups)
groups %>% dim()
groups %>% colnames()
str(groups)
count_noun = docMatrixDF(groups[, 'text'], pos=c('名詞'))
count_noun %>% dim()
count_noun

freq_noun = count_noun %>% rowSums()
# freq_noun %>% sort() %>% tail(30) %>% barplot(horiz=T, las=1, main='Top 30 nouns', xlab='Frequency', cex.names=0.5)

freq_noun[(! names(freq_noun) %in% stopwords) & (! str_detect(names(freq_noun), symbols))] %>% sort() %>% tail(30) %>% barplot(horiz=T, las=1, main='Top 30 nouns', xlab='Frequency', cex.names=1)

count_noun[(! rownames(count_noun) %in% stopwords) & (! str_detect(rownames(count_noun), symbols)), 'ROW.1']  %>% sort() %>% tail(30) %>% barplot(horiz=T, las=1, main='Top 30 nouns', xlab='Frequency', cex.names=1)

# 課題2
# グループごとの頻出語

remove_stopwords = function(df, stopwords, symbols){
    return(df[(! rownames(df) %in% stopwords) & (! str_detect(rownames(df), symbols)), ])
}

remove_stopwords(count_noun, stopwords, symbols)

install.packages('ragg')
library('ragg')

remove_stopwords(count_noun, stopwords, symbols)[, 'ROW.1'] %>% sort() %>% tail(30) %>% barplot(horiz=T, las=1, main='Top 30 nouns', xlab='Frequency', cex.names=1)
remove_stopwords(count_noun, stopwords, symbols)[, 'ROW.2'] %>% sort() %>% tail(30) %>% barplot(horiz=T, las=1, main='Top 30 nouns', xlab='Frequency', cex.names=1)
remove_stopwords(count_noun, stopwords, symbols)[, 'ROW.3'] %>% sort() %>% tail(30) %>% barplot(horiz=T, las=1, main='Top 30 nouns', xlab='Frequency', cex.names=1)
remove_stopwords(count_noun, stopwords, symbols)[, 'ROW.4'] %>% sort() %>% tail(30) %>% barplot(horiz=T, las=1, main='Top 30 nouns', xlab='Frequency', cex.names=1)
remove_stopwords(count_noun, stopwords, symbols)[, 'ROW.5'] %>% sort() %>% tail(30) %>% barplot(horiz=T, las=1, main='Top 30 nouns', xlab='Frequency', cex.names=1)


df %>% filter(str_detect(content, 'アイヅチ') & group==1) %>% select(id, content)


# 課題3 
# TFIDF

tfidf = tf(count_noun) * idf(count_noun)


# TFIDFを一枚のplotにおさめる
par(mfrow=c(1, 5)) # 1行5列
for(i in 1:5){
    remove_stopwords(tfidf, stopwords, symbols)[, i] %>%
        sort() %>%
        tail(30) %>% barplot(horiz=T, las=2, main=character(i), cex.names=1.5)
}
par(mfrow=c(1, 1)) 

df[str_detect(df$content, 'ぎゃ'), 'content'] # 除外
df[str_detect(df$content, 'とり'), 'content'] # とりかみ、しとる（方言）
df[str_detect(df$content, 'ろう'), 'content'] # 分析ミス
df[str_detect(df$content, 'ばん'), 'content'] # 分析ミス
df[str_detect(df$content, 'てん'), 'content'] # てんば(方言)　お転婆
df[str_detect(df$content, '笹巻'), 'content'] # 笹巻　笹巻き　→　統一



# 課題4
colnames(df) = colnames(df) %>% str_replace('#', '')
topics = colnames(df)[5:9]


df$basic = as.numeric(df$basic)
df$childhood = as.numeric(df$childhood)
df$hometown = as.numeric(df$hometown)
df$densho = as.numeric(df$densho)
df$zokushin = as.numeric(df$zokushin)
df[is.na(df)] = 0

# 各話題のTFIDF
# 共起分析

df[topics]
df %>% group_by(group) %>% summarise(
    basic = sum(basic),
    childhood = sum(childhood),
    hometown = sum(hometown),
    densho = sum(densho),
    zokushin = sum(zokushin),
)

df[topics] %>% rowSums() %>% table()


# 話題ごとにまとめてdata.frame
# 
for(i in 1:5){
    if(i == 1){
        df_topic = df[df[topics[i]] == 1, ]
        df_topic$topic = topics[i]
    }else{
        x = df[df[topics[i]] == 1, ]
        x$topic = topics[i]
        df_topic = rbind(df_topic, x)
    }
}
df_topic = as.data.frame(df_topic)
df_topic %>% colnames()
agg_topic = df_topic %>% group_by(topic) %>% summarise(text = paste0(content, collapse=''))
agg_topic = as.data.frame(agg_topic)
agg_topic %>% dim()
count_noun_topic = docMatrixDF(agg_topic[, 'text'], pos=c('名詞'))
count_noun_topic

topics
remove_stopwords(count_noun_topic, stopwords, symbols)[, 'ROW.1'] %>% 
        sort() %>%
        tail(30) %>% barplot(horiz=T, las=2, main=character(i), cex.names=1.0)
remove_stopwords(count_noun_topic, stopwords, symbols)[, 'ROW.2'] %>% 
        sort() %>%
        tail(30) %>% barplot(horiz=T, las=2, main=character(i), cex.names=1.0)
remove_stopwords(count_noun_topic, stopwords, symbols)[, 'ROW.3'] %>% 
        sort() %>%
        tail(30) %>% barplot(horiz=T, las=2, main=character(i), cex.names=1.0)
remove_stopwords(count_noun_topic, stopwords, symbols)[, 'ROW.4'] %>% 
        sort() %>%
        tail(30) %>% barplot(horiz=T, las=2, main=character(i), cex.names=1.0)
remove_stopwords(count_noun_topic, stopwords, symbols)[, 'ROW.5'] %>% 
        sort() %>%
        tail(30) %>% barplot(horiz=T, las=2, main=character(i), cex.names=1.0)

tfidf = tf(count_noun_topic) * idf(count_noun_topic)

par(mfrow=c(1, 5)) # 1行5列
for(i in 1:5){
    remove_stopwords(tfidf, stopwords, symbols)[, i] %>%
        sort() %>%
        tail(30) %>% barplot(horiz=T, las=2, main=character(i), cex.names=1.5)
}
par(mfrow=c(1, 1)) 


# 共起分析をする
topics
df_topic %>% colnames()

res = map(df_topic[df_topic$basic == 1, 'content'], get_cooc, pos=c('名詞'), stopwords=stopwords,
          regex=symbols, with_pos=T) %>% unlist() %>% table()
d = parse_cooc(names(res), as.vector(res))
net = d %>% filter(freq==20)
net %>% graph_from_data_frame() %>% as.undirected() %>% tkplot(vertex.color='SkyBlue', vertex.size=22)

show_cooc = function(column, min_freq){
    res = map(df_topic[df_topic[, column] == 1, 'content'], get_cooc, pos=c('名詞'), stopwords=stopwords,
              regex=symbols, with_pos=T) %>% unlist() %>% table()
    d = parse_cooc(names(res), as.vector(res))
    net = d %>% filter(freq==min_freq)
    net %>% graph_from_data_frame() %>% as.undirected() %>% tkplot(vertex.color='SkyBlue', vertex.size=22)
}

show_cooc('basic', 15)
show_cooc('childhood', 20)
show_cooc('hometown', 20)
show_cooc('densho', 20)
show_cooc('zokushin', 20)


