# パッケージのインストール
install.packages('openxlsx') # excel読み込み用
install.packages('igraph') # グラフ表示用

# パッケージの読み込み
library('openxlsx')
library('tidyverse')
library('RMeCab')
library('igraph')

# 関数群の読み込み
source('functions.r', encoding='utf8') 

# ファイルの読み込み
# data/interview/ 配下にある想定
files = c(
    './data/interview/応用Ⅰ整理データ_1班.xlsx',
    './data/interview/応用Ⅰ整理データ_2班.xlsx',
    './data/interview/応用Ⅰ整理データ_3班.xlsx',
    './data/interview/応用Ⅰ整理データ_4班.xlsx',
    './data/interview/応用Ⅰ整理データ_5班.xlsx'
)

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
table(df$group, df$person)

# データセットの成形
# NAを0に変える
df[is.na(df)] = 0


# 学生のデータを除く
df %>% filter(str_detect(person, '学')) %>% select(person) %>% table()
counted = df %>% filter(!str_detect(person, '学')) %>% select(person) %>% table()
counted

# 頻度が少ないものも除く
excluded = counted[counted < 10] %>% names()


df = df %>% filter(!str_detect(person, '学'))
df = df %>% filter(! person %in% excluded)


df$person %>% table()

# 課題1 
# クリーニング
# 括弧の部分を削る

df$content %>% str_extract_all('[(（][^)）]+[)）]')
df$content = df$content %>% str_replace_all('[(（][^)）]+[)）]', '')

# 謎のカタカナ
df$content = df$content %>% str_replace('[ァ-ヴ]+$', '')


# 表記ゆれ
# 子ども -> 子供
# あたし -> 私
df$content = df$content %>% str_replace_all('あたし', '私')
df$content = df$content %>% str_replace_all('わたし', '私')
df$content = df$content %>% str_replace_all('子供', '子ども')
df$content = df$content %>% str_replace_all('みなさん', '皆さん')
df$content = df$content %>% str_replace_all('笹巻き', '笹巻')

# ストップワード
stopwords = c('ん', 'こと', 'それ', 'の', 'よう', 'ところ', 'さん',
              '何', 'ぉ', 'ー', 'とき', 'もの', 'たち', 'あれ', 'なか', 'とき',
              'そこ', '中', '話', 'なん', 'みたい', 'これ', 'そう', 'わけ', 
              '方', '時', '日', 'ら', 'とこ', '笑', 'ここ', '感じ', '年', '大', '回', '相槌', '上')
# シンボル
symbols = '^[0-9０-９()（）,.ー\\-∼～~〜一十、。…!！?？]+$'


#  上位頻出語 
#　いったんグループごとにまとめる
groups = df %>% group_by(group) %>% summarise(text = paste0(content, collapse=''))
groups = as.data.frame(groups)  # data.frameに変換

count_noun = docMatrixDF(groups[,'text'], pos=c('名詞'))
count_noun %>% dim()

# グループをまとめたもの
freq_noun = count_noun %>% rowSums()
freq_noun %>% sort() %>% tail(30) %>% barplot(horiz=T, las=1, main='Top 30 nouns', xlab='Frequency', cex.names=0.5)



freq_noun[(! names(freq_noun) %in% stopwords) & !str_detect(names(freq_noun), symbols)] %>% sort() %>% tail(30) %>% barplot(horiz=T, las=1, main='Top 30 nouns', xlab='Frequency', cex.names=0.5)
freq_noun[(names(freq_noun) %in% stopwords) | str_detect(names(freq_noun), symbols)]

# 課題2
# グループごとの頻出語

count_noun[(! rownames(count_noun) %in% stopwords) & (!str_detect(rownames(count_noun), symbols)), 'ROW.1'] %>% sort() %>% tail(30) %>% barplot(horiz=T, las=1, main='Top 30 nouns', xlab='Frequency', cex.names=0.5)

# 関数化しておく
remove_stopwords = function(df, stopwords, symbols){
    return(df[(! rownames(df) %in% stopwords) & (!str_detect(rownames(df), symbols)),]) 
}
remove_stopwords(count_noun, stopwords, symbols)[,'ROW.1'] %>% sort() %>% tail(30) %>% barplot(horiz=T, las=1, main='Top 30 nouns', xlab='Frequency', cex.names=0.5)
remove_stopwords(count_noun, stopwords, symbols)[,'ROW.2'] %>% sort() %>% tail(30) %>% barplot(horiz=T, las=1, main='Top 30 nouns', xlab='Frequency', cex.names=0.5)
remove_stopwords(count_noun, stopwords, symbols)[,'ROW.3'] %>% sort() %>% tail(30) %>% barplot(horiz=T, las=1, main='Top 30 nouns', xlab='Frequency', cex.names=0.5)
remove_stopwords(count_noun, stopwords, symbols)[,'ROW.4'] %>% sort() %>% tail(30) %>% barplot(horiz=T, las=1, main='Top 30 nouns', xlab='Frequency', cex.names=0.5)
remove_stopwords(count_noun, stopwords, symbols)[,'ROW.5'] %>% sort() %>% tail(30) %>% barplot(horiz=T, las=1, main='Top 30 nouns', xlab='Frequency', cex.names=0.5)

# 課題3 
# TFIDFを求める
# functions.rにtf(), idf()それぞれ既にある

tfidf = tf(count_noun) * idf(count_noun) 

# TFIDFを一枚のplotにおさめる
par(mfrow=c(1,5)) 
for(i in 1:5){
    tfidf[(!rownames(tfidf) %in% stopwords) & (!str_detect(rownames(tfidf), symbols)), i] %>% 
    sort() %>% 
    tail(30)  %>%  barplot(horiz=T, las=2, main=character(i))
}

par(mfrow=c(1,1)) # 設定をもとに戻す

# TFIDFの結果のなかから見てみる
df[str_detect(df$content, 'ぎゃ'), 'content'] # 擬音→除外
df[str_detect(df$content, 'とり'), 'content'] # 方言 ~しとる の活用
df[str_detect(df$content, 'ろう'), 'content'] # 分析ミス
df[str_detect(df$content, 'ばん'), 'content'] # 分析ミス


# 課題4
# 列名を変える(#だとコメント扱いになってしまうので)
colnames(df) = colnames(df) %>% str_replace('#', '')
topics = colnames(df)[5:9]

# 文字列として読み込まれているので数値データに変換
df$basic

df$basic = as.numeric(df$basic)
df$childhood = as.numeric(df$childhood)
df$hometown = as.numeric(df$hometown)
df$densho = as.numeric(df$densho)
df$zokushin = as.numeric(df$zokushin)
df[is.na(df)] = 0


df[topics]
df %>% group_by(group) %>% summarise(
    basic = sum(basic), childhood=sum(childhood),
    hometown=sum(hometown), densho=sum(densho), zokushin=sum(zokushin))

# 複数のタグが付いている発言
df[topics] %>% rowSums() %>% table()

# トピックごとにまとめたデータフレームを作る
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

# 各トピックでまとめたデータ
agg_topic = df_topic %>% group_by(topic) %>% summarise(text = paste0(content, collapse=''))
agg_topic = as.data.frame(agg_topic)  # data.frameに変換

count_noun_topic = docMatrixDF(agg_topic[,'text'], pos=c('名詞'))

# 各トピック頻出語
remove_stopwords(count_noun_topic, stopwords, symbols)[, 'ROW.1'] %>% sort() %>% tail(30) %>% barplot(horiz=T, las=1, main='Top 30 nouns', xlab='Frequency', cex.names=0.5)
remove_stopwords(count_noun_topic, stopwords, symbols)[, 'ROW.2'] %>% sort() %>% tail(30) %>% barplot(horiz=T, las=1, main='Top 30 nouns', xlab='Frequency', cex.names=0.5)
remove_stopwords(count_noun_topic, stopwords, symbols)[, 'ROW.3'] %>% sort() %>% tail(30) %>% barplot(horiz=T, las=1, main='Top 30 nouns', xlab='Frequency', cex.names=0.5)
remove_stopwords(count_noun_topic, stopwords, symbols)[, 'ROW.4'] %>% sort() %>% tail(30) %>% barplot(horiz=T, las=1, main='Top 30 nouns', xlab='Frequency', cex.names=0.5)
remove_stopwords(count_noun_topic, stopwords, symbols)[, 'ROW.5'] %>% sort() %>% tail(30) %>% barplot(horiz=T, las=1, main='Top 30 nouns', xlab='Frequency', cex.names=0.5)

tfidf = tf(count_noun_topic) * idf(count_noun_topic) 

# TFIDFを一枚のplotにおさめる
par(mfrow=c(1,5)) 
for(i in 1:5){
    tfidf[(!rownames(tfidf) %in% stopwords) & (!str_detect(rownames(tfidf), symbols)), i] %>% 
    sort() %>% 
    tail(30)  %>%  barplot(horiz=T, las=2, main=character(i))
}

par(mfrow=c(1,1)) # 設定をもとに戻す


# 共起分析をする
# 話題ごとに共起したものを可視化
topics
df_topic %>% colnames()

res = map(df_topic[df_topic$basic == 1,'content'], get_cooc, pos=c('名詞'), stopwords=stopwords, regex=symbols, with_pos=T) %>% unlist() %>% table()
d = parse_cooc(names(res), as.vector(res))
net = d %>% 
    filter(freq > 40)
net %>% dim()
net %>% graph_from_data_frame() %>% as.undirected() %>% tkplot(vertex.color='SkyBlue', vertex.size=22)

show_cooc = function(column, min_freq){
    res = map(df_topic[df_topic[, column] == 1, 'content'], get_cooc, pos=c('名詞'), stopwords=stopwords, regex=symbols, with_pos=T) %>% unlist() %>% table()
    d = parse_cooc(names(res), as.vector(res))
    net = d %>% 
        filter(freq > min_freq)
    net %>% dim()
    net %>% graph_from_data_frame() %>% as.undirected() %>% tkplot(vertex.color='SkyBlue', vertex.size=22)
}

show_cooc('childhood', 40)
show_cooc('hometown', 40)
show_cooc('densho', 40)
show_cooc('zokushin', 40)

