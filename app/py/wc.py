from wordcloud import WordCloud
import sys
import pandas as pd
import re
import matplotlib.pyplot as plt

from nltk import word_tokenize 
from nltk.util import ngrams
from nltk.corpus import stopwords
from sklearn.feature_extraction.text import CountVectorizer

series             = sys.argv[1]
words_freq_file    = sys.argv[2]
wordcloud_fig_file = sys.argv[3]
language           = sys.argv[4]

if language == "spanish":
    # print("spanish")
    stop_spanish = stopwords.words('spanish')
    #print(sorted(stop_spanish))
    keep=["no","sin","ni","poco","muy","mucho","muchos","contra","más"]
    new_stop = [stop_spanish for stop_spanish in stop_spanish if stop_spanish not in keep]
    list2 = ["través"]
    new_stop.extend(list2)
else :
    stop_eng = stopwords.words('english')
    keep=["no","not","without","nor","little","very","much","against","more"]
    new_stop = [stop_eng for stop_eng in stop_eng if stop_eng not in keep]
    
text = pd.read_csv(series, index_col = 0, squeeze = True, nrows=500)

big_list=[]
for line in text:
    word_tokens = word_tokenize(line)
    filtered_sentence = []
    for w in word_tokens:
        if w not in new_stop:
            filtered_sentence.append(w)
    
    filtered_sentence = [' '.join(re.split("[ .,;:!?‘’``''@#$%^_&*()<>{}~/\n\t\\\-]", word)) for word in filtered_sentence]
    filtered_sentence = " ".join(filtered_sentence)
    token = word_tokenize(filtered_sentence)
    bigram = list(ngrams(token, 3))
    big_list.append(bigram)

flat_list = [item for sublist in big_list for item in sublist]
dictionary2 = [' '.join(tup) for tup in flat_list]

#Using count vectoriser to view the frequency of bigrams
vectorizer = CountVectorizer(ngram_range=(3, 3))
bag_of_words = vectorizer.fit_transform(dictionary2)
# vectorizer.vocabulary_
sum_words = bag_of_words.sum(axis=0) 
words_freq = [(word, sum_words[0, idx]) for word, idx in vectorizer.vocabulary_.items()]
words_freq =sorted(words_freq, key = lambda x: x[1], reverse=True)
pd.DataFrame(words_freq, columns=["trigram","frequency"]).to_csv(words_freq_file, index=False)
#Generating wordcloud and saving as jpg image
words_dict = dict(words_freq)
WC_height = 1000
WC_width = 1500
WC_max_words = 200
wordCloud = WordCloud(background_color='white',max_words=WC_max_words, height=WC_height, width=WC_width,stopwords=new_stop)
wordCloud.generate_from_frequencies(words_dict)
plt.title('Most frequently occurring bigrams connected by same colour and font size')
plt.imshow(wordCloud, interpolation='bilinear')
plt.axis("off")

wordCloud.to_file(wordcloud_fig_file)
