stopwords_es <- read.csv("https://bitsandbricks.github.io/data/stopwords_es.csv",
                         stringsAsFactors = FALSE)
stopwords_es$lang <- "es"
stopwords_en <- data.frame(STOPWORD = unique(c('i', 'me', 'my', 'myself', 'we', 'our', 'ours',
                                               'ourselves', 'you', "you're", "you've", "you'll",
                                               "you'd", 'your', 'yours', 'yourself', 'yourselves',
                                               'he', 'him', 'his', 'himself', 'she', "she's", 'her',
                                               'hers', 'herself', 'it', "it's", 'its', 'itself', 'they',
                                               'them', 'their', 'theirs', 'themselves', 'what', 'which',
                                               'who', 'whom', 'this', 'that', "that'll", 'these', 'those',
                                               'am', 'is', 'are', 'was', 'were', 'be', 'been', 'being', 'have',
                                               'has', 'had', 'having', 'do', 'does', 'did', 'doing', 'a', 'an',
                                               'the', 'and', 'but', 'if', 'or', 'because', 'as', 'until',
                                               'while', 'of', 'at', 'by', 'for', 'with', 'about', 'against',
                                               'between', 'into', 'through', 'during', 'before', 'after', 'above',
                                               'below', 'to', 'from', 'up', 'down', 'in', 'out', 'on', 'off',
                                               'over', 'under', 'again', 'further', 'then', 'once', 'here',
                                               'there', 'when', 'where', 'why', 'how', 'all', 'any', 'both',
                                               'each', 'few', 'more', 'most', 'other', 'some', 'such',
                                               'nor', 'not', 'only', 'own', 'same', 'so', 'than', 'too',
                                               'very', 's', 't', 'can', 'will', 'just', 'don', "don't",
                                               'should', "should've", 'now', 'd', 'll', 'm', 'o', 're',
                                               've', 'y', 'ain', 'aren', "aren't", 'couldn', "couldn't",
                                               'didn', "didn't", 'doesn', "doesn't", 'hadn', "hadn't", 'hasn',
                                               "hasn't", 'haven', "haven't", 'isn', "isn't", 'ma', 'mightn',
                                               "mightn't", 'mustn', "mustn't", 'needn', "needn't", 'shan',
                                               "shan't", 'shouldn', "shouldn't", 'wasn', "wasn't", 'weren',
                                               "weren't", 'won', "won't", 'wouldn', "wouldn't",
                                               'those', 'on', 'own', '’ve', 'yourselves', 'around', 'between', 'four', 'been', 'alone', 'off', 'am', 'then', 'other', 'can', 'regarding', 'hereafter', 'front', 'too', 'used', 'wherein', '‘ll', 'doing', 'everything', 'up', 'onto', 'never', 'either', 'how', 'before', 'anyway', 'since', 'through', 'amount', 'now', 'he', 'was', 'have', 'into', 'because', 'not', 'therefore', 'they', 'n’t', 'even', 'whom', 'it', 'see', 'somewhere', 'thereupon', 'nothing', 'whereas', 'much', 'whenever', 'seem', 'until', 'whereby', 'at', 'also', 'some', 'last', 'than', 'get', 'already', 'our', 'once', 'will', 'noone', "'m", 'that', 'what', 'thus', 'no', 'myself', 'out', 'next', 'whatever', 'although', 'though', 'which', 'would', 'therein', 'nor', 'somehow', 'whereupon', 'besides', 'whoever', 'ourselves', 'few', 'did', 'without', 'third', 'anything', 'twelve', 'against', 'while', 'twenty', 'if', 'however', 'herself', 'when', 'may', 'ours', 'six', 'done', 'seems', 'else', 'call', 'perhaps', 'had', 'nevertheless', 'where', 'otherwise', 'still', 'within', 'its', 'for', 'together', 'elsewhere', 'throughout', 'of', 'others', 'show', '’s', 'anywhere', 'anyhow', 'as', 'are', 'the', 'hence', 'something', 'hereby', 'nowhere', 'latterly', 'say', 'does', 'neither', 'his', 'go', 'forty', 'put', 'their', 'by', 'namely', 'could', 'five', 'unless', 'itself', 'is', 'nine', 'whereafter', 'down', 'bottom', 'thereby', 'such', 'both', 'she', 'become', 'whole', 'who', 'yourself', 'every', 'thru', 'except', 'very', 'several', 'among', 'being', 'be', 'mine', 'further', 'n‘t', 'here', 'during', 'why', 'with', 'just', "'s", 'becomes', '’ll', 'about', 'a', 'using', 'seeming', "'d", "'ll", "'re", 'due', 'wherever', 'beforehand', 'fifty', 'becoming', 'might', 'amongst', 'my', 'empty', 'thence', 'thereafter', 'almost', 'least', 'someone', 'often', 'from', 'keep', 'him', 'or', '‘m', 'top', 'her', 'nobody', 'sometime', 'across', '‘s', '’re', 'hundred', 'only', 'via', 'name', 'eight', 'three', 'back', 'to', 'all', 'became', 'move', 'me', 'we', 'formerly', 'so', 'i', 'whence', 'under', 'always', 'himself', 'in', 'herein', 'more', 'after', 'themselves', 'you', 'above', 'sixty', 'them', 'your', 'made', 'indeed', 'most', 'everywhere', 'fifteen', 'but', 'must', 'along', 'beside', 'hers', 'side', 'former', 'anyone', 'full', 'has', 'yours', 'whose', 'behind', 'please', 'ten', 'seemed', 'sometimes', 'should', 'over', 'take', 'each', 'same', 'rather', 'really', 'latter', 'and', 'ca', 'hereupon', 'part', 'per', 'eleven', 'ever', '‘re', 'enough', "n't", 'again', '‘d', 'us', 'yet', 'moreover', 'mostly', 'one', 'meanwhile', 'whither', 'there', 'toward', '’m', "'ve", '’d', 'give', 'do', 'an', 'quite', 'these', 'everyone', 'towards', 'this', 'cannot', 'afterwards', 'beyond', 'make', 'were', 'whether', 'well', 'another', 'below', 'first', 'upon', 'any', 'none', 'many', 'serious', 'various', 're', 'two', 'less', '‘ve')
)
)
stopwords_en$lang <- "en"

usethis::use_data(stopwords_es, overwrite = TRUE)
usethis::use_data(stopwords_en, overwrite = TRUE)
