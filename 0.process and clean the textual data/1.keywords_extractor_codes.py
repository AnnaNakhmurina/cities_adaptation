#!/usr/bin/env python
# coding: utf-8

# In[1]:


import re
import os
import sys 
import ast
import tika
import glob
import nltk
import time
import math
import PyPDF2
import pickle
import string
import tabula
import pikepdf
import pathlib
import fnmatch
import itertools
import statistics
import pdfplumber
import collections
import pytesseract

tika.initVM()
import numpy as np
import pandas as pd

from os import walk
from PIL import Image
from tqdm import tqdm
from tika import parser
from pathlib import Path
from bisect import bisect

from statistics import mean 
from nltk.util import ngrams
from itertools import islice
from functools import reduce
from collections import Counter
from nltk.corpus import webtext
from nltk.corpus import stopwords
from PyPDF2 import PdfFileReader
from pdf2image import convert_from_path
from wordcloud import WordCloud, STOPWORDS
from nltk.stem.snowball import SnowballStemmer 
from nltk.tokenize import sent_tokenize, word_tokenize

import warnings
warnings.filterwarnings("ignore")


# In[2]:


def decrypt_docs(path, path_decrypted):
    
    """
    Create unencrypted version of documents

    Parameters:
    ----------
    path : local directory where the text documents are stored
    path_decrypted : local directory to save decrypted documents

    """
    
    # extract all the docs name
    files = []
    for root, dirnames, filenames in os.walk(path):
        for filename in fnmatch.filter(filenames, "*.pdf"):
            files.append(os.path.join(root, filename))

    # extract cafr, bonds and budget documents
    pattern = re.compile(r".*CAFR.*.pdf|.*Budget.*.pdf|.*Bonds.*.pdf")
    files = [x for x in files if re.match(pattern, x)]

    for f in tqdm(files):

        pdf = pikepdf.open(f)
        file_name = f.split("/")[1:]
        new_path = path_decrypted + "/" + "/".join(file_name[:-1])
        
        if not os.path.exists(new_path):
            os.makedirs(new_path)
            
        pdf.save(os.path.join(path_decrypted + "/" + "/".join(file_name)))


# In[3]:


def aggregate_text_docs_pdfplumber(path):
    
    """
    Aggregate all text files into pandas dataframe

    Parameters:
    ----------
    path : local directory where the text documents are stored

    Returns:
    -------
    docs_df : pandas dataframe of all text documents (columns including city_index, city_name, year, docs_type, docs_name and text)
    """
    
    docs_name, docs = [], []
    
    # extract all the docs name
    files = []
    for root, dirnames, filenames in os.walk(path):
        for filename in fnmatch.filter(filenames, "*.pdf"):
            files.append(os.path.join(root, filename))
            

    # extract cafr, bonds and budget documents
    pattern = re.compile(r".*CAFR.*.pdf|.*Budget.*.pdf|.*Bonds.*.pdf")
    files = [x for x in files if re.match(pattern, x)]
    
    # parse pdf
    for f in tqdm(files):
        
        try:
            
            with pdfplumber.open(f) as pdf:

                all_text = ""

                for pages in pdf.pages:
                    single_page_text = pages.dedupe_chars(tolerance = 1).extract_text()

                    if single_page_text is not None:
                        all_text = all_text + "\n" + single_page_text

                docs_name.append(f)
                docs.append(all_text)
        
        except:
            
            continue
    
    docs_df = pd.DataFrame({"docs_name": docs_name, "text": docs})
    
    # add city_index column
    docs_df["city_index"] = docs_df["docs_name"].map(lambda x: int(re.findall(r"\d+", x)[0]))
    
    # add city_name column
    docs_df["city_name"] = docs_df["docs_name"].str.extract(r"\.([^/]*)/")
    docs_df["city_name"] = docs_df["city_name"].map(lambda x: x.strip())
    
    # add year column
    docs_df["year"] = docs_df["docs_name"].map(lambda x: int(re.findall(r"\d+", x)[1]))

    
    # add docs_type column
    docs_df["docs_type"] = np.where(docs_df["docs_name"].str.match(r".*CAFR.*.pdf"), "CAFR", 
                                    (np.where(docs_df["docs_name"].str.match(r".*Budget.*.pdf"), "Budget", "Bonds")))

    
    # docs_name column
    docs_df["docs_name"] = docs_df["docs_name"].map(lambda x: x.split("/")[-1])
    
    
    # rearrange and sort
    docs_df = docs_df[["city_index", "city_name", "year", "docs_type", "docs_name", "text"]].sort_values(by = ["city_index", "year", "docs_type", "docs_name"]).reset_index(drop = True)
    
    return docs_df


# In[4]:


def aggregate_text_docs_tika(path):
    
    """
    Aggregate all text files into pandas dataframe

    Parameters:
    ----------
    path : local directory where the text documents are stored

    Returns:
    -------
    docs_df : pandas dataframe of all text documents (columns including city_index, city_name, year, docs_type, docs_name and text)
    """
    
    docs_name, docs = [], []
    
    # extract all the docs name
    files = []
    for root, dirnames, filenames in os.walk(path):
        for filename in fnmatch.filter(filenames, "*.pdf"):
            files.append(os.path.join(root, filename))
            

    # extract cafr, bonds and budget documents
    pattern = re.compile(r".*CAFR.*.pdf|.*Budget.*.pdf|.*Bonds.*.pdf")
    files = [x for x in files if re.match(pattern, x)]
    
    # parse pdf
    for f in tqdm(files):
        try:
            parsed_pdf = parser.from_file(f, requestOptions = {"timeout": 300})
            all_text = parsed_pdf["content"]
            docs_name.append(f)
            docs.append(all_text)
        except:
            print("timeout: " + f)
            continue
    
    docs_df = pd.DataFrame({"docs_name": docs_name, "text": docs})
    
    # add city_index column
    docs_df["city_index"] = docs_df["docs_name"].map(lambda x: int(re.findall(r"\d+", x)[0]))
    
    # add city_name column
    docs_df["city_name"] = docs_df["docs_name"].str.extract(r"\.([^/]*)/")
    docs_df["city_name"] = docs_df["city_name"].map(lambda x: x.strip())
    
    # add year column
    docs_df["year"] = docs_df["docs_name"].map(lambda x: int(re.findall(r"\d+", x)[1]))

    
    # add docs_type column
    docs_df["docs_type"] = np.where(docs_df["docs_name"].str.match(r".*CAFR.*.pdf"), "CAFR", 
                                    (np.where(docs_df["docs_name"].str.match(r".*Budget.*.pdf"), "Budget", "Bonds")))

    
    # docs_name column
    docs_df["docs_name"] = docs_df["docs_name"].map(lambda x: x.split("/")[-1])
    
    
    # rearrange and sort
    docs_df = docs_df[["city_index", "city_name", "year", "docs_type", "docs_name", "text"]].sort_values(by = ["city_index", "year", "docs_type", "docs_name"]).reset_index(drop = True)
    
    return docs_df


# In[5]:


def sentence_level_tokenizer(docs_df):
    
    """
    Perform sentence and word tokenization for the raw text data
    
    Parameters:
    ----------
    docs_df : pandas dataframe that contains documents information and raw text data

    Returns:
    -------
    docs_df_sentence : pandas dataframe that contains documents information and word tokens for each sentence
    """      
    
    docs_df_sentence = docs_df.copy()
    
    tqdm.pandas() 
    
    # tokenize sentence
    docs_df_sentence["text_tokens"] = docs_df_sentence["text"].progress_map(lambda x: sent_tokenize(x))
    
    # [Added] Bullet Points Partition
    def sep_bullets(x):
        result = []
        for line in x:
            result += line.split("•")
        return result
    
    # Separate Bullets:
    docs_df_sentence["text_tokens"] = docs_df_sentence["text_tokens"].progress_map(sep_bullets)
    # [End] 
    
    # convert text to lowercase
    docs_df_sentence["text_tokens"] = docs_df_sentence["text_tokens"].progress_map(lambda x: [i.lower() for i in x])
    
    # remove special characters and numbers
    docs_df_sentence["text_tokens"] = docs_df_sentence["text_tokens"].progress_map(lambda x: [re.sub("[^A-Za-z ]+", " ", i) for i in x])
    
    # remove all spaces with length more than 1
    docs_df_sentence["text_tokens"] = docs_df_sentence["text_tokens"].progress_map(lambda x: [re.sub("[  ]+", " ", i) for i in x])

    # word tokenize every sentence
    docs_df_sentence["text_tokens"] = docs_df_sentence["text_tokens"].progress_map(lambda x: [word_tokenize(i) for i in x])
    
    # create a Snowball stemmer 
    snow_stemmer = SnowballStemmer("english")
    
    # perform stemming on the tokenized words 
    docs_df_sentence["text_tokens_stemmed"] = docs_df_sentence["text_tokens"].progress_map(lambda x: [[snow_stemmer.stem(w) for w in i] for i in x])
    
    # drop text column
    docs_df_sentence = docs_df_sentence.drop(columns = ["text"])
    
    return docs_df_sentence


# In[6]:


def preprocess(docs_df_sentence):
    
    """
    Perform sentence and word tokenization for the raw text data
    
    Parameters:
    ----------
    docs_df : pandas dataframe that contains documents information and raw text data

    Returns:
    -------
    docs_df_sentence : pandas dataframe that contains documents information and word tokens for each sentence
    """         
    tqdm.pandas() 
    
    # [Added] Part 1: Helper Function for Combining digit chunks and Separate by Bullet Points
    # tokenize sentence
    docs_df_sentence["text_tokens"] = docs_df_sentence["text"].progress_map(lambda x: sent_tokenize(x))
    
    def sep_bullets(x):
        result = []
        for line in x:
            result += line.split("•")
        return result

    # Combine digits and replace chunk of digits with 0
    def combine_digit(x):
        x = re.sub("\d+([^A-Za-z0-9\s]\d+)+",'0',x)
        x = re.sub("\d+",'0',x)
        return x
    
    # Separate Bullets:
    docs_df_sentence["text_tokens"] = docs_df_sentence["text_tokens"].progress_map(sep_bullets)
    # [End] Part 1
    
    # convert text to lowercase
    docs_df_sentence["text_tokens"] = docs_df_sentence["text_tokens"].progress_map(lambda x: [i.lower() for i in x])

    # [Added] Part 2: Cleaning Process Before Combining Chunks and Calculating Digits 
    # remove all next line 
    docs_df_sentence["text_tokens_helper"] = docs_df_sentence["text_tokens"].progress_map(lambda x: [re.sub("\s+", " ", i) for i in x])
    # [End] Part 2 
    
    # remove special characters and numbers
    docs_df_sentence["text_tokens"] = docs_df_sentence["text_tokens"].progress_map(lambda x: [re.sub("[^a-z()]+", " ", i) for i in x])
    
    # [Added] Part 3: Cleaning Process and Combining Chunks
    # Combine chunk of digits
    docs_df_sentence["text_tokens_digit"] = docs_df_sentence["text_tokens_helper"].progress_map(lambda x: [combine_digit(i) for i in x])
    # [End] Part 3

    # word tokenize every sentence
    docs_df_sentence["text_tokens"] = docs_df_sentence["text_tokens"].progress_map(lambda x: [word_tokenize(i) for i in x])


# In[7]:


def article_summary(docs_df_sentence):
    digit_pc_list = []
    for index in range(len(docs_df_sentence)):
        sent = docs_df_sentence.iloc[index]
        a_id = sent['article_id']
        for i,k in enumerate(sent['text_tokens']): 
            alphanum_cnt = 0
            digit_chunk_cnt = 0
            for s in k:
                if re.match('[a-z]',s):
                    alphanum_cnt += 1
                    break
            for s in sent['text_tokens_digit'][i]:
                if s == '0':
                    digit_chunk_cnt +=1

            if alphanum_cnt != 0 :
                temp_dict = dict()
                temp_dict['article_id'] = a_id
                temp_dict['sent_id'] = i
                temp_dict['sent_length'] = len(sent['text_tokens'][i])
                temp_dict['digit_chunk_ratio'] = digit_chunk_cnt/len(sent['text_tokens'][i])
                digit_pc_list.append(temp_dict)
    digit_pc_df = pd.DataFrame(digit_pc_list)
    return digit_pc_df


# In[8]:


def keywords_stemmer(keywords): 
    
    """
    Stemming the keywords, if a keyword is not a unigram, stemming each unigram separately and then combine them together

    Parameters:
    ----------
    keywords : pandas dataframe of all the keywords
    
    Returns:
    -------
    keywords_stem : pandas dataframe of all the keywords with column keywords_stem added
    """
    
    #the stemmer requires a language parameter 
    snow_stemmer = SnowballStemmer(language = "english") 

    #stem's of each word 
    stem_words = [] 
    
    for i in range(len(keywords)):
        
        if keywords.iloc[i]["Stemmed"] == 1:
            
            w = keywords.iloc[i]["Keyword"]
            unigram = []
            split_w = w.split()


            for s in split_w:
                x = snow_stemmer.stem(s) 
                unigram.append(x) 

            unigram = " ".join(unigram)
            stem_words.append(unigram)
            
        else:
            
            stem_words.append(keywords.iloc[i]["Keyword"])
            
    keywords["keywords_stem"] = stem_words
    keywords = keywords.fillna("")  
    
    return keywords


# In[9]:


def keywords_indices(stemmed_keyword, exclude_before, exclude_after, token_stem_text):
   
    key = stemmed_keyword.split()
    
    if len(key) == 1:
        
        indices = [[i for i, x in enumerate(t) if x == key[0]] for t in token_stem_text]
        before = exclude_before.split(", ")
        after = exclude_after.split(", ")
        ind = []
        
        for i in range(len(indices)):
            inde = []
            if indices[i] != []:
                for j in indices[i]:
                    
                    if j == 0 and (j + 1) < len(token_stem_text[i]):
                        if token_stem_text[i][j + 1] not in before:
                            inde.append(j)
                            
                    elif j >= 1 and (j + 1) == len(token_stem_text[i]):
                        if token_stem_text[i][j - 1] not in after:
                            inde.append(j)
                            
                    elif len(token_stem_text[i]) == 1:
                        inde.append(j)
                        
                    elif j >= 1 and (j + 1) < len(token_stem_text[i]):  
                        if token_stem_text[i][j + 1] not in before and token_stem_text[i][j - 1] not in after:
                            inde.append(j)
            
            ind.append(inde)

        
    elif len(key) == 2:
        ind = []
        indices = [[i for i, x in enumerate(t) if x == key[0]] for t in token_stem_text]
        for i in range(len(indices)):
            inde = [] 
            if indices[i] != []:
                for j in indices[i]:
                    if (j + 1) < len(token_stem_text[i]):
                        if token_stem_text[i][j + 1] == key[1]:
                            inde.append(j)
            
            ind.append(inde)
            
    elif len(key) == 3:
        ind = []
        indices = [[i for i, x in enumerate(t) if x == key[0]] for t in token_stem_text]
        for i in range(len(indices)):
            inde = [] 
            if indices[i] != []:
                for j in indices[i]:
                    if (j + 2) < len(token_stem_text[i]):
                        if token_stem_text[i][j + 1] == key[1]:
                            if token_stem_text[i][j + 2] == key[2]:
                                inde.append(j)
            
            ind.append(inde)    
            

    elif len(key) == 4:
        ind = []
        indices = [[i for i, x in enumerate(t) if x == key[0]] for t in token_stem_text]
        for i in range(len(indices)):
            inde = [] 
            if indices[i] != []:
                for j in indices[i]:
                    if (j + 3) < len(token_stem_text[i]):
                        if token_stem_text[i][j + 1] == key[1]:
                            if token_stem_text[i][j + 2] == key[2]:
                                if token_stem_text[i][j + 3] == key[3]:
                                    inde.append(j)
            
            ind.append(inde)
    
    res = dict()
    
    for i in range(len(ind)):
        if ind[i] != []:
            res[i] = ind[i]
            
    return res


# In[10]:


def keywords_extractor(keywords_stem, docs_df_sentence):
    
    """
    Add columns of sentence indices for each keyword
    
    Parameters:
    ----------
    keywords_stem : pandas dataframe of keywords including a keywords_stem column
    docs_df_sentence : pandas dataframe that contains documents information and word tokens for each sentence before/after stemming

    Returns:
    -------
    docs_df_sentence : pandas dataframe after adding columns of indices for each keyword
    """  
    
    for i in tqdm(range(len(keywords_stem))):
        
        k = keywords_stem.iloc[i]["keywords_stem"]
        be = keywords_stem.iloc[i]["Exclude Before"]
        af = keywords_stem.iloc[i]["Exclude After"]
        st = keywords_stem.iloc[i]["Stemmed"]
        
        colname = re.sub(" ", "_", k)
        
        if st == 1:
            docs_df_sentence[colname] = docs_df_sentence["text_tokens_stemmed"].map(lambda x: keywords_indices(k, be, af, x))
    
        elif st == 0:
            docs_df_sentence[colname] = docs_df_sentence["text_tokens"].map(lambda x: keywords_indices(k, be, af, x))

    s2 = docs_df_sentence.shape[1]
    
    docs_df_sentence["number_of_sentences"] = docs_df_sentence["text_tokens_stemmed"].map(lambda x: len(x))
    docs_df_sentence["count"] = docs_df_sentence.apply(lambda x: [sum([len(o) for o in x[i].values()]) for i in range(7, s2)], axis = 1)
    docs_df_sentence["count"] = docs_df_sentence.apply(lambda x: [len([item for sublist in x["text_tokens_stemmed"] for item in sublist])] + x["count"], axis = 1)
    
    return docs_df_sentence


# In[11]:


def cumulative(lists):
    
    cu_list = []
    length = len(lists)
    cu_list = [sum(lists[0:x:1]) for x in range(0, length + 1)]
    
    return cu_list[1:]


# In[12]:


def flatten_sentence_index(docs_df_sentence):
    
    """
    Flatten sentence indices to word indices
    
    Parameters:
    ----------
    docs_df_sentence : pandas dataframe that contains documents information and columns of indices for each keyword

    Returns:
    -------
    docs_df_sentence_flatten : pandas dataframe after flattening sentence indices to word indices
    """  
    
    tqdm.pandas() 
    
    docs_df_sentence_index_flatten = docs_df_sentence.copy()
    
    docs_df_sentence_index_flatten["sentence_words_count"] = docs_df_sentence_index_flatten["text_tokens_stemmed"].progress_map(lambda x: [len(x[i]) for i in range(len(x))])
    docs_df_sentence_index_flatten["cum_sentence_words_count"] = docs_df_sentence_index_flatten["sentence_words_count"].progress_map(lambda x: cumulative(x))

    cols = docs_df_sentence_index_flatten.columns.tolist()[7:-4]
    
    for col in tqdm(cols):
        docs_df_sentence_index_flatten[col] = docs_df_sentence_index_flatten.apply(lambda x: [x["cum_sentence_words_count"][i[0]-1] + i[1] if i[0] >= 1 else i[1] for i in [[k, j] for k, l in x[col].items() for j in l]], axis = 1)

    docs_df_sentence_index_flatten["text_tokens_stemmed"] = docs_df_sentence_index_flatten["text_tokens_stemmed"].progress_map(lambda x: [item for sublist in x for item in sublist])
    
    return docs_df_sentence_index_flatten


# In[13]:


def counts_for_separate_keywords(docs_df_sentence_index_flatten):

    """
    Generate a table for the counts of each keyword
    
    Parameters:
    ----------
    docs_df_sentence_index_flatten : pandas dataframe after flattening sentence indices to word indices
    
    Returns:
    -------
    keywords_counts : pandas dataframe with the counts of each keyword
    """  
    
    keywords_counts = docs_df_sentence_index_flatten.copy()
    cols = docs_df_sentence_index_flatten.columns.tolist()[7:-4]
    
    for col in tqdm(cols):
        keywords_counts[col] = keywords_counts[col].map(lambda x: len(x))
        
    keywords_counts["number_of_words"] = keywords_counts["count"].map(lambda x: x[0])
    keywords_counts = keywords_counts.drop(columns = ["text_tokens", "text_tokens_stemmed", "sentence_words_count", "cum_sentence_words_count", "count"]).reset_index(drop = True)

    return keywords_counts


# In[14]:


def group_index(l, within_next_n):
    
    res, last = [[]], None
    for x in l:
        if last is None or abs(last - x) <= within_next_n:
            res[-1].append(x)
        else:
            res.append([x])
        last = x
        
    return res


# In[15]:


def add_group_count(docs_df_sentence_index_flatten, within_next_n, group_size):
    
    docs_df_sentence_group = docs_df_sentence_index_flatten.copy()
    
    # combine word indices of all the keywords
    docs_df_sentence_group["combine_indices"] = docs_df_sentence_group.iloc[:, 7:-4].apply(list, axis = 1)
    docs_df_sentence_group["combine_indices"] = docs_df_sentence_group["combine_indices"].map(lambda x: sorted(list(set([item for sublist in x for item in sublist]))))
    docs_df_sentence_group["combine_indices"] = docs_df_sentence_group["combine_indices"].map(lambda x: [i for i in x if i != 0])

    # convert all the word indices to sentence indices
    docs_df_sentence_group["sentence_indices"] = docs_df_sentence_group.apply(lambda x: [bisect(x["cum_sentence_words_count"], i) for i in x["combine_indices"]], axis = 1)

    
    # group sentence indices based on the distance requirement
    docs_df_sentence_group["group_sentence_indices"] = docs_df_sentence_group["sentence_indices"].map(lambda x: group_index(x, within_next_n))
    
    # size of each group: number of keywords within each group
    docs_df_sentence_group["group_size"] = docs_df_sentence_group["group_sentence_indices"].map(lambda x: [len(i) for i in x] if x != [[]] else [])
    
    # group word indices based on the distance requirement
    docs_df_sentence_group["temp"] = docs_df_sentence_group["combine_indices"].map(lambda x: iter(x))
    docs_df_sentence_group["group_word_indices"] = docs_df_sentence_group.apply(lambda x: [list(islice(iter(x["temp"]), elem)) for elem in x["group_size"]], axis = 1)
    docs_df_sentence_group = docs_df_sentence_group.drop(columns = ["temp"])

    
    # get the name indices of the keywords within each group
    docs_df_sentence_group["keyword_index"] =  docs_df_sentence_group.apply(lambda x: x.tolist()[7:-9], axis = 1)                      
    docs_df_sentence_group["keyword_name_indices"] = docs_df_sentence_group.apply(lambda x: [[[i for i, c in enumerate(x["keyword_index"]) if j in c][0] for j in k] for k in x["group_word_indices"]], axis = 1)
    docs_df_sentence_group = docs_df_sentence_group.drop(columns = ["keyword_index"])

    
    # sentence length
    docs_df_sentence_group["group_sentence_length"] = docs_df_sentence_group["group_sentence_indices"].map(lambda x: [max(i) - min(i) + 1 for i in x] if x != [[]] else [])
    
    # total number of groups
    docs_df_sentence_group["total_number_of_group"] = docs_df_sentence_group["group_sentence_indices"].map(lambda x: len(x) if x != [[]] else 0)
 
    # number of groups that meet the size requirement
    docs_df_sentence_group["number_of_groups_meet_requirement"] = docs_df_sentence_group["group_size"].map(lambda x: len([i for i in x if i >= group_size]) if x != [] else 0)
    
    # number of grouped keywords that meet the size requirement
    docs_df_sentence_group["number_of_keywords_meet_requirement"] = docs_df_sentence_group["group_size"].map(lambda x: sum([i for i in x if i >= group_size]))

    # group sentence indices for groups that meet the size requirement
    docs_df_sentence_group["group_sentence_indices_meet_requirement"] = docs_df_sentence_group.apply(lambda x: [x["group_sentence_indices"][i] for i in range(x["total_number_of_group"]) if x["group_size"][i] >= group_size], axis = 1)
 
    # sentence length for groups that meet the size requirement
    docs_df_sentence_group["group_sentence_length_meet_requirement"] = docs_df_sentence_group.apply(lambda x: [x["group_sentence_length"][i] for i in range(x["total_number_of_group"]) if x["group_size"][i] >= group_size], axis = 1)
 
    # sentence length sum for groups that meet the size requirement
    docs_df_sentence_group["group_sentence_length_sum_meet_requirement"] = docs_df_sentence_group["group_sentence_length_meet_requirement"].map(lambda x: sum(x))
 
    
    # total number of distinct words per group 
    docs_df_sentence_group["total_number_of_distinct_words"] = docs_df_sentence_group["keyword_name_indices"].map(lambda x: [i for i in x if len(i) >= group_size])
    docs_df_sentence_group["total_number_of_distinct_words"] = docs_df_sentence_group["total_number_of_distinct_words"].map(lambda x: sum([len(set(i)) for i in x]))

    # a group is defined as 2 distinct keywords within the given parameter within_next_n sentences
    docs_df_sentence_group["size_2_group"] = docs_df_sentence_group["keyword_name_indices"].map(lambda x: [i for i in x if len(set(i)) >= 2])
    docs_df_sentence_group["size_2_group"] = docs_df_sentence_group["size_2_group"].map(lambda x: len(x))

    # weight the groups so that if it is closer to the front, it gets a higher weight 
    # split the document into ten parts based on the sentence index - sentence length * 10 if in the first part, sentence length * 9 if in the second part, etc.
    docs_df_sentence_group["first_sentence_index_per_group"] = docs_df_sentence_group["group_sentence_indices_meet_requirement"].map(lambda x: [i[0] for i in x] if x != [] else [])
    docs_df_sentence_group["position"] = docs_df_sentence_group.apply(lambda x: [math.ceil(i / x["number_of_sentences"] * 10) for i in x["first_sentence_index_per_group"]], axis = 1)
    docs_df_sentence_group["weighted_group_sentence_length"] = docs_df_sentence_group.apply(lambda x: [(10 - x["position"][i] + 1) * x["group_sentence_length_meet_requirement"][i] for i in range(len(x["position"]))], axis = 1)
    docs_df_sentence_group["weighted_group_sentence_length_sum"] = docs_df_sentence_group["weighted_group_sentence_length"].map(lambda x: sum(x))
    
    docs_df_sentence_group = docs_df_sentence_group.drop(docs_df_sentence_group.iloc[:,5:-23], axis = 1)
    
    return docs_df_sentence_group


# In[16]:


def extract_n(text, ind_list, n, c):

    before = [text[(i - n):(i + 1 + c)] if i - n >= 0 else text[0:(i + 1 + c)] for i in ind_list]
    after = [text[i:(i + n + 1 + c)] for i in ind_list]
    
    combined = before + after
    
    return combined


# In[17]:


def adjacent_indices(docs_df_sentence_index_flatten, n):
    
    docs_df_sentence_index_flatten["word_ind"] = docs_df_sentence_index_flatten["text_tokens_stemmed"].map(lambda x: [str(i) for i in range(len(x))])
    docs_df_adjacent_keywords = docs_df_sentence_index_flatten.iloc[:,:7]
    docs_df_adjacent_keywords["cum_sentence_words_count"] = docs_df_sentence_index_flatten["cum_sentence_words_count"]
    
    cols = docs_df_sentence_index_flatten.columns.tolist()[7:-5]
    
    for col in tqdm(cols):
        c = col.count("_")
        docs_df_adjacent_keywords[col] = docs_df_sentence_index_flatten.apply(lambda x: extract_n(x["word_ind"], x[col], n, c), axis = 1)

    return docs_df_adjacent_keywords


# In[18]:


def combine_adjacent_indices(docs_df_sentence_index_flatten, n):
    
    docs_df_adjacent_keywords = adjacent_indices(docs_df_sentence_index_flatten, n)
    docs_df_adjacent_keywords_combined = docs_df_adjacent_keywords.iloc[:,:8]
 
    cols = len(docs_df_sentence_index_flatten.columns.tolist()[7:-5])
    
    docs_df_adjacent_keywords_combined["adjacent_index_combined"] = docs_df_adjacent_keywords.apply(lambda x: [list(itertools.chain(x[i])) for i in range(8, cols + 1)], axis = 1)
    docs_df_adjacent_keywords_combined["adjacent_index_combined"] = docs_df_adjacent_keywords_combined["adjacent_index_combined"].map(lambda x: [item for sublist in x for item in sublist])
    docs_df_adjacent_keywords_combined["adjacent_index_combined"] = docs_df_adjacent_keywords_combined["adjacent_index_combined"].map(lambda x: [item for sublist in x for item in sublist])
    docs_df_adjacent_keywords_combined["adjacent_index_combined"] = docs_df_adjacent_keywords_combined["adjacent_index_combined"].map(lambda x: list(set(x)))
    docs_df_adjacent_keywords_combined["adjacent_index_combined"].map(lambda x: x.sort())
    
    return docs_df_adjacent_keywords_combined 


# In[19]:


def add_comma(ind):
    
    l = []
    
    for i in range(len(ind)):
        
        if i <= len(ind) - 2:
            
            if int(ind[i+1]) - int(ind[i]) > 1:
                
                l.append(ind[i])
                l.append(",")
                
            else:
                l.append(ind[i])   
                
        else:  
            l.append(ind[-1])
            
    return l


# In[20]:


def add_period(cum_sentence_words_count, ind):
    
    l = []
    
    for i in range(len(ind)):
        
        if i <= len(ind) - 2:
            
            if ind[i+1] != "," and ind[i] != ",":
                
                a = bisect(cum_sentence_words_count, int(ind[i]))
                b = bisect(cum_sentence_words_count, int(ind[i+1]))
                
                if a != b:
                    l.append(ind[i])
                    l.append(".")

                else:
                    l.append(ind[i])
                    
            elif ind[i+1] == "," and ind[i] != ",":
                
                a = bisect(cum_sentence_words_count, int(ind[i]))
                b = bisect(cum_sentence_words_count, int(ind[i+2]))
                
                if a != b:
                    l.append(ind[i])
                    l.append(".")

                else:
                    l.append(ind[i])
                
            elif ind[i+1] != "," and ind[i] == ",":
                l.append(ind[i])
                
        else:
            
            l.append(ind[-1])
        
        for i in range(len(l)-1):
            if l[i] == "." and l[i+1] == ",":
                 l.pop(i+1)
                     
    return l 


# In[21]:


def to_text(ind, text):
    
    l = []
    for i in range(len(ind)):
        if ind[i] != "," and ind[i] != ".":
            
            l.append(text[int(ind[i])])
                
        elif ind[i] == ",":
            
            l.append(",")
            
        elif ind[i] == ".":
            l.append(".")
            
    return l


# In[22]:


def adjacent_n_words(docs_df_sentence_index_flatten, n):
    
    docs_df_adjacent_keywords_combined = combine_adjacent_indices(docs_df_sentence_index_flatten, n)
    docs_df_adjacent_keywords_combined["add_comma"] = docs_df_adjacent_keywords_combined["adjacent_index_combined"].map(lambda x: add_comma(x))
    docs_df_adjacent_keywords_combined["add_period"] = docs_df_adjacent_keywords_combined.apply(lambda x: add_period(x["cum_sentence_words_count"], x["add_comma"]), axis = 1) 
    docs_df_adjacent_keywords_combined["text_tokens"] = docs_df_adjacent_keywords_combined["text_tokens"].map(lambda x: [item for sublist in x for item in sublist])
    docs_df_adjacent_keywords_combined["text"] = docs_df_adjacent_keywords_combined.apply(lambda x: to_text(x["add_period"], x["text_tokens"]), axis = 1)
    docs_df_adjacent_keywords_combined = docs_df_adjacent_keywords_combined.drop(columns = ["text_tokens", "text_tokens_stemmed", "cum_sentence_words_count", "adjacent_index_combined", "add_comma", "add_period"])
    
    return docs_df_adjacent_keywords_combined


# In[23]:



def split(l, value):
    
    if l != []:
        
        if "." in l:
            size = len(l)
            idx_list = [idx + 1 for idx, val in
                    enumerate(l) if val == value]

            res = [l[i: j] for i, j in
                zip([0] + idx_list, idx_list + 
                ([size] if idx_list[-1] != size else []))]
        else:
            res = [l]
    else:
        res = [[]] 
    
    return res


# In[24]:


def adjacent_tokenizer(docs_df_adjacent_keywords_combined):
    
    tqdm.pandas() 
    
    docs_df_adjacent_sentence = docs_df_adjacent_keywords_combined.copy()
    
    # create a Snowball stemmer 
    snow_stemmer = SnowballStemmer("english")
    
    # sentence tokenize
    docs_df_adjacent_sentence["text_tokens"] = docs_df_adjacent_sentence["text"].progress_map(lambda x: split(x, "."))
    
    # perform stemming on the tokenized words 
    docs_df_adjacent_sentence["text_tokens_stemmed"] = docs_df_adjacent_sentence["text_tokens"].progress_map(lambda x: [[snow_stemmer.stem(w) for w in i] for i in x])
   
    # drop text column
    docs_df_adjacent_sentence = docs_df_adjacent_sentence.drop(columns = ["text"])
    
    return docs_df_adjacent_sentence


# In[25]:


def adaptation_sentences(docs_df_adjacent_sentence):
    
    tqdm.pandas() 
    
    docs_df_adjacent_sentence_count = docs_df_adjacent_sentence.copy()
    s2 = docs_df_adjacent_sentence_count.shape[1]
    
    docs_df_adjacent_sentence_count["sentences_indices"] = docs_df_adjacent_sentence_count.progress_apply(lambda x: [list(x[i].keys()) for i in range(7,s2-2)], axis = 1)
    docs_df_adjacent_sentence_count["sentences_indices"] = docs_df_adjacent_sentence_count["sentences_indices"].progress_map(lambda x: [item for sublist in x for item in sublist])
    docs_df_adjacent_sentence_count["sentences_counts"] = docs_df_adjacent_sentence_count["sentences_indices"].progress_map(lambda x: len(list(set(x))))
   
    docs_df_adjacent_sentence_count = docs_df_adjacent_sentence_count[["city_index", "city_name", "year", "docs_type", "docs_name", "sentences_counts", "number_of_sentences"]]
    
    return docs_df_adjacent_sentence_count



keywords_hazards = pd.read_excel("Climate_Change_Textual_Analysis/Keywords/keywords_hazards_230911.xlsx")
keywords_hazards_stem = keywords_stemmer(keywords_hazards)

keywords_mitigation = pd.read_excel("Climate_Change_Textual_Analysis/Keywords/keywords_mitigation_230911.xlsx")
keywords_mitigation_stem = keywords_stemmer(keywords_mitigation)

keywords_placebo = pd.read_excel("Climate_Change_Textual_Analysis/Keywords/keywords_placebo_230911.xlsx")
keywords_placebo_stem = keywords_stemmer(keywords_placebo)



# In[27]:


## Read in the pickled data:
docs_df = pd.read_pickle("Climate_Change_Textual_Analysis/Pickled/AL_pickled.pkl")
docs_df.head()

docs_df["text"] = docs_df["text"].map(lambda x: "\n\n\n\n".join([i[1] for i in x.values()]))


# ### with tables

# In[28]:


docs_df1 = docs_df.copy()

docs_df_sentence = sentence_level_tokenizer(docs_df1)
docs_df_sentence.head()


docs_df_sentence_hazards = docs_df_sentence.copy()
docs_df_sentence_mitigation = docs_df_sentence.copy()
docs_df_sentence_placebo = docs_df_sentence.copy()


# In[30]:


if not os.path.exists("Climate_Change_Textual_Analysis/RESULT_keywords/AL"):
    os.makedirs("Climate_Change_Textual_Analysis/RESULT_keywords/AL")




# In[33]:


# hazards
################################################################################################################
# Extract Keywords
docs_df_sentence_hazards = keywords_extractor(keywords_hazards_stem, docs_df_sentence_hazards)
docs_df_sentence_index_flatten_hazards = flatten_sentence_index(docs_df_sentence_hazards)
keywords_counts_per_document_hazards = counts_for_separate_keywords(docs_df_sentence_index_flatten_hazards)

# For Graphs - subcategory level(combine keywords counts based on their subcategories)
subcategory_keywords_hazards = {k: [keywords_hazards_stem["keywords_stem"][i].replace(" ", "_") for i in [j for j, x in enumerate(keywords_hazards_stem["Subcategory"]) if x == k]] for k in set(keywords_hazards_stem["Subcategory"])}
subcategory_keywords_counts_per_document_hazards = keywords_counts_per_document_hazards.iloc[:, np.r_[0:5, -1, -2]]
for k in list(subcategory_keywords_hazards.keys()):
    v = subcategory_keywords_hazards.get(k)
    subcategory_keywords_counts_per_document_hazards[k + "_keyword_counts"] = keywords_counts_per_document_hazards.apply(lambda x: sum(x[v]), axis = 1)
    subcategory_keywords_counts_per_document_hazards[k + "_sentence_counts"] = docs_df_sentence_hazards.apply(lambda x: len(set([item for sublist in [list(x[ii].keys()) for ii in v] for item in sublist])), axis = 1)

# Group Keywords
docs_df_sentence_group_hazards = add_group_count(docs_df_sentence_index_flatten_hazards, 5, 3)

# Number of Sentences with Keywords & For Graphs - ngroups, nkeywords, nkeywordspergroup, nsentences, nsentencespergroup, 2distinctwords, weightedgroup
measures_hazards = docs_df_sentence_group_hazards[["city_index", "city_name", "year", "docs_type", "docs_name", "count", "number_of_sentences",                         "sentence_indices", "number_of_groups_meet_requirement", "number_of_keywords_meet_requirement",                         "group_sentence_length_sum_meet_requirement", "total_number_of_distinct_words",                         "size_2_group", "weighted_group_sentence_length_sum"]]
measures_hazards["count"] = measures_hazards["count"].map(lambda x: x[0])
measures_hazards["sentence_indices"] = measures_hazards["sentence_indices"].map(lambda x: len(set(x)))
measures_hazards = measures_hazards.rename(columns = {"count":"number_of_words"})
measures_hazards = measures_hazards.rename(columns = {"sentence_indices":"sentences_counts"})
measures_hazards = measures_hazards.sort_values(by = ["city_index", "year", "docs_type", "docs_name"])

# For Graphs - distance * size
total_nsentences_size_distance_hazards = measures_hazards.iloc[:, :7]
for d in range(21):
    for s in range(1, 21):
        total_nsentences_size_distance_hazards["distance_" + str(d) + "_size_" + str(s)] = add_group_count(docs_df_sentence_index_flatten_hazards, d, s)["group_sentence_length_sum_meet_requirement"]

# Save to Excel - all on document level
keywords_counts_per_document_hazards["total_number_of_distinct_keywords"] = keywords_counts_per_document_hazards.apply(lambda x: sum([1 if x[i] != 0 else 0 for i in range(5, keywords_counts_per_document_hazards.shape[1] - 2)]), axis = 1)
cols = keywords_counts_per_document_hazards.columns.tolist()
cols = cols[:-3] + cols[-1:] + cols[-3:-1]
keywords_counts_per_document_hazards = keywords_counts_per_document_hazards[cols]

keywords_counts_per_document_hazards.to_excel("Climate_Change_Textual_Analysis/RESULT_keywords/AL/hazards_counts.xlsx") 
subcategory_keywords_counts_per_document_hazards.to_excel("Climate_Change_Textual_Analysis/RESULT_keywords/AL/hazards_subcategory.xlsx") 
measures_hazards.to_excel("Climate_Change_Textual_Analysis/RESULT_keywords/AL/hazards_measures.xlsx") 
total_nsentences_size_distance_hazards.to_excel("Climate_Change_Textual_Analysis/RESULT_keywords/AL/hazards_diff_distance_size.xlsx")
################################################################################################################

# mitigation
################################################################################################################
# Extract Keywords
docs_df_sentence_mitigation = keywords_extractor(keywords_mitigation_stem, docs_df_sentence_mitigation)
docs_df_sentence_index_flatten_mitigation = flatten_sentence_index(docs_df_sentence_mitigation)
keywords_counts_per_document_mitigation = counts_for_separate_keywords(docs_df_sentence_index_flatten_mitigation)

# For Graphs - subcategory level(combine keywords counts based on their subcategories)
subcategory_keywords_mitigation = {k: [keywords_mitigation_stem["keywords_stem"][i].replace(" ", "_") for i in [j for j, x in enumerate(keywords_mitigation_stem["Subcategory"]) if x == k]] for k in set(keywords_mitigation_stem["Subcategory"])}
subcategory_keywords_counts_per_document_mitigation = keywords_counts_per_document_mitigation.iloc[:, np.r_[0:5, -1, -2]]
for k in list(subcategory_keywords_mitigation.keys()):
    v = subcategory_keywords_mitigation.get(k)
    subcategory_keywords_counts_per_document_mitigation[k + "_keyword_counts"] = keywords_counts_per_document_mitigation.apply(lambda x: sum(x[v]), axis = 1)
    subcategory_keywords_counts_per_document_mitigation[k + "_sentence_counts"] = docs_df_sentence_mitigation.apply(lambda x: len(set([item for sublist in [list(x[ii].keys()) for ii in v] for item in sublist])), axis = 1)

# Group Keywords
docs_df_sentence_group_mitigation = add_group_count(docs_df_sentence_index_flatten_mitigation, 5, 3)

# Number of Sentences with Keywords & For Graphs - ngroups, nkeywords, nkeywordspergroup, nsentences, nsentencespergroup, 2distinctwords, weightedgroup
measures_mitigation = docs_df_sentence_group_mitigation[["city_index", "city_name", "year", "docs_type", "docs_name", "count", "number_of_sentences",                         "sentence_indices", "number_of_groups_meet_requirement", "number_of_keywords_meet_requirement",                         "group_sentence_length_sum_meet_requirement", "total_number_of_distinct_words",                         "size_2_group", "weighted_group_sentence_length_sum"]]
measures_mitigation["count"] = measures_mitigation["count"].map(lambda x: x[0])
measures_mitigation["sentence_indices"] = measures_mitigation["sentence_indices"].map(lambda x: len(set(x)))
measures_mitigation = measures_mitigation.rename(columns = {"count":"number_of_words"})
measures_mitigation = measures_mitigation.rename(columns = {"sentence_indices":"sentences_counts"})
measures_mitigation = measures_mitigation.sort_values(by = ["city_index", "year", "docs_type", "docs_name"])

# For Graphs - distance * size
total_nsentences_size_distance_mitigation = measures_mitigation.iloc[:, :7]
for d in range(21):
    for s in range(1, 21):
        total_nsentences_size_distance_mitigation["distance_" + str(d) + "_size_" + str(s)] = add_group_count(docs_df_sentence_index_flatten_mitigation, d, s)["group_sentence_length_sum_meet_requirement"]

# Save to Excel - all on document level
keywords_counts_per_document_mitigation["total_number_of_distinct_keywords"] = keywords_counts_per_document_mitigation.apply(lambda x: sum([1 if x[i] != 0 else 0 for i in range(5, keywords_counts_per_document_mitigation.shape[1] - 2)]), axis = 1)
cols = keywords_counts_per_document_mitigation.columns.tolist()
cols = cols[:-3] + cols[-1:] + cols[-3:-1]
keywords_counts_per_document_mitigation = keywords_counts_per_document_mitigation[cols]

keywords_counts_per_document_mitigation.to_excel("Climate_Change_Textual_Analysis/RESULT_keywords/AL/mitigation_counts.xlsx") 
subcategory_keywords_counts_per_document_mitigation.to_excel("Climate_Change_Textual_Analysis/RESULT_keywords/AL/mitigation_subcategory.xlsx") 
measures_mitigation.to_excel("Climate_Change_Textual_Analysis/RESULT_keywords/AL/mitigation_measures.xlsx") 
total_nsentences_size_distance_mitigation.to_excel("Climate_Change_Textual_Analysis/RESULT_keywords/AL/mitigation_diff_distance_size.xlsx")
################################################################################################################

# placebo
################################################################################################################
# Extract Keywords
docs_df_sentence_placebo = keywords_extractor(keywords_placebo_stem, docs_df_sentence_placebo)
docs_df_sentence_index_flatten_placebo = flatten_sentence_index(docs_df_sentence_placebo)
keywords_counts_per_document_placebo = counts_for_separate_keywords(docs_df_sentence_index_flatten_placebo)

# For Graphs - subcategory level(combine keywords counts based on their subcategories)
subcategory_keywords_placebo = {k: [keywords_placebo_stem["keywords_stem"][i].replace(" ", "_") for i in [j for j, x in enumerate(keywords_placebo_stem["Subcategory"]) if x == k]] for k in set(keywords_placebo_stem["Subcategory"])}
subcategory_keywords_counts_per_document_placebo = keywords_counts_per_document_placebo.iloc[:, np.r_[0:5, -1, -2]]
for k in list(subcategory_keywords_placebo.keys()):
    v = subcategory_keywords_placebo.get(k)
    subcategory_keywords_counts_per_document_placebo[k + "_keyword_counts"] = keywords_counts_per_document_placebo.apply(lambda x: sum(x[v]), axis = 1)
    subcategory_keywords_counts_per_document_placebo[k + "_sentence_counts"] = docs_df_sentence_placebo.apply(lambda x: len(set([item for sublist in [list(x[ii].keys()) for ii in v] for item in sublist])), axis = 1)

# Group Keywords
docs_df_sentence_group_placebo = add_group_count(docs_df_sentence_index_flatten_placebo, 5, 3)

# Number of Sentences with Keywords & For Graphs - ngroups, nkeywords, nkeywordspergroup, nsentences, nsentencespergroup, 2distinctwords, weightedgroup
measures_placebo = docs_df_sentence_group_placebo[["city_index", "city_name", "year", "docs_type", "docs_name", "count", "number_of_sentences",                         "sentence_indices", "number_of_groups_meet_requirement", "number_of_keywords_meet_requirement",                         "group_sentence_length_sum_meet_requirement", "total_number_of_distinct_words",                         "size_2_group", "weighted_group_sentence_length_sum"]]
measures_placebo["count"] = measures_placebo["count"].map(lambda x: x[0])
measures_placebo["sentence_indices"] = measures_placebo["sentence_indices"].map(lambda x: len(set(x)))
measures_placebo = measures_placebo.rename(columns = {"count":"number_of_words"})
measures_placebo = measures_placebo.rename(columns = {"sentence_indices":"sentences_counts"})
measures_placebo = measures_placebo.sort_values(by = ["city_index", "year", "docs_type", "docs_name"])

# For Graphs - distance * size
total_nsentences_size_distance_placebo = measures_placebo.iloc[:, :7]
for d in range(21):
    for s in range(1, 21):
        total_nsentences_size_distance_placebo["distance_" + str(d) + "_size_" + str(s)] = add_group_count(docs_df_sentence_index_flatten_placebo, d, s)["group_sentence_length_sum_meet_requirement"]

# Save to Excel - all on document level
keywords_counts_per_document_placebo["total_number_of_distinct_keywords"] = keywords_counts_per_document_placebo.apply(lambda x: sum([1 if x[i] != 0 else 0 for i in range(5, keywords_counts_per_document_placebo.shape[1] - 2)]), axis = 1)
cols = keywords_counts_per_document_placebo.columns.tolist()
cols = cols[:-3] + cols[-1:] + cols[-3:-1]
keywords_counts_per_document_placebo = keywords_counts_per_document_placebo[cols]

keywords_counts_per_document_placebo.to_excel("Climate_Change_Textual_Analysis/RESULT_keywords/AL/placebo_counts.xlsx") 
subcategory_keywords_counts_per_document_placebo.to_excel("Climate_Change_Textual_Analysis/RESULT_keywords/AL/placebo_subcategory.xlsx") 
measures_placebo.to_excel("Climate_Change_Textual_Analysis/RESULT_keywords/AL/placebo_measures.xlsx") 
total_nsentences_size_distance_placebo.to_excel("Climate_Change_Textual_Analysis/RESULT_keywords/AL/placebo_diff_distance_size.xlsx")
################################################################################################################


# In[35]:


numberofwords = keywords_counts_per_document_placebo["number_of_words"].tolist()
numberofsentences = keywords_counts_per_document_placebo["number_of_sentences"].tolist()


# ### drop tables

# In[36]:


docs_df_sentence = docs_df.copy()
docs_df_sentence['article_id'] = docs_df_sentence.index
preprocess(docs_df_sentence)
docs_df_sentence.head()

digit_pc_df = article_summary(docs_df_sentence)

# Filter out the Sentences that satisfied the criteria
satisfied_df = digit_pc_df[(digit_pc_df.sent_length >= 5)&
                           (digit_pc_df.sent_length < 300)&
                           (digit_pc_df.digit_chunk_ratio < 0.4)].copy()

df_to_keep = pd.DataFrame(satisfied_df.groupby("article_id")["sent_id"].apply(lambda x: x.tolist()))

docs_df_sentence = pd.merge(docs_df_sentence, df_to_keep, on = "article_id", how = "outer")

docs_df_sentence["sent_id"] = docs_df_sentence["sent_id"].fillna("").apply(list)

docs_df_sentence["text_tokens"] = docs_df_sentence.apply(lambda x: [x["text_tokens"][i] for i in x["sent_id"]], axis = 1)

# create a Snowball stemmer 
snow_stemmer = SnowballStemmer("english")
    
# perform stemming on the tokenized words 
docs_df_sentence["text_tokens_stemmed"] = docs_df_sentence["text_tokens"].progress_map(lambda x: [[snow_stemmer.stem(w) for w in i] for i in x])
 
docs_df_sentence = docs_df_sentence.drop(columns = ["text", "article_id", "text_tokens_helper", "text_tokens_digit", "sent_id"])

docs_df_sentence.head()


docs_df_sentence_hazards = docs_df_sentence.copy()
docs_df_sentence_mitigation = docs_df_sentence.copy()
docs_df_sentence_placebo = docs_df_sentence.copy()


# In[38]:


if not os.path.exists("Climate_Change_Textual_Analysis/RESULT_keywords/AL_no_table"):
    os.makedirs("Climate_Change_Textual_Analysis/RESULT_keywords/AL_no_table")


# In[39]:

# hazards
################################################################################################################
# Extract Keywords
docs_df_sentence_hazards = keywords_extractor(keywords_hazards_stem, docs_df_sentence_hazards)
docs_df_sentence_index_flatten_hazards = flatten_sentence_index(docs_df_sentence_hazards)
keywords_counts_per_document_hazards = counts_for_separate_keywords(docs_df_sentence_index_flatten_hazards)

# For Graphs - subcategory level(combine keywords counts based on their subcategories)
subcategory_keywords_hazards = {k: [keywords_hazards_stem["keywords_stem"][i].replace(" ", "_") for i in [j for j, x in enumerate(keywords_hazards_stem["Subcategory"]) if x == k]] for k in set(keywords_hazards_stem["Subcategory"])}
subcategory_keywords_counts_per_document_hazards = keywords_counts_per_document_hazards.iloc[:, np.r_[0:5, -1, -2]]
for k in list(subcategory_keywords_hazards.keys()):
    v = subcategory_keywords_hazards.get(k)
    subcategory_keywords_counts_per_document_hazards[k + "_keyword_counts"] = keywords_counts_per_document_hazards.apply(lambda x: sum(x[v]), axis = 1)
    subcategory_keywords_counts_per_document_hazards[k + "_sentence_counts"] = docs_df_sentence_hazards.apply(lambda x: len(set([item for sublist in [list(x[ii].keys()) for ii in v] for item in sublist])), axis = 1)

# Group Keywords
docs_df_sentence_group_hazards = add_group_count(docs_df_sentence_index_flatten_hazards, 5, 3)

# Number of Sentences with Keywords & For Graphs - ngroups, nkeywords, nkeywordspergroup, nsentences, nsentencespergroup, 2distinctwords, weightedgroup
measures_hazards = docs_df_sentence_group_hazards[["city_index", "city_name", "year", "docs_type", "docs_name", "count", "number_of_sentences",                         "sentence_indices", "number_of_groups_meet_requirement", "number_of_keywords_meet_requirement",                         "group_sentence_length_sum_meet_requirement", "total_number_of_distinct_words",                         "size_2_group", "weighted_group_sentence_length_sum"]]
measures_hazards["count"] = measures_hazards["count"].map(lambda x: x[0])
measures_hazards["sentence_indices"] = measures_hazards["sentence_indices"].map(lambda x: len(set(x)))
measures_hazards = measures_hazards.rename(columns = {"count":"number_of_words"})
measures_hazards = measures_hazards.rename(columns = {"sentence_indices":"sentences_counts"})
measures_hazards = measures_hazards.sort_values(by = ["city_index", "year", "docs_type", "docs_name"])

# For Graphs - distance * size
total_nsentences_size_distance_hazards = measures_hazards.iloc[:, :7]
for d in range(21):
    for s in range(1, 21):
        total_nsentences_size_distance_hazards["distance_" + str(d) + "_size_" + str(s)] = add_group_count(docs_df_sentence_index_flatten_hazards, d, s)["group_sentence_length_sum_meet_requirement"]

# Save to Excel - all on document level
keywords_counts_per_document_hazards["total_number_of_distinct_keywords"] = keywords_counts_per_document_hazards.apply(lambda x: sum([1 if x[i] != 0 else 0 for i in range(5, keywords_counts_per_document_hazards.shape[1] - 2)]), axis = 1)
cols = keywords_counts_per_document_hazards.columns.tolist()
cols = cols[:-3] + cols[-1:] + cols[-3:-1]
keywords_counts_per_document_hazards = keywords_counts_per_document_hazards[cols]

keywords_counts_per_document_hazards["number_of_words"] = numberofwords
keywords_counts_per_document_hazards["number_of_sentences"] = numberofsentences
subcategory_keywords_counts_per_document_hazards["number_of_words"] = numberofwords
subcategory_keywords_counts_per_document_hazards["number_of_sentences"] = numberofsentences
measures_hazards["number_of_words"] = numberofwords
measures_hazards["number_of_sentences"] = numberofsentences
total_nsentences_size_distance_hazards["number_of_words"] = numberofwords
total_nsentences_size_distance_hazards["number_of_sentences"] = numberofsentences


keywords_counts_per_document_hazards.to_excel("Climate_Change_Textual_Analysis/RESULT_keywords/AL_no_table/hazards_counts.xlsx") 
subcategory_keywords_counts_per_document_hazards.to_excel("Climate_Change_Textual_Analysis/RESULT_keywords/AL_no_table/hazards_subcategory.xlsx") 
measures_hazards.to_excel("Climate_Change_Textual_Analysis/RESULT_keywords/AL_no_table/hazards_measures.xlsx") 
total_nsentences_size_distance_hazards.to_excel("Climate_Change_Textual_Analysis/RESULT_keywords/AL_no_table/hazards_diff_distance_size.xlsx")
################################################################################################################

# mitigation
################################################################################################################
# Extract Keywords
docs_df_sentence_mitigation = keywords_extractor(keywords_mitigation_stem, docs_df_sentence_mitigation)
docs_df_sentence_index_flatten_mitigation = flatten_sentence_index(docs_df_sentence_mitigation)
keywords_counts_per_document_mitigation = counts_for_separate_keywords(docs_df_sentence_index_flatten_mitigation)

# For Graphs - subcategory level(combine keywords counts based on their subcategories)
subcategory_keywords_mitigation = {k: [keywords_mitigation_stem["keywords_stem"][i].replace(" ", "_") for i in [j for j, x in enumerate(keywords_mitigation_stem["Subcategory"]) if x == k]] for k in set(keywords_mitigation_stem["Subcategory"])}
subcategory_keywords_counts_per_document_mitigation = keywords_counts_per_document_mitigation.iloc[:, np.r_[0:5, -1, -2]]
for k in list(subcategory_keywords_mitigation.keys()):
    v = subcategory_keywords_mitigation.get(k)
    subcategory_keywords_counts_per_document_mitigation[k + "_keyword_counts"] = keywords_counts_per_document_mitigation.apply(lambda x: sum(x[v]), axis = 1)
    subcategory_keywords_counts_per_document_mitigation[k + "_sentence_counts"] = docs_df_sentence_mitigation.apply(lambda x: len(set([item for sublist in [list(x[ii].keys()) for ii in v] for item in sublist])), axis = 1)

# Group Keywords
docs_df_sentence_group_mitigation = add_group_count(docs_df_sentence_index_flatten_mitigation, 5, 3)

# Number of Sentences with Keywords & For Graphs - ngroups, nkeywords, nkeywordspergroup, nsentences, nsentencespergroup, 2distinctwords, weightedgroup
measures_mitigation = docs_df_sentence_group_mitigation[["city_index", "city_name", "year", "docs_type", "docs_name", "count", "number_of_sentences",                         "sentence_indices", "number_of_groups_meet_requirement", "number_of_keywords_meet_requirement",                         "group_sentence_length_sum_meet_requirement", "total_number_of_distinct_words",                         "size_2_group", "weighted_group_sentence_length_sum"]]
measures_mitigation["count"] = measures_mitigation["count"].map(lambda x: x[0])
measures_mitigation["sentence_indices"] = measures_mitigation["sentence_indices"].map(lambda x: len(set(x)))
measures_mitigation = measures_mitigation.rename(columns = {"count":"number_of_words"})
measures_mitigation = measures_mitigation.rename(columns = {"sentence_indices":"sentences_counts"})
measures_mitigation = measures_mitigation.sort_values(by = ["city_index", "year", "docs_type", "docs_name"])

# For Graphs - distance * size
total_nsentences_size_distance_mitigation = measures_mitigation.iloc[:, :7]
for d in range(21):
    for s in range(1, 21):
        total_nsentences_size_distance_mitigation["distance_" + str(d) + "_size_" + str(s)] = add_group_count(docs_df_sentence_index_flatten_mitigation, d, s)["group_sentence_length_sum_meet_requirement"]

# Save to Excel - all on document level
keywords_counts_per_document_mitigation["total_number_of_distinct_keywords"] = keywords_counts_per_document_mitigation.apply(lambda x: sum([1 if x[i] != 0 else 0 for i in range(5, keywords_counts_per_document_mitigation.shape[1] - 2)]), axis = 1)
cols = keywords_counts_per_document_mitigation.columns.tolist()
cols = cols[:-3] + cols[-1:] + cols[-3:-1]
keywords_counts_per_document_mitigation = keywords_counts_per_document_mitigation[cols]

keywords_counts_per_document_mitigation["number_of_words"] = numberofwords
keywords_counts_per_document_mitigation["number_of_sentences"] = numberofsentences
subcategory_keywords_counts_per_document_mitigation["number_of_words"] = numberofwords
subcategory_keywords_counts_per_document_mitigation["number_of_sentences"] = numberofsentences
measures_mitigation["number_of_words"] = numberofwords
measures_mitigation["number_of_sentences"] = numberofsentences
total_nsentences_size_distance_mitigation["number_of_words"] = numberofwords
total_nsentences_size_distance_mitigation["number_of_sentences"] = numberofsentences


keywords_counts_per_document_mitigation.to_excel("Climate_Change_Textual_Analysis/RESULT_keywords/AL_no_table/mitigation_counts.xlsx") 
subcategory_keywords_counts_per_document_mitigation.to_excel("Climate_Change_Textual_Analysis/RESULT_keywords/AL_no_table/mitigation_subcategory.xlsx") 
measures_mitigation.to_excel("Climate_Change_Textual_Analysis/RESULT_keywords/AL_no_table/mitigation_measures.xlsx") 
total_nsentences_size_distance_mitigation.to_excel("Climate_Change_Textual_Analysis/RESULT_keywords/AL_no_table/mitigation_diff_distance_size.xlsx")
################################################################################################################

# placebo
################################################################################################################
# Extract Keywords
docs_df_sentence_placebo = keywords_extractor(keywords_placebo_stem, docs_df_sentence_placebo)
docs_df_sentence_index_flatten_placebo = flatten_sentence_index(docs_df_sentence_placebo)
keywords_counts_per_document_placebo = counts_for_separate_keywords(docs_df_sentence_index_flatten_placebo)

# For Graphs - subcategory level(combine keywords counts based on their subcategories)
subcategory_keywords_placebo = {k: [keywords_placebo_stem["keywords_stem"][i].replace(" ", "_") for i in [j for j, x in enumerate(keywords_placebo_stem["Subcategory"]) if x == k]] for k in set(keywords_placebo_stem["Subcategory"])}
subcategory_keywords_counts_per_document_placebo = keywords_counts_per_document_placebo.iloc[:, np.r_[0:5, -1, -2]]
for k in list(subcategory_keywords_placebo.keys()):
    v = subcategory_keywords_placebo.get(k)
    subcategory_keywords_counts_per_document_placebo[k + "_keyword_counts"] = keywords_counts_per_document_placebo.apply(lambda x: sum(x[v]), axis = 1)
    subcategory_keywords_counts_per_document_placebo[k + "_sentence_counts"] = docs_df_sentence_placebo.apply(lambda x: len(set([item for sublist in [list(x[ii].keys()) for ii in v] for item in sublist])), axis = 1)

# Group Keywords
docs_df_sentence_group_placebo = add_group_count(docs_df_sentence_index_flatten_placebo, 5, 3)

# Number of Sentences with Keywords & For Graphs - ngroups, nkeywords, nkeywordspergroup, nsentences, nsentencespergroup, 2distinctwords, weightedgroup
measures_placebo = docs_df_sentence_group_placebo[["city_index", "city_name", "year", "docs_type", "docs_name", "count", "number_of_sentences",                         "sentence_indices", "number_of_groups_meet_requirement", "number_of_keywords_meet_requirement",                         "group_sentence_length_sum_meet_requirement", "total_number_of_distinct_words",                         "size_2_group", "weighted_group_sentence_length_sum"]]
measures_placebo["count"] = measures_placebo["count"].map(lambda x: x[0])
measures_placebo["sentence_indices"] = measures_placebo["sentence_indices"].map(lambda x: len(set(x)))
measures_placebo = measures_placebo.rename(columns = {"count":"number_of_words"})
measures_placebo = measures_placebo.rename(columns = {"sentence_indices":"sentences_counts"})
measures_placebo = measures_placebo.sort_values(by = ["city_index", "year", "docs_type", "docs_name"])

# For Graphs - distance * size
total_nsentences_size_distance_placebo = measures_placebo.iloc[:, :7]
for d in range(21):
    for s in range(1, 21):
        total_nsentences_size_distance_placebo["distance_" + str(d) + "_size_" + str(s)] = add_group_count(docs_df_sentence_index_flatten_placebo, d, s)["group_sentence_length_sum_meet_requirement"]

# Save to Excel - all on document level
keywords_counts_per_document_placebo["total_number_of_distinct_keywords"] = keywords_counts_per_document_placebo.apply(lambda x: sum([1 if x[i] != 0 else 0 for i in range(5, keywords_counts_per_document_placebo.shape[1] - 2)]), axis = 1)
cols = keywords_counts_per_document_placebo.columns.tolist()
cols = cols[:-3] + cols[-1:] + cols[-3:-1]
keywords_counts_per_document_placebo = keywords_counts_per_document_placebo[cols]

keywords_counts_per_document_placebo["number_of_words"] = numberofwords
keywords_counts_per_document_placebo["number_of_sentences"] = numberofsentences
subcategory_keywords_counts_per_document_placebo["number_of_words"] = numberofwords
subcategory_keywords_counts_per_document_placebo["number_of_sentences"] = numberofsentences
measures_placebo["number_of_words"] = numberofwords
measures_placebo["number_of_sentences"] = numberofsentences
total_nsentences_size_distance_placebo["number_of_words"] = numberofwords
total_nsentences_size_distance_placebo["number_of_sentences"] = numberofsentences


keywords_counts_per_document_placebo.to_excel("Climate_Change_Textual_Analysis/RESULT_keywords/AL_no_table/placebo_counts.xlsx") 
subcategory_keywords_counts_per_document_placebo.to_excel("Climate_Change_Textual_Analysis/RESULT_keywords/AL_no_table/placebo_subcategory.xlsx") 
measures_placebo.to_excel("Climate_Change_Textual_Analysis/RESULT_keywords/AL_no_table/placebo_measures.xlsx") 
total_nsentences_size_distance_placebo.to_excel("Climate_Change_Textual_Analysis/RESULT_keywords/AL_no_table/placebo_diff_distance_size.xlsx")
################################################################################################################


