# python3 load_model.py clf_NBC.pkl count_vect.pkl my_ser.csv outfile

import sys
import os
import pickle
import pandas as pd
import numpy as np

model = sys.argv[1]
count = sys.argv[2]
series = sys.argv[3]
out_file = sys.argv[4]

def read_pickle_file(file):
  pickled_model = pickle.load(open(file, 'rb'))
  return pickled_model

clf_model  = read_pickle_file(model)
count_vect = read_pickle_file(count)

series = pd.read_csv(series, index_col = 0, squeeze = True)

def predict():
    return clf_model.predict(count_vect.transform(series)) 

array = predict()

with open(out_file, "w") as txt_file:
    for line in array:
        txt_file.write("".join(line) + "\n") 

