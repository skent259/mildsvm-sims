##-----------------------------------------------------------------------------#
#' Simulation 5.0.0
#'   Fiber features DCIS data 
#'   Step 1 - Test set evaluation (no grid-search necessary)
#' 
#' See simulation-spreadsheet.xlsx for details 
##-----------------------------------------------------------------------------#

import sys
# import os
from pathlib import Path
import json 
import pandas as pd
import tensorflow as tf
import numpy as np

from mmi.dataset import MMIMNIST
from mmi.loss    import BinaryCrossEntropyLoss, BinaryAccuracy,\
                        L1Regularizer, L2Regularizer
from mmi.model   import Sigmoid, ReLU, Softmax, Convolution2D,\
                        MaxPooling2D,Dropout, Flatten, BatchNormalization,\
                        LinearLayer, BagLayerMax, Model


## Command line arguments -----------------------------------------------------#
for i in range(1, len(sys.argv)):
    print(sys.argv[i], end = " ")

if len(sys.argv) >= 4:
    i = sys.argv[0]
    batch_size = sys.argv[1] 
    output_dir = sys.argv[2]
    data_dir = sys.argv[3]
else:
    i = 1
    batch_size = 1
    output_dir = "output/5.0"
    data_dir = "data/processed"
# 100 runs at `batch_size` = 1, for 100 total 
batch_size = 1 # note: only batch_size = 1 is supported currently


## Pull in evaluation spec -----------------------------------------------------#

fname = Path(output_dir, "eval_spec_5.0.0.json")
eval_spec = json.load(open(fname))

train_ind = eval_spec["train"][i]
test_ind = eval_spec["test"][i]

## Data set --------------------------------------------------------------------#

df = pd.read_csv(Path(data_dir, "mild-dcis-ff.csv"))
y = df["bag_label"]
bags = df["bag_name"]
x = df.iloc[:, 4:23]

# maybe split into test/train here

## Model parameters -----------------------------------------------------------#

layers = [
    LinearLayer(128),
    BagLayerMax(),
    ReLU(),
    LinearLayer(128),
    BagLayerMax(),
    ReLU(),
    LinearLayer(1, init_type='he_uniform')
]

model = Model()
for layer in layers:
    model.add_layer(layer)

input_shape = {
    'x': {'shape':[19], 'sparse':False, 'type':tf.float32},
    'y': {'shape':[], 'sparse':False, 'type':tf.float32},
    'segment_ids': {'shape':[2], 'sparse':False, 'type':tf.int32}
}

model.compile_(input_shape, BinaryCrossEntropyLoss(),\
               callbacks=[BinaryAccuracy(), L2Regularizer(5e-4)],\
               optimizer='adam', learning_rate=1e-3,\
               debug_folder='debug/')

