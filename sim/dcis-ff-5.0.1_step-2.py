
import sys
import os
from pathlib import Path
import json 
import pandas as pd
import tensorflow as tf
import numpy as np

from sim.utils import DCISFF, get_logits, sigmoid 

from mmi.loss    import BinaryCrossEntropyLoss, BinaryAccuracy,\
                        L1Regularizer, L2Regularizer
from mmi.model   import Sigmoid, ReLU, Softmax, Convolution2D,\
                        MaxPooling2D,Dropout, Flatten, BatchNormalization,\
                        LinearLayer, BagLayerMax, Model

## Command line arguments -----------------------------------------------------#
for i in range(1, len(sys.argv)):
    print(sys.argv[i], end = " ")

print(len(sys.argv))

if len(sys.argv) >= 4:
    n = int(sys.argv[1])
    batch_size = int(sys.argv[2])
    output_dir = sys.argv[3]
    data_dir = sys.argv[4]
else:
    n = 100
    batch_size = 1
    output_dir = "output/5.0"
    data_dir = "data/processed"
# 100 runs at `batch_size` = 1, for 100 total 
batch_size = 1 # note: only batch_size = 1 is supported currently
# output_dir = "/z/Comp/spkent/simulation/mildsvm-sims/5.0"

## Pull in evaluation spec, data -----------------------------------------------#

#' See dcis-ff-5.0.0_step-1.R and gs-to-eval-spec_5.0.0.R for details.  
#' 10-fold cross validation
#' 10 replications 

fname = Path(output_dir, "eval_spec_5.0.0.json")
eval_spec = json.load(open(fname))

df = pd.read_csv(Path(data_dir, "mild-dcis-ff.csv"))


## Get predictions for all models -------------------------------------------------#

preds = []
for i in range(n):

    # Pull information from evaluation specification 
    train_ind = eval_spec["train"][i]
    train_ind = [x-1 for x in train_ind] # convert to 0 index in python

    test_ind = eval_spec["test"][i]
    test_ind.sort()
    test_ind = [x-1 for x in test_ind]

    rep = eval_spec["rep"][i]
    fold = eval_spec["fold"][i]

    # Set up data frames
    df_train = df.iloc[train_ind, :]
    df_test = df.iloc[test_ind, :]

    train = DCISFF(df_train, seed = 8)
    test = DCISFF(df_test, seed = 8)

    # Load model 
    debug_folder = Path(output_dir, str(i), "debug/")
    sess = os.listdir(debug_folder)[0]

    model = Model()
    model.load_from_json(Path(debug_folder, sess, "model.json"))

    input_shape = {
        'x': {'shape':[19], 'sparse':False, 'type':tf.float32},
        'y': {'shape':[], 'sparse':False, 'type':tf.float32},
        'segment_ids': {'shape':[2], 'sparse':False, 'type':tf.int32}
    }

    model.compile_(input_shape, BinaryCrossEntropyLoss(),\
                callbacks=[BinaryAccuracy(), L2Regularizer(5e-4)],\
                optimizer='adam', learning_rate=1e-3,\
                debug_folder=Path(output_dir, str(i), 'debug2/'))

    model.restore(str(Path(debug_folder, sess, "logs/model-201")))

    # Pull logits from the model and output an array 
    for xb,mb_dim,yb,ib in test.get_minibatches(500, shuffle=False):
        logits, y = get_logits(model, xb, yb, ib)

    logits = sigmoid(logits)
    y = y.reshape(logits.shape)
    logits = logits.reshape(y.shape)

    n_bag = len(y)
    rep = np.array([rep]*n_bag).reshape(-1, 1)
    fold = np.array([fold]*n_bag).reshape(-1, 1)

    out = np.hstack([rep, fold, logits, y])
    preds.append(out)

pred_df = pd.DataFrame(np.concatenate(preds))
pred_df.columns = ["rep", "fold", "y_pred", "y_true"]
pred_df.to_csv(Path(output_dir, "mildsvm-sims-preds-5.0.0-2.csv"))
