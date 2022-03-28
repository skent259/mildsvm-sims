##-----------------------------------------------------------------------------#
#' Simulation 5.0.1
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

print(len(sys.argv))

if len(sys.argv) >= 4:
    i = int(sys.argv[1])
    batch_size = int(sys.argv[2])
    output_dir = sys.argv[3]
    data_dir = sys.argv[4]
else:
    i = 0
    batch_size = 1
    output_dir = "output/5.0"
    data_dir = "data/processed"
# 100 runs at `batch_size` = 1, for 100 total 
batch_size = 1 # note: only batch_size = 1 is supported currently

# output_dir = "/z/Comp/spkent/simulation/mildsvm-sims/5.0"

## Pull in evaluation spec -----------------------------------------------------#

#' See dcis-ff-5.0.0_step-1.R and gs-to-eval-spec_5.0.0.R for details.  
#' 10-fold cross validation
#' 10 replications 

fname = Path(output_dir, "eval_spec_5.0.0.json")
eval_spec = json.load(open(fname))

train_ind = eval_spec["train"][i]
test_ind = eval_spec["test"][i]
test_ind.sort()

# convert to 0 index in python
train_ind = [x-1 for x in train_ind]
test_ind = [x-1 for x in test_ind]

## Data set --------------------------------------------------------------------#

df = pd.read_csv(Path(data_dir, "mild-dcis-ff.csv"))

df_train = df.iloc[train_ind, :]
df_test = df.iloc[test_ind, :]

class DCISFF(object):
    """
    Loosely based off of MMIMNIST here: https://github.com/alessandro-t/mmi/blob/master/mmi/dataset.py
    """
    def __init__(self, df, seed=None):
        self.X = np.array(df.iloc[:, 4:23])
        self.y = np.array(df["bag_label"])
        self.bags = np.array(df["bag_name"])
        self.inst = np.array(df["instance_name"])
        self.seed = seed
        if seed is not None:
            np.random.seed(seed)
    
    def get_minibatches(self, mb_size, shuffle=True):
        """
        grab `mb_size` number of top bags randomly (shuffled)
        output:
        - `X_batch`: The data set with all rows (samples/instances) in the top bag
        - `right_i-left_i`: The number of indices taken
        - `y_batch`: The labels of the top bags from the batch (length mb_size)
        - `new_seg_ids`: An array with two columns, the first indicates the index of the top bag (0:mb_size) and 
          the second indicates the index of the sub bag (within the bag, 0:n_subbag).  
        """
        assert type(mb_size) == int
        assert mb_size > 0

        self.unique_bags = np.unique(self.bags)

        n_samples = len(self.unique_bags)
        n_batches = np.ceil(1.*n_samples/mb_size).astype(np.int32)

        all_indices = np.arange(n_samples)
        if shuffle:
            np.random.shuffle(all_indices)
        
        for b in range(n_batches):
            left_i  = b*mb_size
            right_i = min((b+1)*mb_size, n_samples)

            curr_ind = all_indices[left_i:right_i]

            row_inds = [np.where(self.bags == x)[0] for x in self.unique_bags[curr_ind]]
            rows = np.hstack(row_inds)
            X_batch = self.X[rows,] 
            y_batch = self.y[[ind[0] for ind in row_inds]].astype(np.float32)

            seg_ids = []
            for i,ind in enumerate(row_inds):
                bag_i = np.unique(self.inst[ind])
                for j,inst_j in enumerate(bag_i):
                    n_j = len(np.where(self.inst == inst_j)[0])
                    for k in range(n_j):
                        seg_ids.append([i,j])
            seg_ids = np.array(seg_ids)

            yield X_batch, right_i-left_i, y_batch, seg_ids

train = DCISFF(df_train, seed = 8)
test = DCISFF(df_test, seed = 8)


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
               debug_folder=Path(output_dir, i, 'debug/'))


## Evaluate models in current batch -------------------------------------------#

for epoch in range(200):
    for xb,mb_dim,yb,ib in train.get_minibatches(10, shuffle=True):
        model.train_on_batch(xb,mb_dim,yb,ib)
    if (epoch+1)%5 == 0:
        for xb,mb_dim,yb,ib in train.get_minibatches(200, shuffle=False):
            model.update_metrics(xb, mb_dim, yb, ib)
        train_stats = model.save_merged('train', epoch, True)
        for xb,mb_dim,yb,ib in test.get_minibatches(200, shuffle=False):
            model.update_metrics(xb, mb_dim, yb, ib)
        test_stats  = model.save_merged('test', epoch, True)
        print('End of Epoch %03d' % (epoch + 1))
        print('Train - Loss: %.3f Accuracy: %.3f' %\
              (train_stats['loss'],train_stats['binaryaccuracy']))
        print('Test  - Loss: %.3f Accuracy: %.3f' %\
              (test_stats['loss'],test_stats['binaryaccuracy']))
        print('-'*35)

model.save_model(epoch+1)