import numpy as np 
import pandas as pd 

def sigmoid(x):
    return 1/(1 + np.exp(-x))

def get_logits(model, xb, yb, ib):
    with model.graph.as_default():
        sess = model.sess
        x = model.inputs
        y = model.targets
        segment_ids = model.segment_ids
        is_training = model.is_training 

        feed_dict = {x:xb, segment_ids:ib, y:yb, is_training:False}

        return sess.run([model.outputs, model.targets], feed_dict)
        
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
