from typing import Dict, List, Tuple

import numpy as np
import pandas as pd
import tensorflow as tf

BagWise = tf.keras.layers.TimeDistributed


def MILPool(pooling_mode: str = "max"):
    layer = {
        "max": tf.keras.layers.GlobalMaxPool1D,
        "mean": tf.keras.layers.GlobalAveragePooling1D,
    }

    return layer[pooling_mode]


class MILTabularDataGenerator(tf.keras.utils.Sequence):
    """
    Keras Sequence to pull in batches of bags in a MIL data set

    NOTE: currently only works with `batch_size` = 1 unless all bags have
    the same bag_size. The numpy array can't convert different bag sizes into
    a single array.

    Thank you to following site for inspiration:
    https://medium.com/analytics-vidhya/write-your-own-custom-data-generator-for-tensorflow-keras-1252b64e41c3
    """

    def __init__(
        self,
        dataframe: pd.DataFrame,
        x_cols: List[str],
        y_col: str,
        bag_col: str,
        batch_size: int,
        shuffle: bool,
        class_mode: str,
        class_indices: Dict[str, int] = None,
    ):
        # TODO: maybe add `rescale`?

        self.df = dataframe.copy()
        self.x_cols = x_cols
        self.y_col = y_col
        self.bag_col = bag_col
        self.batch_size = batch_size
        self.shuffle = shuffle
        self.class_mode = class_mode

        self.bags = self.df[self.bag_col].unique()
        self.n = len(self.bags)

        if not class_indices:
            class_indices = np.unique(self.df[self.y_col].explode())
            self.class_indices = dict(zip(class_indices, np.arange(len(class_indices))))
        else:
            self.class_indices = class_indices

    def on_epoch_end(self):
        if self.shuffle:
            self.df = self.df.sample(frac=1).reset_index(drop=True)

    def __get_input(self, bag):
        ind = self.df[self.bag_col] == bag
        return np.asarray(self.df.loc[ind, self.x_cols])

    def __get_output(self, bag):
        ind = self.df[self.bag_col] == bag
        label = (
            self.df[self.y_col]
            .loc[
                ind,
            ]
            .tolist()
        )

        if type(label) is list:
            int_label = [self.class_indices[i] for i in label]
            int_label = np.max(int_label)
        else:
            int_label = self.class_indices[label]

        if self.class_mode == "sparse":
            return float(int_label)
        if self.class_mode == "categorical":
            return tf.keras.utils.to_categorical(
                int_label, num_classes=len(self.class_indices)
            )
        return None

    def __getitem__(self, index):
        # Each row represents an instance, but each batch represents a bag
        bag_batches = self.bags[index * self.batch_size : (index + 1) * self.batch_size]

        X_batch = np.asarray([self.__get_input(i) for i in bag_batches])
        y_batch = np.asarray([self.__get_output(i) for i in bag_batches])

        return X_batch, y_batch

    def __len__(self):
        return self.n // self.batch_size
