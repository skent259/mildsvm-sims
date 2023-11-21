##-----------------------------------------------------------------------------#
#' Simulation 5.0.3
#'   Fiber features DCIS data
#'   Step 1 - Test set evaluation (no grid-search necessary)
#'
#' See simulation-spreadsheet.xlsx for details
##-----------------------------------------------------------------------------#

import json
import sys
import time
from pathlib import Path

[sys.path.append(i) for i in [".", ".."]]  # need to access datasets and models module

import numpy as np
import pandas as pd
import scipy.stats
import tensorflow as tf
from tensorflow.keras import layers, losses, metrics
from tensorflow.keras.callbacks import CSVLogger

from sim.utilsnnet import BagWise, MILPool, MILTabularDataGenerator

## Command line arguments -----------------------------------------------------#
for i in range(1, len(sys.argv)):
    print(sys.argv[i], end=" ")

print(len(sys.argv))

if len(sys.argv) >= 4:
    i = int(sys.argv[1])
    batch_size = int(sys.argv[2])
    output_dir = sys.argv[3]
    data_dir = sys.argv[4]
else:
    i = 0
    batch_size = 1
    output_dir = "output/5.0/"
    data_dir = "data/processed/"
# 100 runs at `batch_size` = 1, for 100 total
batch_size = 1  # note: only batch_size = 1 is supported currently

## Pull in evaluation spec -----------------------------------------------------#

#' See gs-to-eval-spec_5.0.3.R for details.
#' 10-fold cross validation
#' 10 replications

fname = Path(output_dir, "eval_spec_5.0.3.json")
eval_spec = json.load(open(fname))

train_ind = eval_spec["train"][i]
test_ind = eval_spec["test"][i]
test_ind.sort()

# convert to 0 index in python
train_ind = [x - 1 for x in train_ind]
test_ind = [x - 1 for x in test_ind]

## Data set --------------------------------------------------------------------#

df = pd.read_csv(Path(data_dir, "mild-dcis-ff.csv"))


def summarize_features(df: pd.DataFrame):
    """summarize the fibers to the spot (instance) level"""

    f_cols = df.columns[3:]
    fns = [
        "mean",
        "std",
        "skew",
        lambda x: scipy.stats.kurtosis(x),
        lambda x: np.percentile(x, q=25),
        lambda x: np.percentile(x, q=75),
    ]
    agg_spec = {col: fns for col in f_cols}

    feat_1 = df.groupby("instance_name").agg(agg_spec)

    feat_corr = df[["instance_name", *f_cols]].groupby("instance_name").corr()
    feat_corr = feat_corr.unstack(level=1)

    feat_all = pd.merge(feat_1, feat_corr, on="instance_name")
    bag_info = df[["bag_label", "bag_name", "instance_name"]].drop_duplicates()

    return pd.merge(bag_info, feat_all, on="instance_name", how="right")


df_train = df.iloc[train_ind, :]
df_test = df.iloc[test_ind, :]

df_train = summarize_features(df_train)
df_test = summarize_features(df_test)

train_generator = MILTabularDataGenerator(
    dataframe=df_train,
    x_cols=list(df_train.columns[3:]),
    y_col="bag_label",
    bag_col="bag_name",
    batch_size=1,
    shuffle=True,
    class_mode="sparse",
)

test_generator = MILTabularDataGenerator(
    dataframe=df_test,
    x_cols=list(df_test.columns[3:]),
    y_col="bag_label",
    bag_col="bag_name",
    batch_size=1,
    shuffle=True,
    class_mode="sparse",
)

## Model parameters -----------------------------------------------------------#

k = 400  # layer size
n_features = 520
pooling_mode = "max"
learning_rate = 0.001

inputs = layers.Input(shape=(None, n_features))

x = BagWise(layers.Dense(k))(inputs)
x = BagWise(layers.LeakyReLU(alpha=0.01))(x)
x = BagWise(layers.Dropout(0.2))(x)
x = BagWise(layers.BatchNormalization())(x)

x = BagWise(layers.Dense(k))(x)
x = BagWise(layers.LeakyReLU(alpha=0.01))(x)
x = BagWise(layers.Dropout(0.2))(x)
x = BagWise(layers.BatchNormalization())(x)

outputs = MILPool(pooling_mode=pooling_mode)()(x)
outputs = layers.Dense(1, activation="sigmoid")(outputs)

model = tf.keras.Model(inputs=inputs, outputs=outputs)
model.compile(
    optimizer=tf.keras.optimizers.Adam(lr=learning_rate),
    loss=losses.BinaryCrossentropy(from_logits=False),
    metrics=[metrics.AUC(), "accuracy"],
)

## Train model ---------------------------------------------------------------#

t_start = time.time()
csv_log = CSVLogger(filename=output_dir + f"dcis-ff-5.0.3-1_log_i={i}.csv")

STEP_SIZE_TRAIN = train_generator.n // train_generator.batch_size
model.fit(
    x=train_generator,
    steps_per_epoch=STEP_SIZE_TRAIN,
    epochs=200,
    # epochs=50,
    callbacks=[csv_log],
    verbose=0,
)
t_end = time.time()

## Compute metrics on test set -----------------------------------------------#

eval_info = {key: eval_spec[key][i] for key in ["rep", "fold"]}

metrics1 = model.evaluate(test_generator)
metrics2 = {i: j for (i, j) in zip(["loss", "auc", "accuracy"], metrics1)}
metrics_df = pd.DataFrame(
    {"i": i, **eval_info, **metrics2, "time": t_end - t_start}, index=[0]
)
metrics_df.to_csv(output_dir + f"dcis-ff-5.0.3-1_metrics_i={i}.csv")
