"""Script to train models. Runs training code twice. In the first run, 20% of the data is excluded from training and
used as a test set to estimate model accuracy. In the second run all the data is used for training, and the trained models are
saved."""

import keras as ks
import pandas as pd
import numpy as np
import metrics
from collections import namedtuple
import tensorflow as tf
import datetime as dt
import os
from utilities import *
from model_definitions import *


model_arr = get_model_params()
log_file = 'log_{}.txt'.format(dt.datetime.now().strftime('%y%m%d_%H%M'))

for test_mode in [True, False]:
    for m in model_arr:

        # read data
        series = pd.read_csv(os.path.join('data', '{}_train_shuffle.csv'.format(m.freq_name)), header=0, index_col=0)
        num_values = series.notnull().sum(axis=1)

        # in test mode, make predictions on holdout set of training data
        prediction = dict()

        # loop over the different training lengths
        training_lengths = m.training_lengths
        for series_length in training_lengths:
            horizon = m.horizon

            all_data = get_base_data(series, series_length, horizon, m.cycle_length)

            # get extended training set
            train_x_ext, train_y_ext, weights = get_extended_data(series, series_length, min(training_lengths),
                                                                  horizon, m.cycle_length, m.augment, test_mode)
            train_x_ext, train_mean_ext, train_std_ext = normalise(train_x_ext)

            # only train on series that are long enough
            train_length_ok = np.logical_or(series_length == min(training_lengths),
                                            num_values.values - horizon >= series_length)

            if test_mode:
                # initialise prediction array
                prediction[series_length] = np.zeros([len(all_data), horizon])
                curr_prediction = prediction[series_length]
                curr_prediction[:] = np.nan

            # different models for each future time step
            for horizon_step in range(horizon):
                print('\n{} model, series length {}, horizon step {}'.format(m.freq_name, series_length, horizon_step))

                train_y = train_y_ext[:, horizon_step]
                train_y = (train_y - train_mean_ext) / train_std_ext

                # exclude outliers
                max_y = 5 * np.std(train_y)
                train_y[train_y > max_y] = max_y
                train_y[train_y < -max_y] = -max_y

                # clear session and reset default graph, as suggested here, to speed up training
                # https://stackoverflow.com/questions/45796167/training-of-keras-model-gets-slower-after-each-repetition
                ks.backend.clear_session()
                tf.reset_default_graph()

                # set random seeds as described here:
                # https://keras.io/getting-started/faq/#how-can-i-obtain-reproducible-results-using-keras-during-development
                np.random.seed(0)
                session_conf = tf.ConfigProto(intra_op_parallelism_threads=1, inter_op_parallelism_threads=1)
                tf.set_random_seed(0)
                tf_session = tf.Session(graph=tf.get_default_graph(), config=session_conf)
                ks.backend.set_session(tf_session)
                # end setting random seeds

                # define model and train
                nn_model_def = m.model_constructor
                est, epochs, batch_size = nn_model_def(series_length)
                est.fit(train_x_ext, train_y, epochs=epochs, batch_size=batch_size, shuffle=True, validation_split=0.1,
                        callbacks=[ks.callbacks.EarlyStopping(monitor='val_loss', min_delta=0.0001, patience=10)])

                if test_mode:
                    train_x = all_data[:, :series_length]
                    train_x, train_mean, train_std = normalise(train_x)
                    curr_prediction[:, horizon_step] = np.where(train_length_ok, est.predict(train_x).flatten(),
                                                                curr_prediction[:, horizon_step])
                else:
                    # save model
                    if not os.path.exists('trained_models'):
                        os.mkdir('trained_models')
                    model_file = os.path.join('trained_models',
                                              '{}_length_{}_step_{}.h5'.format(m.freq_name, series_length,
                                                                                 horizon_step))
                    est.save(model_file)

            if test_mode:
                # denormalise
                prediction[series_length] = denormalise(prediction[series_length], train_mean, train_std)

        if test_mode:

            blend_predictions(prediction, num_values.values - horizon)

            # evaluate on test set and write to log
            with open(log_file, 'a') as f:
                f.write('\n\n{} ACCURACY'.format(m.freq_name.upper()))
            for series_length in training_lengths:
                train_length_ok = np.logical_or(series_length == min(training_lengths),
                                                num_values.values - horizon >= series_length)
                with open(log_file, 'a') as f:
                    f.write('\nAccuracy for series with training_length >= {}'.format(series_length))
                for train_length in training_lengths + [-1]:
                    if train_length > series_length:
                        continue
                    curr_prediction = prediction[train_length]
                    mase_arr = np.zeros([len(all_data)])
                    smape_arr = np.zeros(len(all_data))
                    for i in range(len(all_data)):
                        row = series.iloc[i].values
                        row = row[np.logical_not(np.isnan(row))]
                        mase_arr[i] = metrics.mase(row[:-horizon], row[-horizon:], curr_prediction[i, :], m.freq)
                        smape_arr[i] = metrics.smape(row[-horizon:], curr_prediction[i, :])

                    last_test_row = int(np.round(len(all_data) * 0.2, 0))
                    test_period = np.arange(len(all_data)) < last_test_row
                    bool_ind = train_length_ok & test_period
                    with open(log_file, 'a') as f:
                        f.write('\nMASE {}: {}'.format(train_length,
                                                   np.mean(mase_arr[bool_ind][np.logical_not(np.isinf(mase_arr[bool_ind]))])))
                        f.write('\nSMAPE {}: {}'.format(train_length,
                                                    np.mean(smape_arr[bool_ind][np.logical_not(np.isinf(smape_arr[bool_ind]))])))

