"""Script to make predictions by loading trained models."""

import keras as ks
import pandas as pd
import numpy as np
import metrics
from collections import namedtuple
import tensorflow as tf
import datetime as dt
import os
from scipy import stats
from utilities import *
from model_definitions import *


model_arr = get_model_params()

fc_arr = {'fc': [], 'lower': [], 'upper': []}
for m in model_arr:

    # read data
    series = pd.read_csv(os.path.join('data', '{}-train.csv'.format(m.freq_name)), header=0, index_col=0)
    num_values = series.notnull().sum(axis=1)

    # dictionary of predictions
    prediction = dict()
    valid_prediction = dict()

    # loop over the different training lengths
    training_lengths = m.training_lengths
    for series_length in training_lengths:
        horizon = m.horizon

        # data for prediction
        train_x = get_base_data(series, series_length, 0, m.cycle_length)
        train_x, train_mean, train_std = normalise(train_x)

        # data to measure errors calculating the prediction intervals
        valid_x = get_base_data(series, series_length, horizon, m.cycle_length)[:, :-horizon]
        valid_x, valid_mean, valid_std = normalise(valid_x)

        # only predict on series that are long enough
        train_length_ok = np.logical_or(series_length == min(training_lengths), num_values.values >= series_length)
        valid_length_ok = np.logical_or(series_length == min(training_lengths),
                                        num_values.values >= series_length + horizon)

        # initialise array for forecast
        prediction[series_length] = np.zeros([len(train_x), horizon])
        curr_prediction = prediction[series_length]
        curr_prediction[:] = np.nan

        # initialise prediction array for training period, used to calculate prediction intervals
        valid_prediction[series_length] = np.zeros([len(train_x), horizon])
        curr_valid_prediction = valid_prediction[series_length]
        curr_valid_prediction[:] = np.nan

        # different models for each future time step
        for horizon_step in range(horizon):
            print(m.freq_name, series_length, horizon_step)

            # clear session and reset default graph, as suggested here, to speed up prediction
            # https://stackoverflow.com/questions/45796167/training-of-keras-model-gets-slower-after-each-repetition
            ks.backend.clear_session()
            tf.reset_default_graph()

            # load model and predict
            model_file = os.path.join('trained_models',
                                      '{}_length_{}_step_{}.h5'.format(m.freq_name, series_length,
                                                                       horizon_step))
            est = ks.models.load_model(model_file)
            curr_prediction[:, horizon_step] = np.where(train_length_ok, est.predict(train_x).flatten(),
                                                        curr_prediction[:, horizon_step])
            curr_valid_prediction[:, horizon_step] \
                = np.where(valid_length_ok, est.predict(valid_x).flatten(),
                           curr_valid_prediction[:, horizon_step])

        # denormalise
        prediction[series_length] = denormalise(prediction[series_length], train_mean, train_std)
        valid_prediction[series_length] = denormalise(valid_prediction[series_length], valid_mean, valid_std)

    # blend predictions
    blend_predictions(prediction, num_values.values)
    blend_predictions(valid_prediction, num_values.values - horizon)

    # calculate prediction intervals
    actual = get_base_data(series, horizon, 0, m.cycle_length)
    err = (actual - valid_prediction[-1]) / valid_prediction[-1]
    lower = np.percentile(err, 2.5, axis=0)
    upper = np.percentile(err, 97.5, axis=0)

    for fc_type in fc_arr:
        output = pd.DataFrame(index=series.index, columns=['F' + str(i) for i in range(1, 49)])
        output.index.name = 'id'
        if fc_type == 'fc':
            output.iloc[:, :m.horizon] = prediction[-1]
        elif fc_type == 'lower':
            output.iloc[:, :m.horizon] = prediction[-1] * (1 + lower)
        elif fc_type == 'upper':
            output.iloc[:, :m.horizon] = prediction[-1] * (1 + upper)
        fc_arr[fc_type].append(output)

for fc_type in fc_arr:
    # order as in sample submission and save
    all_output = pd.concat(fc_arr[fc_type], axis=0)
    sample = pd.read_csv(os.path.join('data', 'template_Naive.csv'), index_col=0)
    all_output = all_output.reindex(sample.index)
    all_output.to_csv('Submission_{}_{}.csv'.format(fc_type, dt.datetime.now().strftime('%y%m%d_%H%M')))
