
import keras as ks
import pandas as pd
import numpy as np
import metrics
from collections import namedtuple
import tensorflow as tf
import datetime as dt
import os
from utilities import *


def yearly_model(series_length):
    input = ks.layers.Input((series_length,))
    yearly_input = ks.layers.Reshape((series_length, 1))(input)
    yearly_hidden1 = ks.layers.Dense(units=50, activation='relu')(ks.layers.Flatten()(yearly_input))
    yearly_hidden2 = ks.layers.Dense(units=50, activation='relu')(yearly_hidden1)
    yearly_output = ks.layers.Dense(units=1, activation='linear')(yearly_hidden2)
    est = ks.Model(inputs=input, outputs=yearly_output)
    est.compile(optimizer=ks.optimizers.Adam(lr=0.001), loss='mse', metrics=['mse'])
    epochs = 250
    batch_size = 1000
    return est, epochs, batch_size


def quarterly_model(series_length):
    input = ks.layers.Input((series_length,))
    yearly_input = ks.layers.Reshape((series_length, 1))(input)
    yearly_avg = ks.layers.AveragePooling1D(pool_size=4, strides=4, padding='valid')(yearly_input)
    yearly_hidden1 = ks.layers.Dense(units=50, activation='relu')(ks.layers.Flatten()(yearly_avg))
    yearly_hidden2 = ks.layers.Dense(units=50, activation='relu')(yearly_hidden1)
    yearly_output = ks.layers.Dense(units=1, activation='linear')(yearly_hidden2)

    yearly_avg_up = ks.layers.UpSampling1D(size=4)(yearly_avg)
    periodic_diff = ks.layers.Subtract()([input, ks.layers.Flatten()(yearly_avg_up)])
    periodic_input = ks.layers.Reshape((series_length // 4, 4, 1))(periodic_diff)
    periodic_conv = ks.layers.Conv2D(filters=3, kernel_size=(1, 4), strides=(1, 4),
                                     padding='valid')(periodic_input)
    periodic_hidden1 = ks.layers.Dense(units=50, activation='relu')(ks.layers.Flatten()(periodic_conv))
    periodic_hidden2 = ks.layers.Dense(units=50, activation='relu')(periodic_hidden1)
    periodic_output = ks.layers.Dense(units=1, activation='linear')(periodic_hidden2)
    output = ks.layers.Add()([yearly_output, periodic_output])
    est = ks.Model(inputs=input, outputs=output)
    est.compile(optimizer=ks.optimizers.Adam(lr=0.001), loss='mse', metrics=['mse'])
    epochs = 250
    batch_size = 1000
    return est, epochs, batch_size


def monthly_model(series_length):
    input = ks.layers.Input((series_length,))
    yearly_input = ks.layers.Reshape((series_length, 1))(input)
    yearly_avg = ks.layers.AveragePooling1D(pool_size=12, strides=12, padding='valid')(yearly_input)
    yearly_hidden1 = ks.layers.Dense(units=20, activation='relu')(ks.layers.Flatten()(yearly_avg))
    yearly_hidden2 = ks.layers.Dense(units=20, activation='relu')(yearly_hidden1)
    yearly_output = ks.layers.Dense(units=1, activation='linear')(yearly_hidden2)

    yearly_avg_up = ks.layers.UpSampling1D(size=12)(yearly_avg)
    periodic_diff = ks.layers.Subtract()([input, ks.layers.Flatten()(yearly_avg_up)])
    periodic_input = ks.layers.Reshape((series_length // 12, 12, 1))(periodic_diff)
    periodic_conv = ks.layers.Conv2D(filters=3, kernel_size=(1, 12), strides=(1, 12),
                                     padding='valid')(periodic_input)
    periodic_hidden1 = ks.layers.Dense(units=20, activation='relu')(ks.layers.Flatten()(periodic_conv))
    periodic_hidden2 = ks.layers.Dense(units=20, activation='relu')(periodic_hidden1)
    periodic_output = ks.layers.Dense(units=1, activation='linear')(periodic_hidden2)
    output = ks.layers.Add()([yearly_output, periodic_output])
    est = ks.Model(inputs=input, outputs=output)
    est.compile(optimizer=ks.optimizers.Adam(lr=0.001), loss='mse', metrics=['mse'])
    epochs = 250
    batch_size = 1000
    return est, epochs, batch_size


def weekly_model(series_length):
    if series_length == 52:
        input = ks.layers.Input((series_length,))
        hidden1 = ks.layers.Dense(units=20, activation='relu')(input)
        hidden2 = ks.layers.Dense(units=20, activation='relu')(hidden1)
        output = ks.layers.Dense(units=1, activation='linear')(hidden2)
        est = ks.Model(inputs=input, outputs=output)
        est.compile(optimizer=ks.optimizers.Adam(lr=0.001), loss='mse', metrics=['mse'])
        epochs = 250
        batch_size = 1000
    else:
        input = ks.layers.Input((series_length,))
        yearly_input = ks.layers.Reshape((series_length, 1))(input)
        yearly_avg = ks.layers.AveragePooling1D(pool_size=52, strides=52, padding='valid')(yearly_input)
        yearly_hidden1 = ks.layers.Dense(units=20, activation='relu')(ks.layers.Flatten()(yearly_avg))
        yearly_hidden2 = ks.layers.Dense(units=20, activation='relu')(yearly_hidden1)
        yearly_output = ks.layers.Dense(units=1, activation='linear')(yearly_hidden2)

        yearly_avg_up = ks.layers.UpSampling1D(size=52)(yearly_avg)
        periodic_diff = ks.layers.Subtract()([input, ks.layers.Flatten()(yearly_avg_up)])
        periodic_input = ks.layers.Reshape((series_length // 52, 52, 1))(periodic_diff)
        periodic_conv = ks.layers.Conv2D(filters=3, kernel_size=(1, 52), strides=(1, 52),
                                         padding='valid')(periodic_input)
        periodic_hidden1 = ks.layers.Dense(units=20, activation='relu')(ks.layers.Flatten()(periodic_conv))
        periodic_hidden2 = ks.layers.Dense(units=20, activation='relu')(periodic_hidden1)
        periodic_output = ks.layers.Dense(units=1, activation='linear')(periodic_hidden2)
        output = ks.layers.Add()([yearly_output, periodic_output])
        est = ks.Model(inputs=input, outputs=output)
        est.compile(optimizer=ks.optimizers.Adam(lr=0.001), loss='mse', metrics=['mse'])
        epochs = 250
        batch_size = 1000
    return est, epochs, batch_size


def daily_model(series_length):
    input = ks.layers.Input((series_length,))
    weekly_input = ks.layers.Reshape((series_length, 1))(input)
    weekly_avg = ks.layers.AveragePooling1D(pool_size=7, strides=7, padding='valid')(weekly_input)
    weekly_hidden1 = ks.layers.Dense(units=20, activation='relu')(ks.layers.Flatten()(weekly_avg))
    weekly_hidden2 = ks.layers.Dense(units=20, activation='relu')(weekly_hidden1)
    weekly_output = ks.layers.Dense(units=1, activation='linear')(weekly_hidden2)

    weekly_avg_up = ks.layers.UpSampling1D(size=7)(weekly_avg)
    periodic_diff = ks.layers.Subtract()([input, ks.layers.Flatten()(weekly_avg_up)])
    periodic_input = ks.layers.Reshape((series_length // 7, 7, 1))(periodic_diff)
    periodic_conv = ks.layers.Conv2D(filters=3, kernel_size=(1, 7), strides=(1, 7),
                                     padding='valid')(periodic_input)
    periodic_hidden1 = ks.layers.Dense(units=20, activation='relu')(ks.layers.Flatten()(periodic_conv))
    periodic_hidden2 = ks.layers.Dense(units=20, activation='relu')(periodic_hidden1)
    periodic_output = ks.layers.Dense(units=1, activation='linear')(periodic_hidden2)
    output = ks.layers.Add()([weekly_output, periodic_output])
    est = ks.Model(inputs=input, outputs=output)
    est.compile(optimizer=ks.optimizers.Adam(lr=0.001), loss='mse', metrics=['mse'])
    epochs = 250
    batch_size = 1000
    return est, epochs, batch_size


def hourly_model(series_length):
    input = ks.layers.Input((series_length,))
    weekly_input = ks.layers.Reshape((series_length, 1))(input)
    weekly_avg = ks.layers.AveragePooling1D(pool_size=168, strides=168, padding='valid')(weekly_input)
    weekly_hidden1 = ks.layers.Dense(units=20, activation='relu')(ks.layers.Flatten()(weekly_avg))
    weekly_hidden2 = ks.layers.Dense(units=20, activation='relu')(weekly_hidden1)
    weekly_output = ks.layers.Dense(units=1, activation='linear')(weekly_hidden2)
    weekly_avg_up = ks.layers.UpSampling1D(size=168)(weekly_avg)

    daily_diff = ks.layers.Subtract()([input, ks.layers.Flatten()(weekly_avg_up)])
    daily_diff_input = ks.layers.Reshape((series_length // 7, 7, 1))(daily_diff)
    daily_diff_conv = ks.layers.Conv2D(filters=3, kernel_size=(1, 7), strides=(1, 7),
                                       padding='valid')(daily_diff_input)
    daily_diff_hidden1 = ks.layers.Dense(units=20, activation='relu')(ks.layers.Flatten()(daily_diff_conv))
    daily_diff_hidden2 = ks.layers.Dense(units=20, activation='relu')(daily_diff_hidden1)
    daily_diff_output = ks.layers.Dense(units=1, activation='linear')(daily_diff_hidden2)

    daily_avg = ks.layers.AveragePooling1D(pool_size=24, strides=24, padding='valid')(weekly_input)
    daily_avg_up = ks.layers.UpSampling1D(size=24)(daily_avg)
    hourly_diff = ks.layers.Subtract()([input, ks.layers.Flatten()(daily_avg_up)])
    hourly_diff_input = ks.layers.Reshape((series_length // 24, 24, 1))(hourly_diff)
    hourly_diff_conv = ks.layers.Conv2D(filters=3, kernel_size=(1, 24), strides=(1, 24),
                                        padding='valid')(hourly_diff_input)
    hourly_diff_hidden1 = ks.layers.Dense(units=20, activation='relu')(ks.layers.Flatten()(hourly_diff_conv))
    hourly_diff_hidden2 = ks.layers.Dense(units=20, activation='relu')(hourly_diff_hidden1)
    hourly_diff_output = ks.layers.Dense(units=1, activation='linear')(hourly_diff_hidden2)

    output = ks.layers.Add()([weekly_output, daily_diff_output, hourly_diff_output])

    est = ks.Model(inputs=input, outputs=output)
    est.compile(optimizer=ks.optimizers.Adam(lr=0.001), loss='mse', metrics=['mse'])
    epochs = 250
    batch_size = 1000
    return est, epochs, batch_size


def get_model_params():
    # define the model parameters
    Model = namedtuple('Model', ['freq_name', 'horizon', 'freq', 'model_constructor', 'training_lengths',
                                 'cycle_length', 'augment'])
    yearly = Model('Yearly', 6, 1, yearly_model, [10, 20, 30], 1, False)
    quarterly = Model('Quarterly', 8, 4, quarterly_model, [20, 48, 100], 4, False)
    monthly = Model('Monthly', 18, 12, monthly_model, [48, 120, 240], 12, False)
    weekly = Model('Weekly', 13, 1, weekly_model, [52, 520, 1040], 52, True)
    daily = Model('Daily', 14, 1, daily_model, [98], 7, True)
    hourly = Model('Hourly', 48, 24, hourly_model, [672], 7*24, True)
    return [yearly, quarterly, monthly, weekly, daily, hourly]

