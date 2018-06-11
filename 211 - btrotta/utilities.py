"""Utility functions used by training and prediction code."""

import pandas as pd
import numpy as np


def get_base_data(series, series_length, horizon, cycle_length):
    """Get data from series dataframe, padding series that are too short."""
    all_data = np.zeros([len(series), series_length + horizon])
    for i in range(len(series)):
        row = series.iloc[i].values
        row = row[np.logical_not(np.isnan(row))]
        if len(row) < series_length + horizon:
            num_missing = series_length + horizon - len(row)
            all_data[i, -len(row):] = row[-(series_length + horizon):]
            if num_missing > cycle_length:
                all_data[i, :num_missing] = np.mean(row[:-horizon])  # mean of non-test values
            else:
                all_data[i, :num_missing] = all_data[i, cycle_length:(cycle_length + num_missing)]  # copy from same period in cycle
        else:
            all_data[i, :] = row[-(series_length + horizon):]
    return all_data


def get_extended_data(series, series_length, min_length, horizon, cycle_length, augment=True, test_mode=True):
    """Get data from series dataframe, padding series that are too short."""
    ext_arr = []
    weights = []
    first_row = int(len(series) * 0.2) if test_mode else 0
    for i in range(first_row, len(series)):
        row = series.iloc[i].values
        row = row[np.logical_not(np.isnan(row))]
        if len(row) <= series_length + horizon:
            if series_length == min_length:
                num_missing = series_length + horizon - len(row)
                row_to_add = np.zeros([series_length + horizon])
                row_to_add[-len(row):] = row[-(series_length + horizon):]
                if num_missing > cycle_length:
                    row_to_add[:num_missing] = np.mean(row[:-horizon])  # mean of non-test values
                else:
                    row_to_add[:num_missing] = row[cycle_length - num_missing:cycle_length]  # copy from same period in cycle
                ext_arr.append(row_to_add)
                weights.append(1)
        else:
            num_to_add = cycle_length if augment else 1
            num_extra = min(num_to_add, len(row) - series_length - horizon)
            for j in range(num_extra):
                if j == 0:
                    ext_arr.append(row[-(series_length + horizon):])
                else:
                    ext_arr.append(row[-(series_length + horizon + j):-j])
                weights.append(1 / num_extra)
    train_ext = np.stack(ext_arr, 0)
    train_x_ext = train_ext[:, :-horizon]
    train_y_ext = train_ext[:, -horizon:]
    weights = np.array(weights)
    return train_x_ext, train_y_ext, weights


def normalise(arr):
    mean_arr = np.mean(arr, 1)
    series_length = arr.shape[1]
    std_arr = np.std(arr, axis=1)
    std_arr = np.where(std_arr == 0, mean_arr, std_arr)
    std_arr = np.where(std_arr == 0, np.ones(mean_arr.shape), std_arr)
    norm_arr = (arr - np.repeat(mean_arr[:, np.newaxis], series_length, 1)) \
               / np.repeat(std_arr[:, np.newaxis], series_length, 1)
    return norm_arr, mean_arr, std_arr


def denormalise(norm_arr, mean_arr, std_arr):
    series_length = norm_arr.shape[1]
    denorm_arr = norm_arr * np.repeat(std_arr[:, np.newaxis], repeats=series_length, axis=1) \
                 + np.repeat(mean_arr[:, np.newaxis], repeats=series_length, axis=1)
    return denorm_arr


def blend_predictions(prediction_dict, num_values):
    """Given the original series and dictionary of predictions indexed by the training length,
    produce a blended prediction and store it in prediction[-1]."""

    training_lengths = list(prediction_dict.keys())
    prediction_dict[-1] = np.copy(prediction_dict[min(training_lengths)])
    for series_length in training_lengths:
        train_length_ok = np.logical_or(series_length == min(training_lengths), num_values >= series_length)
        comb_prediction = np.mean(np.stack([prediction_dict[s] for s in training_lengths if s <= series_length], 2),
                                  2)
        prediction_dict[-1] = np.where(np.repeat(train_length_ok[:, np.newaxis], prediction_dict[-1].shape[1], 1),
                                       comb_prediction, prediction_dict[-1])