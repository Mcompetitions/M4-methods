"""Python interface to the C++ library for time series forecasting based on compression."""

import math
import numpy as np
import predictor as p
import pandas as pd
import itertools as it
import time
from rpy2.robjects import r
import rpy2.robjects.numpy2ri

rpy2.robjects.numpy2ri.activate()


def decompose(series, frequency, s_window='periodic', **kwargs):
    '''
    Decompose a time series into seasonal, trend and irregular components using loess, 
    acronym STL.
    https://www.rdocumentation.org/packages/stats/versions/3.4.3/topics/stl

    params:
        series: a time series

        frequency: the number of observations per “cycle” 
                   (normally a year, but sometimes a week, a day or an hour)
                   https://robjhyndman.com/hyndsight/seasonal-periods/

        s_window: either the character string "periodic" or the span 
                 (in lags) of the loess window for seasonal extraction, 
                 which should be odd and at least 7, according to Cleveland 
                 et al.

        log:    boolean.  take log of series



        **kwargs:  See other params for stl at 
           https://www.rdocumentation.org/packages/stats/versions/3.4.3/topics/stl
    '''

    assert(isinstance(series, pd.Series))
    length = len(series)
    s = r.ts(series.values, frequency=frequency)
    decomposed = [x for x in r.stl(s, s_window, **kwargs).rx2('time.series')]
    return pd.Series(decomposed[length:2*length]) + pd.Series(decomposed[2*length:3*length]), pd.Series(decomposed[0:length])


def select_difference(time_series):
    """Selects best order of difference to the time series."""

    assert(len(time_series > 0))
    assert(isinstance(time_series, pd.Series))

    if len(time_series) <= 2:
        return 0

    d0 = np.std(time_series)
    d1 = np.std(np.diff(time_series, 1))

    if len(time_series) > 3:
        d2 = np.std(np.diff(time_series, 2))
    else:
        d2 = float("inf")

    if min(d0, d1, d2) == d0:
        return 0
    elif min(d1, d2) == d1:
        return 1
    return 2


def add_seasonality_to_forecast(forecast, season, frequency):
    """Add seasonal component to the forecast. Season parameter should contain seasonal component for the whole
     time series."""

    assert(len(forecast) > 0)
    assert(isinstance(forecast, pd.Series))
     
    k = len(forecast)
    ts_len = len(season)
    for i in range(0, k):
        forecast[i] += season[(ts_len + i) % frequency]

         
def smooth_m3c(ts):
    """Simple smoothing of the time series: t_i = (2*t_{i} + t_{i-1} + t_{i-2})/4."""
    assert(len(ts) > 1)
    assert(isinstance(ts, pd.Series))

    tmp = pd.Series(np.zeros(len(ts) - 2))
    for i in range(2, len(ts)):
        tmp.values[i - 2] = (2 * ts.values[i] + ts.values[i - 1] + ts.values[i - 2]) / 4
    return tmp


def smooth_none(ts):
    return ts


def smooth_sunspots(ts):
    """Simple smoothing of the time series: t_i = (2*t_{i} + t_{i-1} + t_{i-2})/4."""
    assert(len(ts) > 1)
    assert(isinstance(ts, pd.Series))

    tmp = pd.Series(np.zeros(len(ts) - 2))
    for i in range(2, len(ts)):
        tmp.values[i - 2] = (ts.values[i] + ts.values[i - 1] + ts.values[i - 2]) / 3
    return tmp


def forecast_with_preprocessing(ts, h, frequency, groups, sparse, quants_count,
                                difference, smooth_func, forecast_func):
    assert(isinstance(ts, pd.Series))
    
    if difference is None:
        difference = select_difference(ts)

    season = None
    if frequency is not None:
        ts,season = decompose(ts, frequency, robust=True)

    ts = smooth_func(ts)

    if forecast_func == p.make_forecast_discrete:
        ts = [int(x) for x in ts.values.tolist()]
        res = forecast_func(ts, groups=groups, difference=difference, h=h,
                            sparse=sparse)
    else:
        res = forecast_func(ts.values.tolist(), groups=groups, difference=difference, h=h,
                                sparse=sparse, max_quants_count=quants_count)

    if season is not None:
        for group in groups:
            res[group] = pd.Series(res[group])
            add_seasonality_to_forecast(res[group], season, frequency)

    return pd.DataFrame(res)


def ts_from_file(filename):
    """Reads array of time series data from the specified file."""

    data = pd.DataFrame()
    with open(filename, 'r') as f:
        for line in f:
            row_list = line.strip().split(' ')
            column = row_list[0]
            row_list = [float(x) for x in row_list[1:]]
            data = pd.concat([data, pd.DataFrame({column: row_list})], axis=1, ignore_index=False)

    return data


def split_row_with_frequency(row):
    assert(isinstance(row, pd.Series))
    
    h = int(row[0])
    frequency = int(row[1])
    if frequency == 1:
        frequency = None
    ts = row[2:]
    return h,frequency,ts


def split_row_without_frequency(row):
    assert(isinstance(row, pd.Series))
    
    h = int(row[0])
    ts = row[1:]
    return h,ts
        

def forecast_chunk(range_begin, range_end, ts_data, groups, sparse, quants_count, difference,
                   smooth_func, forecast_func):
    results = []
    for i in range(range_begin, range_end):
        curr_column = ts_data.columns.values[i]
        h,frequency,ts = split_row_with_frequency(ts_data[curr_column].dropna())
        curr_ts_forecast = forecast_with_preprocessing(ts, h, frequency, groups=groups, sparse=sparse,
                                                       quants_count=quants_count,
                                                       difference=difference,
                                                       smooth_func=smooth_func,
                                                       forecast_func=forecast_func)
        results.append((curr_column, curr_ts_forecast))

    return results
    
    
def forecast_ts_array_mpi(comm, ts_data, groups, sparse, quants_count, difference, smooth_func,
                          forecast_func):
    size = comm.Get_size()
    rank = comm.Get_rank()

    chunk_size = int(math.floor(len(ts_data.columns) / size))
    assert(chunk_size > 0)

    chunk_begin = chunk_size*rank
    if rank + 1 < size:
        chunk_end = chunk_size*(rank+1)
    else:
        chunk_end = len(ts_data.columns)

    results = forecast_chunk(chunk_begin, chunk_end, ts_data, groups, sparse, quants_count,
                             difference, smooth_func, forecast_func)
            
    common_results = comm.gather(results, root=0)

    if rank == 0:
        common_results = list(it.chain.from_iterable(common_results))
        return common_results

    return None


def forecast_from_memory(comm, ts_data, groups, sparse, quants_count, difference=None,
                         smooth_func=smooth_none, forecast_func=p.make_forecast_multialphabet):
    if comm.Get_rank() == 0:
        results = forecast_ts_array_mpi(comm, ts_data, groups, sparse, quants_count, difference,
                                        smooth_func, forecast_func)
        assert(results is not None)
        return results


def forecast_real_from_file(comm, ts_file_name, groups, sparse, quants_count, difference=None,
                            smooth_func=smooth_none):
    ts_data = ts_from_file(ts_file_name)
    return forecast_from_memory(comm, ts_data, groups, sparse, quants_count, difference,
                                smooth_func, p.make_forecast_real)

    
def forecast_multialphabet_from_file(comm, ts_file_name, groups, sparse, quants_count, difference=None,
                                     smooth_func=smooth_none):
    ts_data = ts_from_file(ts_file_name)
    return forecast_from_memory(comm, ts_data, groups, sparse, quants_count, difference,
                                smooth_func, p.make_forecast_multialphabet)


def forecast_discrete_from_file(comm, ts_file_name, groups, sparse, quants_count, difference=None,
                                smooth_func=smooth_none):
    ts_data = ts_from_file(ts_file_name)
    return forecast_from_memory(comm, ts_data, groups, sparse, quants_count, difference, smooth_func,
                                forecast_func=p.make_forecast_discrete)


def print_summary(results, group):
    print("----- " + group + " -----")
    for result in results:
        print(str(result[0]) + ' ' + ' '.join(map(str, result[1][group])))

    means = compute_means(results, group)
    print("Means: " + ' '.join(map(str, means)))
    print("Mean of means: " + str(np.average(means)))
    

def mape(predicted, observed):
    """Computes Mean Absolute Percentage Error (MAPE)."""

    assert(len(predicted) == len(observed))
    assert(len(predicted) > 0)
    assert(isinstance(predicted, pd.Series))
    assert(isinstance(observed, pd.Series))
    assert((observed > 0).all())

    predicted[predicted < 0] = 0
    return np.abs(predicted - observed) / ((predicted + observed) / 2) * 100


def mae(predicted, observed):
    """Computes Mean Absolute Error."""

    assert(len(predicted) == len(observed))
    assert(len(predicted) > 0)
    assert(isinstance(predicted, pd.Series))
    assert(isinstance(observed, pd.Series))

    return np.abs(predicted - observed)


def compute_means(results, group):
    # length is unknown, array will be dynamically extended
    means = np.array([])
    length_ts_counters = np.array([])
    for result in results:
        # extend is needed
        if len(means) < len(result[1][group]):
            means = np.append(means, np.zeros(len(result[1][group]) - len(means)))
            length_ts_counters = np.append(length_ts_counters,
                                           np.zeros(len(result[1][group]) - len(length_ts_counters)))

        means[:len(result[1][group])] += result[1][group]
        length_ts_counters[:len(result[1][group])] += 1

    means /= length_ts_counters
    return means


def error_for_each_group(forecasts, observed, error_func):
    result = pd.DataFrame()
    for group in forecasts.columns.values:
        result[group] = error_func(forecasts[group], observed)
    return result


def compute_errors(forecasts, observed_fname, error_func):
    observed = ts_from_file(observed_fname)
    result = []
    for ts_id,ts_forecast in forecasts:
        result.append((ts_id, error_for_each_group(ts_forecast, observed[ts_id].dropna(), error_func)))
    return result
