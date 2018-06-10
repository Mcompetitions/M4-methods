""" Contains unit tests for the driver module. """

import unittest
from unittest import mock
import driver as d
import numpy as np
import predictor as p
import pandas as pd
import os

class TestSeasonalDecomposition(unittest.TestCase):
    """Test Seasonal Decomposition Fucntion, wich in turn calls R STL function"""
    
    def test_correct_data(self):
        """Basic ordinary data test"""
        ts = pd.Series([1.2, 3.2, 3.2, 4.5, 3.4, 2.1, 3.4, 7.8, 5.4, 3.2, 1.2, 4.5, 6.8, 3.2, 1.2,
                        5.6, 4.3])
        res,season = d.decompose(ts, 4)
        pd.testing.assert_series_equal(season, pd.Series([0.6577939, -0.7280809, -1.5487618, 1.6190541,
                                                          0.6577939, -0.7280809, -1.5487618, 1.6190541,
                                                          0.6577939, -0.7280809, -1.5487618, 1.6190541,
                                                          0.6577939, -0.7280809, -1.5487618, 1.6190541,
                                                          0.6577939]))
        

class TestMakeForecast(unittest.TestCase):
    def setUp(self):
        self.basic_forecast = {'zlib': [1.,2.,3.,4.], 'rp': [3.,4.,5.,6.]}
        self.basic_decomposition = (pd.Series([10, 15, 20, 25, 30, 35, 40, 45, 50]),
                                    pd.Series([0, 1, 2, 3, 0, 1, 2, 3, 0]))
        self.time_series = pd.Series([1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5, 5.0])
        self.discrete_time_series = pd.Series([1, 5, 2, 5, 3, 3, 4, 4, 5])
        
    @mock.patch('predictor.make_forecast_multialphabet')
    @mock.patch('driver.driver.decompose')
    def test_mock_without_season(self, mocked_decompose, mocked_make_forecast):
        mocked_decompose.return_value = self.basic_decomposition
        mocked_make_forecast.return_value = self.basic_forecast
        expected_result = pd.DataFrame({'zlib': [1.,2.,3.,4.], 'rp': [3.,4.,5.,6.]})
        
        res = d.forecast_with_preprocessing(ts=self.time_series, h=4, frequency=None,
                                            groups=['zlib', 'rp'], sparse=-1, quants_count=4,
                                            difference=0, smooth_func=d.smooth_none,
                                            forecast_func=p.make_forecast_multialphabet)
        pd.testing.assert_frame_equal(res, expected_result)


    @mock.patch('predictor.make_forecast_discrete')
    def test_mock_discrete(self, mocked_make_forecast):
        mocked_make_forecast.return_value = {'zlib': [int(i) for i in self.basic_forecast['zlib']],
                                             'rp': [int(i) for i in self.basic_forecast['rp']]}
        expected_result = pd.DataFrame({'zlib': [1, 2, 3, 4], 'rp': [3, 4, 5, 6]})
        
        res = d.forecast_with_preprocessing(ts=self.discrete_time_series, h=4, frequency=None,
                                            groups=['zlib', 'rp'], sparse=-1, quants_count=4,
                                            difference=0, smooth_func=d.smooth_none,
                                            forecast_func=p.make_forecast_discrete)
        pd.testing.assert_frame_equal(res, expected_result)
        mocked_make_forecast.assert_called_with([1, 5, 2, 5, 3, 3, 4, 4, 5], difference=0,
                                                groups=['zlib', 'rp'], h=4, sparse=-1)


    @mock.patch('predictor.make_forecast_multialphabet')
    @mock.patch('driver.driver.decompose')
    def test_mock_with_season(self, mocked_decompose, mocked_make_forecast):
        mocked_decompose.return_value = self.basic_decomposition
        mocked_make_forecast.return_value = self.basic_forecast
        expected_result = pd.DataFrame({'zlib': [2, 4, 6, 4], 'rp': [4, 6, 8, 6]}, dtype=float)
        
        res = d.forecast_with_preprocessing(ts=self.time_series, h=4, frequency=4,
                                            groups=['zlib', 'rp'], sparse=-1, quants_count=4,
                                            difference=0, smooth_func=d.smooth_none,
                                            forecast_func=p.make_forecast_multialphabet)
        pd.testing.assert_frame_equal(res, expected_result)
        mocked_make_forecast.assert_called_with([10, 15, 20, 25, 30, 35, 40, 45, 50], difference=0,
                                                groups=['zlib', 'rp'], h=4, max_quants_count=4,
                                                sparse=-1)

        
class MapeTest(unittest.TestCase):
    """Test MAPE function."""

    def test_m3c_year(self):
        # zstd+ppmd d = 1, smoothing = False, sparse = 2, 
        observed = pd.Series([5379.75, 6158.68, 6876.58, 7851.91, 8407.84, 9156.01])
        predicted = pd.Series([5427.0124308808, 5917.0363290153, 6407.0594768841, 6262.0988838384,
                               6165.6275143097, 6999.809906989])
        pd.testing.assert_series_equal(d.mape(predicted, observed),
                                       pd.Series([0.8746825182, 4.0021422233, 7.0691548643,
                                                  22.5281297362, 30.7711597599, 26.6925492538]))


class FileReadingTest(unittest.TestCase):
    """Test functions for input reading"""

    def test_single_series_reading(self):
        os.chdir(os.path.dirname(__file__))        
        ts_dframe = d.ts_from_file('single_ts.dat')
        self.assertEqual(ts_dframe.columns.values, ['1'])
        pd.testing.assert_series_equal(ts_dframe['1'],
                                       pd.Series([6, 940.66, 1084.86, 1244.98, 1445.02, 1683.17,
                                                  2038.15, 2342.52, 2602.45, 2927.87, 3103.96, 3360.27,
                                                  3807.63, 4387.88, 4936.99], name='1'))


    def test_multiple_series_reading(self):
        os.chdir(os.path.dirname(__file__))        
        ts_dframe = d.ts_from_file('multi_ts.dat')
        np.testing.assert_array_equal(ts_dframe.columns.values,
                                      ['1402', '1403', '1404', '1405', '1406', '1407'])
        pd.testing.assert_series_equal(ts_dframe['1402'],
                                       pd.Series([18, 2640, 2640, 2160, 4200, 3360, 2400, 3600, 1920,
                                                  4200, 4560, 480, 3720, 5640, 2880, 1800, 3120, 2400,
                                                  2520, 9000, 2640, 3120, 2880, 8760, 5160, 2160, 8280,
                                                  4920, 3120, 6600, 4080, 5880, 1680, 6720, 2040, 6480,
                                                  1920, 3600, 2040, 2760, 3840, 960, 2280, 1320, 2160,
                                                  4800, 3000, 3120, 5880, 2640, 2400], name='1402',
                                                 dtype=float))
        pd.testing.assert_series_equal(ts_dframe['1403'],
                                       pd.Series([18, 1680, 1920, 120, 1080, 840, 1440, 480, 720, 4080,
                                                  1560, 480, 720, 6120, 2040, 3960, 2160, 120, 1200,
                                                  1080, 1080, 1080, 2160, 240, 1440, 1200, 1560, 2520,
                                                  600, 1560, 3240, 7440, 480, 2640, 960, 3120, 1200,
                                                  960, 480, 600, 120, 2640, 720, 600, 840, 1320, 2160,
                                                  1200, 1800, 1320, 600], name='1403', dtype=float))
        pd.testing.assert_series_equal(ts_dframe['1404'],
                                       pd.Series([18, 1140, 720, 4860, 1200, 3150, 2130, 1800, 2010,
                                                  2880, 1650, 1740, 4440, 7410, 3000, 4410, 4980, 2040,
                                                  3000, 5430, 4650, 2520, 2250, 2100, 5730, 2550, 6450,
                                                  1050, 3240, 3960, 3030, 2850, 4380, 4080, 4140, 5160,
                                                  5100, 3480, 4350, 5550, 4200, 5640, 4980, 3810, 3540,
                                                  8760, 5610, 6090, 4980, 7680, 5550], name='1404',
                                                 dtype=float))
        pd.testing.assert_series_equal(ts_dframe['1405'],
                                       pd.Series([18, 180, 940, 2040, 800, 1000, 520, 500, 400, 1760,
                                                  1520, 1400, 1840, 3360, 220, 6000, 3160, 1060, 820,
                                                  2860, 440, 1880, 220, 1460, 4620, 1360, 3220, 1500,
                                                  1940, 3100, 2340, 2540, 1720, 1560, 4580, 1340, 3860,
                                                  2440, 2180, 2020, 4160, 2760, 3480, 2240, 5900, 6720,
                                                  7500, 4880, 7780, 4360, 4640], name='1405',
                                                 dtype=float))
        pd.testing.assert_series_equal(ts_dframe['1406'],
                                       pd.Series([18, 2000, 1550, 4450, 3050, 3050, 2250, 2200, 2450,
                                                  4900, 5300, 600, 12050, 4050, 1900, 3200, 7400, 10650,
                                                  9850, 4000, 11500, 2750, 5700, 3900, 9200, 3000, 5550,
                                                  3300, 4700, 6250, 4550, 8550, 4400, 5200, 3800, 3750,
                                                  11100, 6800, 6600, 5100, 5400, 11400, 9400, 4150,
                                                  7500, 11150, 7250, 5650, 12000, 11550, 7450],
                                                 name='1406', dtype=float))
        pd.testing.assert_series_equal(ts_dframe['1407'],
                                       pd.Series([18, 1200, 2850, 1350, 1500, 1950,1950, 600, 1650,
                                                  2250, 3000, 1500, 2400, 300, 2250, 1500, 2100, 3600,
                                                  3300, 4500, 150, 5700, 600, 750, 1650, 150, 1950,
                                                  1800, 1350, 1200, 2550, 1200, 2550, 2550, 1350, 3150,
                                                  1500, 3150, 3150, 1500, 3150, 2550, 2250, 3750, 450,
                                                  1350, 2100, 1800, 6750, 2400, 4200], name='1407',
                                                 dtype=float))

    def test_m3c_year(self):
        os.chdir(os.path.dirname(__file__))        
        ts_dframe = d.ts_from_file('m3c_year_data.dat')
        self.assertEqual(len(ts_dframe.columns.values), 645)

        observed_dframe = d.ts_from_file('m3c_observed.dat')
        for col in ts_dframe.columns:
            self.assertTrue(col in observed_dframe.columns)
            

    def test_row_splitting(self):
        ts_dframe = d.ts_from_file('single_ts.dat')
        h,ts = d.split_row_without_frequency(ts_dframe['1'].dropna())
        self.assertEqual(h, 6)
        np.testing.assert_array_almost_equal(ts,
                                             np.array([940.66, 1084.86, 1244.98, 1445.02, 1683.17,
                                                       2038.15, 2342.52, 2602.45, 2927.87, 3103.96,
                                                       3360.27, 3807.63, 4387.88, 4936.99]))
        

class TestParallelization(unittest.TestCase):

    @mock.patch('driver.driver.forecast_chunk', return_value=[{'zlib': [1,2,3,4], 'rp': [3,4,5,6]}])
    def test_chunks_splitting(self, mocked_forecast_chunk):
        comm_mock = mock.Mock()
        comm_mock.Get_size.return_value = 2
        comm_mock.Get_rank.return_value = 1
        ts_data = pd.DataFrame({'1': [1,2,3], '2': [3,4,5], '3': [4,5,6]})
        groups = ['zlib', 'rp']
        self.assertIsNone(d.forecast_ts_array_mpi(comm_mock, ts_data, groups, -1, 4, 1,
                                                d.smooth_none, p.make_forecast_multialphabet))
        mocked_forecast_chunk.assert_called_with(1, 3, ts_data, groups, -1, 4, 1,
                                                 d.smooth_none, p.make_forecast_multialphabet)

        comm_mock.Get_rank.return_value = 0
        comm_mock.gather.return_value = [[('1', {'zlib': [1, 2], 'rp': [3, 4]})],
                                         [('2', {'zlib': [5, 6], 'rp': [7, 8]}),
                                          ('3', {'zlib': [9, 10], 'rp': [11, 12]})]]
        self.assertListEqual(d.forecast_ts_array_mpi(comm_mock, ts_data,
                                                     groups, -1, 4, 1, d.smooth_none,
                                                     p.make_forecast_multialphabet),
                             [('1', {'zlib': [1, 2], 'rp': [3, 4]}),
                              ('2', {'zlib': [5, 6], 'rp': [7, 8]}),
                              ('3', {'zlib': [9, 10], 'rp': [11, 12]})])
        mocked_forecast_chunk.assert_called_with(0, 1, ts_data,
                                                 groups, -1, 4, 1, d.smooth_none,
                                                 p.make_forecast_multialphabet)
        

class TestMeans(unittest.TestCase):

    def test_equal_length(self):
        forecast1 = {'zlib': np.array([1, 2, 3, 4, 5]), 'rp': np.array([6, 7, 8, 9, 10])}
        forecast2 = {'zlib': np.array([10, 20, 30, 40, 50]), 'rp': np.array([60, 70, 80, 90, 100])}
        forecast3 = {'zlib': np.array([100, 200, 300, 400, 500]),
                     'rp': np.array([600, 700, 800, 900, 1000])}
        data = [('1', forecast1), ('2', forecast2), ('3', forecast3)]
        means = d.compute_means(data, 'zlib')
        np.testing.assert_array_almost_equal(means, np.array([(1+10+100)/3, (2+20+200)/3, (3+30+300)/3,
                                                              (4+40+400)/3, (5+50+500)/3]))


class TestDiscreteCase(unittest.TestCase):
    """Test forecasting of discrete time series from file in M3C format."""

    @mock.patch('predictor.make_forecast_discrete')
    def test_basic_forecast_from_file(self, mocked_make_forecast):
        comm = mock.MagicMock()
        comm.Get_rank.return_value = 0
        comm.Get_size.return_value = 1
        mocked_make_forecast.return_value = {'zlib': [1,2], 'ppmd': [3,4], 'rp': [5,6],
                                             'zlib_ppmd_rp': [7,8]}
        d.forecast_discrete_from_file(comm, 'kindex_ts.dat',
                                      ['zlib', 'ppmd','rp', 'zlib_ppmd_rp'], sparse=-1, quants_count=16,
                                      difference=0, smooth_func=d.smooth_none)
        mocked_make_forecast.assert_called_with([2,1,1,1,1,1,2,2,3,1,1,2,2,2,3,4,5], difference=0,
                                                groups=['zlib', 'ppmd', 'rp', 'zlib_ppmd_rp'],
                                                h=2, sparse=-1)

if __name__ == '__main__':
    unittest.main()
