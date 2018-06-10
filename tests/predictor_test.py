""" Contains unit tests for the driver module. """

import unittest
import driver
import numpy as np
import pandas as pd
import predictor as p


class TestDifferenceSelector(unittest.TestCase):
    """Tests for select_difference."""

    def test_ordinary_data(self):
        """Test ordinary input."""
        ts = pd.Series([2.5, 2.3, 1.1, 0.9, 3.2, 5.4, 4.3])
        self.assertEqual(driver.select_difference(ts), 1)

        ts = pd.Series([3.5, 3.3, 3.1, 3.9, 3.2, 3.4, 4.3])
        self.assertEqual(driver.select_difference(ts), 0)

        ts = pd.Series([1.1, 4.9, 2.1, 8.0, 3.5, 12.9, 7.1, 9.8, 13.4, 21.4, 28.5,
                        39.1, 44.5, 58.9, 123.4, 454.2])
        self.assertEqual(driver.select_difference(ts), 2)

    def test_short_data(self):
        """Test very short time series."""

        ts = pd.Series([1])
        self.assertEqual(driver.select_difference(ts), 0)

        ts = pd.Series([1, 2])
        self.assertEqual(driver.select_difference(ts), 0)

        ts = pd.Series([1.1, 4.9, 2.1])
        self.assertEqual(driver.select_difference(ts), 0)

        ts = pd.Series([4.7, 4.9, 5.2])
        self.assertEqual(driver.select_difference(ts), 1)


class TestAddSeasonalityToForecast(unittest.TestCase):
    """Tests add_seasonality_to_forecast function."""

    def test_ordinary_input(self):
        season = pd.Series([2.3, 1.8, 3.4, 3.6])
        forecast = pd.Series([0.9, 2.3, 1.2, 3.4, 5.3, 2.1])
        expected_result = pd.Series([0.9 + 2.3, 2.3 + 1.8, 1.2 + 3.4, 3.4 + 3.6, 5.3 + 2.3, 2.1 + 1.8])

        driver.add_seasonality_to_forecast(forecast, season, 4)
        pd.testing.assert_series_equal(forecast, expected_result)


class TestSmoothing(unittest.TestCase):
    """Test smoothing function."""

    def test_ordinary_input(self):
        ts = pd.Series([4.5, 3.4, 2.1, 7.8, 4.5])
        smoothed = driver.smooth_m3c(ts)

        first_expected_item = (4.5+3.4+2.1*2)/4
        second_expected_item = (3.4+2.1+7.8*2)/4
        third_expected_item = (2.1+7.8+4.5*2)/4

        expected_result = pd.Series([first_expected_item, second_expected_item, third_expected_item])
        pd.testing.assert_series_equal(smoothed, expected_result)


class TestPredictor(unittest.TestCase):
    """Tests for main module"""

    def test_basic_forecast(self):
        ts = np.array([3.4, 0.1, 3.9, 4.8, 1.5, 1.8, 2.0, 4.9, 5.1, 2.1])
        forecast = p.make_forecast_multialphabet(ts, groups=["zlib_rp"], h=2, max_quants_count=4, difference=0)
        np.testing.assert_array_almost_equal(np.array(forecast["zlib_rp"]),
                                             np.array([3.0934987622, 3.0934080567]))

        
    def test_discrete_forecast(self):
        ts = np.array([2, 0, 2, 3, 1, 1, 1, 3, 3, 1])
        forecast = p.make_forecast_discrete(ts.tolist(), groups=["zlib_rp"], h=2, difference=0)
        np.testing.assert_array_almost_equal(np.array(forecast["zlib_rp"]),
                                             np.array([1.0264274976, 1.0151519618]))
            
    def test_m3c_year(self):
        ts = np.array([940.66, 1084.86, 1244.98, 1445.02, 1683.17, 2038.15, 2342.52, 2602.45,
                       2927.87, 3103.96, 3360.27, 3807.63, 4387.88, 4936.99])
        forecast = p.make_forecast_multialphabet(ts, groups=["zlib_rp"], h=6, max_quants_count=4, sparse=2,
                                   difference=1)
        np.testing.assert_array_almost_equal(np.array(forecast["zlib_rp"]),
                                             np.array([5427.0124308808, 5917.0363290153,
                                                       6407.0594768841, 6262.0988838384,
                                                       6165.6275143097, 6999.809906989]))

        forecast = p.make_forecast_multialphabet(ts, groups=["zstd_ppmd"], h=6, max_quants_count=4, sparse=2,
                                                 difference=1)
        np.testing.assert_array_almost_equal(np.array(forecast["zstd_ppmd"]),
                                             np.array([5427.0304921905, 5916.0763075179,
                                                       6406.1167892739, 5976.9412898617,
                                                       5748.1506248563, 6495.2662933248]))
        

if __name__ == '__main__':
    unittest.main()
