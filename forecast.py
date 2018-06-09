#!/usr/bin/env python

import csv
import logging
from m4.forecast import m4forecast


HORIZONS = [
    ('H', 48),
    ('D', 14),
    ('W', 13),
    ('M', 18),
    ('Q', 8),
    ('Y', 6)
]


def id_to_horizon(id):
    for prefix, horizon in HORIZONS:
        if id.startswith(prefix):
            return horizon
    raise ValueError('Invalid M4 series id: {}'.format(id))


def main(path):
    with open(path, newline='') as f:
        reader = csv.reader(f)
        for row in reader:
            id = row[0]
            try:
                horizon = id_to_horizon(id)
            except ValueError as e:
                logging.error(e.args[0])
                continue
            sample = [float(v) for v in row[1:] if v]
            forecast = m4forecast(sample, horizon)
            print('{},{}'.format(id, ','.join([str(v) for v in forecast])))


if __name__ == '__main__':
    import sys

    if len(sys.argv) != 2:
        msg = 'Usage: {} <path/to/series.csv>\n'
        sys.stderr.write(msg.format(sys.argv[0]))
    else:
        main(sys.argv[1])
