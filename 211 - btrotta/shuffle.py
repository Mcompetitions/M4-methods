import pandas as pd

for freq in ['Daily', 'Hourly', 'Monthly', 'Quarterly', 'Weekly', 'Yearly']:
    df = pd.read_csv('data\\{}-train.csv'.format(freq), header=0, index_col=0)
    df = df.sample(frac=1, random_state=0)
    df.to_csv('data\\{}_train_shuffle.csv'.format(freq), index=True)