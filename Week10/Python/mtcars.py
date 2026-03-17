import statsmodels.api as sm
import pandas as pd

# Load the mtcars dataset from R datasets
mtcars_r_dataset = sm.datasets.get_rdataset('mtcars')

# Access the data as a pandas DataFrame
mtcars_data = mtcars_r_dataset.data
