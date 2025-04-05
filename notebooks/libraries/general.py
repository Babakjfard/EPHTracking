## This library contains general functions that can be used for all data validations

def get_Counties_FIPS(state='NE'):
    """Get a list of FIPS codes for counties in a state.
    
    Args:
        state (str): The state code.
        
    Returns:
        dictionary: A dictionary of County names and their FIPS codes for counties in the state.
    """
    # download a json file from of FIPS code of counties in Nebraska
    # https://www.census.gov/geographies/reference-files/time-series/geo/carto-boundary-file.html
    # https://www2.census.gov/geo/docs/reference/codes/files/national_county.txt
    # Download the file into a dataframe
    import pandas as pd

    file = r'https://www2.census.gov/geo/docs/reference/codes/files/national_county.txt'
    counties = pd.read_csv(file, sep=',', header=None, dtype=str)
    counties.columns = ['state_code' ,'state_fips', 'county_fips', 'county_name', 'code']

    # select only the counties in the state
    counties = counties[counties['state_code'] == state]

    # attach state_fips to county_fips to create a unique FIPS code
    counties['fips'] = counties['state_fips'] + counties['county_fips']
    counties.reset_index(inplace=True, drop=True)
    
    return counties[['fips', 'county_name']]



import requests
from io import StringIO

def get_Counties_FIPS_with_requests(state='NE'):
    """
    This one was added later, when the direct download was prohibited from the webiste.
    Now, this uses requests (instead of direct pandas opening) with a header that pretends
    this is a web browser request
    """
    url = 'https://www2.census.gov/geo/docs/reference/codes/files/national_county.txt'
    
    headers = {
        "User-Agent": "Mozilla/5.0"  # Pretend to be a browser
    }
    
    response = requests.get(url, headers=headers)
    if response.status_code != 200:
        raise Exception(f"Failed to download file: {response.status_code}")
    
    data = StringIO(response.text)
    counties = pd.read_csv(data, sep=",", header=None, dtype=str)
    counties.columns = ['state_code' ,'state_fips', 'county_fips', 'county_name', 'code']

    counties = counties[counties['state_code'] == state]
    counties['fips'] = counties['state_fips'] + counties['county_fips']
    counties.reset_index(inplace=True, drop=True)
    
    return counties[['fips', 'county_name']]



# TODO: Add a docstring and comments
import pandas as pd
import numpy as np

def separate_date_column(df):
    # Create a copy of the input DataFrame to avoid modifying the original
    df = df.copy()
    
    # Extract year and month from the DateSampled column
    df['theYear'] = pd.DatetimeIndex(df['DateSampled']).year
    df['month'] = pd.DatetimeIndex(df['DateSampled']).month
    
    # Use numpy.select to assign the correct quarter based on the month
    conditions = [
        df['month'].isin([1, 2, 3]),
        df['month'].isin([4, 5, 6]),
        df['month'].isin([7, 8, 9]),
        df['month'].isin([10, 11, 12])
    ]
    choices = ['Q1', 'Q2', 'Q3', 'Q4']
    df['Quarter'] = np.select(conditions, choices)
    
    # Combine theYear and Quarter columns into a new column named TimePeriod
    df['TimePeriod'] = df['theYear'].astype(str) + '-' + df['Quarter']
    
    # Drop the theYear, month, and Quarter, columns
    df = df.drop(['theYear', 'month', 'Quarter'], axis=1)
    
    return df


# TODO: Add a docstring and comments
# TODO: Add a test for this function
# TODO: Completet to the all analytes
# TODO: Move into a water related library
def replace_zero_concentration(df):
    df.loc[df['Concentration'] == 0, 'Concentration'] = df.apply(lambda row: 
        0.01 if row['AnalyteCode'] == 1005 else
        0.002 if row['AnalyteCode'] == 2050 else
        0.25 if row['AnalyteCode'] == 2456 else
        row['Concentration'],
        axis=1
    )
    return df