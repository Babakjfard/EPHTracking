import pandas as pd
from datetime import date
from typing import Optional, List, Literal
from pydantic import BaseModel, ValidationError, Field, conint, confloat, constr, validator
import datetime

class PWS_Inventory(BaseModel):
    """Class for representing Public Water System (PWS) Inventory information.
    
    Attributes:
        RowIdentifier (int): A unique integer identifying the row in the inventory.
                             This is for validation purposes only. To be deleted from 
                             populated data before submission.
        PWSIDNumber (str): nine-character value consisting of the 2-letter state abbreviation followed by 7 numbers 
        YearAssociatedTo (int): YYYY. 1999 through latest complete year (e.g., 2021) .
        YearPulled (int): YYYY. 1999 through latest year.  .
        PWSName (str): The name of the PWS.
        PrincipalCountyServedFIPS (str): The FIPS code for the principal county served by the PWS.
        PrincipalCityFeatureID (int): The feature ID for the principal city served by the PWS.
        TotalConnections (int): The total number of connections for the PWS.
        SystemPopulation (int): The population served by the PWS.
        PrimarySourceCode (str): The code representing the primary source of water for the PWS.
        Latitude (float): The latitude of the PWS location.
        Longitude (float): The longitude of the PWS location.
        LocationDerivationCode (str): The code indicating how the PWS location was derived.
    """
    RowIdentifier: int
    PWSIDNumber: constr(regex=r'^NE\d{7}') #Change NE to represent your state code
    
    YearAssociatedTo: conint(ge=1999, le=2023) 
    YearPulled: conint(ge=1999, le=2023)
    
    PWSName: str #Should it have distinction between Unknows and Not Submitted? or just be blank?
    
    PrincipalCountyServedFIPS: str

    PrincipalCityFeatureID: int # ????How to get it from the introduced source?

    TotalConnections: conint(ge=1, le=9999999)
    SystemPopulation: conint(ge=10, le=99999999)
    PrimarySourceCode: Literal['GU', 'GUP', 'GW', 'GWP', 'SW', 'SWP', 'U', 'NS']

    # For Nebraska in NAD83
    Latitude: confloat(ge= 39.999998, le=43.001702) 
    Longitude: confloat(ge= -104.053514, le=-95.308290)
    LocationDerivationCode: Literal['SA', 'MFL', 'PCS', 'GSH','O', '-999', '-888']

    @validator('PrincipalCountyServedFIPS')
    def check_PrincipalCountyServedFIPS(cls, v):
        """Validator function for PrincipalCountyServedFIPS attribute.
        
        Checks whether the input value for PrincipalCountyServedFIPS is valid.
        """
        allowed_values = counties['fips'].tolist()
        if v not in allowed_values:
            raise ValueError('PrincipalCountyServedFIPS must be a valid FIPS code')
        return v   

    def __init__(self, counties: pd.DataFrame, **data):
        self.counties = counties
        super().__init__(**data)
         

class Quality_Sampling(BaseModel):
    """
    A class to represent Quality Sampling data in a Public Water System (PWS).

    ...

    Attributes
    ----------
    RowIdentifier : int
        Unique identifier for each sampling event in the system.
    PWSIDNumber : constr
        Public Water System Identification Number, formatted as state code followed by 7 digits.
    Year : conint
        Year of the sampling event, must be between 1999 and 2023 (inclusive).
    AnalyteCode : Literal
        Code representing the specific chemical or substance being measured. Must be one of the following:
        '1005', '2050', '2456', '2950', '2039', '1040', '2987', '2984', '4010', '226', '228', '4006'.
    ConcentrationUnits : Literal
        Unit of measure for the concentration of the Analyte. Must be one of the following:
        'ug/l', 'mg/l', 'pci/l'.
    Concentration : float
        The concentration of the Analyte in the sample.
    DateSampled : datetime.date
        Date when the sample was taken. Must be between January 1, 1999 and the latest complete year.
    AggregationType : Literal
        Type of aggregation applied to the sample data. Must be one of the following:
        'X' (no aggregation), 'MX' (monthly average).
    NumSamplingLocations : conint
        Number of locations where the sample was taken. Must be between 1 and 9999 (inclusive), or -888 if not submitted.
    SummaryTimePeriod : str
        Period of time over which the samples were taken and summarized. TODO: look into its Data Dictionary
    NumSamples : int
        Total number of samples taken.
    NumNonDetects : int
        Number of non-detectable samples taken.
    """
    RowIdentifier: int
    PWSIDNumber: constr(regex=r'^NE\d{7}')

    Year: conint(ge=1999, le=2023)
    
    AnalyteCode: Literal['1005', '2050', '2456', '2950', '2039','1040', '2987', 
    '2984', '4010', '4006']
    ConcentrationUnits: Literal['ug/l', 'mg/l','pci/l']
    Concentration: float

    DateSampled: datetime.date
    
    AggregationType: Literal['X', 'MX']
    NumSamplingLocations: conint(ge=1, le=9999)
    SummaryTimePeriod: str
    NumSamples: int
    NumNonDetects: int
    """
    TODO:
    Apply the rules of what ConcentrationUnits applies to each AnalyteCode.
    Look into the Data Dictionary for the SummaryTimePeriod attribute.
    """

class Quality_samling_Lead(Quality_Sampling):
    """
    A class to represent Quality Sampling data in a Public Water System (PWS) for Lead.

    ...

    Attributes
    ----------
    RowIdentifier : int
        Unique identifier for each sampling event in the system.
    PWSIDNumber : constr
        Public Water System Identification Number, formatted as state code followed by 7 digits.
    Year : conint
        Year of the sampling event, must be between 1999 and 2023 (inclusive).
    AnalyteCode : Literal
        Code representing the specific chemical or substance being measured. Must be one of the following:
        '1005', '2050', '2456', '2950', '2039', '1040', '2987', '2984', '4010', '226', '228', '4006'.
    ConcentrationUnits : Literal
        Unit of measure for the concentration of the Analyte. Must be one of the following:
        'ug/l', 'mg/l', 'pci/l'.
    Concentration : float
        The concentration of the Analyte in the sample.
    DateSampled : datetime.date
        Date when the sample was taken. Must be between January 1, 1999 and the latest complete year.
    AggregationType : Literal
        Type of aggregation applied to the sample data. Must be one of the following:
        'X' (no aggregation), 'MX' (monthly average).
    NumSamplingLocations : conint
        Number of locations where the sample was taken. Must be between 1 and 9999 (inclusive), or -888 if not submitted.
    SummaryTimePeriod : str
        Period of time over which the samples were taken and summarized. TODO: look into its Data Dictionary
    NumSamples : int
        Total number of samples taken.
    NumNonDetects : int
        Number of non-detectable samples taken.
    """
    AnalyteCode: Literal['PB90', '1030']
    
    # ConcentrationUnits can only have 'ug/l' 
    ConcentrationUnits: Literal['ug/l']
    Concentration: float

    DateSampled: datetime.date
    
    AggregationType: Literal['90X']
    NumSamplingLocations: conint(ge=1, le=9999)
    
    # SummaryTimePeriod can be a year in format YYYY
    SummaryTimePeriod: conint(ge=1999, le=2023) 
    NumSamples: Optional[int]

