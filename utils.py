import re
import pandas as pd
import numpy as np



def str_normalize(s):
    s = str(s).strip().lower()
    s = re.sub(r"	", " ", s)
    s = re.sub(r" ", " ", s) # b'\xc2\xa0'
    return s


def age_group(df):
    col1 = df["age_source1"]
    col2 = df["age_source2"]

    def _combine_func(x, y):
        if x == "nan" and y == "nan":
            return "nan"
        if x == "nan":
            return y
        return x

    age = col1.combine(col2, _combine_func)
    age.fillna("nan", inplace=True)

    bins = np.array([-1, 0, 15, 18, 22, 25, 28, 31, 35, 38, 41, 44, 47, 50, 53, 55, 57, 60, 65, 70, 80])
    inds = np.digitize(age.replace("nan", -1).to_numpy(dtype=np.int), bins)
    df["age_group"] = inds
    df["age_group"].replace(1, 0, inplace=True)
    
    return df
    

def field7_count(df):
    col = df.FIELD_7
    col.fillna("[]", inplace=True)
    col = col.apply(lambda x: len(eval(str(x))))
    df["FIELD_7_COUNT"] = col
    return df

