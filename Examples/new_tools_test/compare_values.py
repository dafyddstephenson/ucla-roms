#!/usr/bin/env python3
import xarray as xr
import numpy as np
import sys
file1=sys.argv[1]
file2=sys.argv[2]
DS0=xr.open_dataset(file1)
DS1=xr.open_dataset(file2)

for v in DS0.variables:
    print(v)
    vdiff = np.sum(np.abs(DS0[v].values - DS1[v].values))
    if vdiff>0:
        print(f"DIFFERENCE of {vdiff} in {v}")
        # sys.exit(1)
