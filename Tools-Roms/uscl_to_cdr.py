import argparse
import numpy as np
import xarray as xr
from pathlib import Path

class CDRUpscaler:
    def __init__(self,files):
        self.files = files
        print(f"Opening {len(self.files)} `_uscl` files...")
        self.uscl_dataset = xr.open_mfdataset(
            self.files,
            concat_dim="time",
            combine="nested",
            data_vars="minimal",
            coords="minimal"
        )
        self.eta_rho = self.uscl_dataset.eta_rho
        self.xi_rho = self.uscl_dataset.xi_rho
        self.s_rho = self.uscl_dataset.s_rho
        self.time = self.uscl_dataset.time

        # Determine active boundaries and add up all columns to get no. profiles
        self.child_boundaries = {}
        self.n_profiles = 0
        for bry in ["north","south","east","west"]:
            v = f"ALK_add_{bry}"
            if v in self.uscl_dataset.variables:
                self.child_boundaries[bry] = True
                rho_var = "eta_rho" if bry.endswith("st") else "xi_rho"
                self.n_profiles = self.n_profiles + self.uscl_dataset.sizes.get(rho_var)
        print(f"Found {self.n_profiles} boundary tracer profiles to convert to CDR forcing profiles")

        # Initialize CDR dataset
        self.cdr_dataset = xr.Dataset()

    @property
    def filename_prefix(self) -> str:
        prefixes = [f.split("_uscl")[0] for f in self.files]
        if not all([prefix == prefixes[0] for prefix in prefixes]):
            raise ValueError("Filenames do not share a common prefix")
        return prefixes[0]

    def create_cdr_dataset(self):
        ds = self.cdr_dataset

        ds["depth_profiles"] = xr.DataArray(
            np.array(["T"], dtype="S1"),
            dims=("one",),
            attrs={
                "long_name": "depth profiles (T) or Gaussian (F)",
                "units": "nondim",
            },
        )

        ds["cdr_trcflx_profile"] = xr.DataArray(
            np.zeros((len(self.time), len(self.s_rho), 2, self.n_profiles)),
            dims=("cdr_time", "s_rho", "two", "ncdr_prof"),
            attrs={
                "long_name": "tracer flux [mmol/s]",
                "units": "mmol/s",
            },
        )

        ds["cdr_time"] = xr.DataArray(
            self.uscl_dataset.ocean_time.values/86400.,
            dims=("cdr_time",),
            attrs={
                "long_name": "Time for CDR release",
                "units": "days",
            },
        )

        ds["cdr_lon"] = xr.DataArray(
            np.zeros(self.n_profiles),
            dims=("ncdr_prof",),
            attrs={
                "long_name": "longitude of CDR release [degrees East]",
                "units": "deg E",
            },
        )

        ds["cdr_lat"] = xr.DataArray(
            np.zeros(self.n_profiles),
            dims=("ncdr_prof",),
            attrs={
                "long_name": "latitude of CDR release [degrees North]",
                "units": "deg N",
            },
        )

        ds["cdr_layer_thickness"] = xr.DataArray(
            np.zeros((len(self.time), len(self.s_rho), self.n_profiles)),
            dims=("cdr_time", "s_rho", "ncdr_prof"),
            attrs={
                "long_name": "layer thicknesses of CDR release given in a vertical profile [m]",
                "units": "m",
            },
        )
        self.cdr_dataset = ds

    def populate_cdr_dataset(self):

        def boundaries_to_profiles(var_prefix):
            """Convert boundary columns to a sequence of profiles.

            This formulation uses `.values` which triggers compute.
            See boundaries_to_profiles_xr for lazy implementation.
            """

            return np.concatenate(
                [self.uscl_dataset[f"{var_prefix}_{bry}"].values
                 for bry in ["north","east","south","west"]
                 if self.child_boundaries.get(bry)],
                axis=-1
            )

        def boundaries_to_profiles_xr(var_prefix):
            """boundaries_to_profiles using xarray/lazy concatenation.

            Should be faster, but isn't. Huge bottleneck when saving."""

            data_arrays=[]
            for bry in ["north","east","south","west"]:
                if self.child_boundaries.get(bry):
                    rho_var = "eta_rho" if bry.endswith("st") else "xi_rho"
                    da = self.uscl_dataset[f"{var_prefix}_{bry}"].rename({rho_var: "ncdr_prof"})
                    if "time" in da.dims:
                        da = da.rename({"time" : "cdr_time"})
                    data_arrays.append(da)
            return xr.concat(data_arrays, dim="ncdr_prof")

        print("Populating `cdr_lat`...")
        self.cdr_dataset["cdr_lat"][:] = boundaries_to_profiles("lat")
        print("Populating `cdr_lon`...")
        self.cdr_dataset["cdr_lon"][:] = boundaries_to_profiles("lon")
        print("Populating `cdr_trcflx_profile` for tracer ALK...")
        self.cdr_dataset["cdr_trcflx_profile"][:,:,0,:] = boundaries_to_profiles("ALK_add")
        print("Populating `cdr_trcflx_profile` for tracer DIC...")
        self.cdr_dataset["cdr_trcflx_profile"][:,:,1,:] = boundaries_to_profiles("DIC_add")
        print("Populating `cdr_layer_thickness`...")
        self.cdr_dataset["cdr_layer_thickness"][:] = boundaries_to_profiles("h")


    def save(self,filename: str | None = None):
        if not filename:
            filename = f"cdr_release_profiles_from_{self.filename_prefix}.nc"
        print(f"Saving output to {filename}")
        self.cdr_dataset.to_netcdf(filename)
        print("Done.")


def init_argparse() -> "ArgumentParser":
    description = """
    Utility for converting ROMS `<prefix>_uscl.<time>.nc` files to CDR forcing files.

    This program takes a series of `uscl` files from a nested/child simulation and
    creates a series of CDR forcing depth profiles that can be supplied to the parent
    simulation to propagate CDR-related tracers from child to parent conservatively.

    Each column at the edge of the child boundary is converted to a time-varying
    depth profile with lat/lon information such that ROMS can vertically remap it
    at the correct point on the parent grid.
    """

    parser = argparse.ArgumentParser(
        prog="uscl_to_cdr",
        description=description,
        formatter_class=argparse.RawDescriptionHelpFormatter
    )
    parser.add_argument("files", nargs="+", help="Input files")

    return parser

if __name__ == "__main__":
    parser = init_argparse()
    args = parser.parse_args()

    cdr_upscaler = CDRUpscaler(sorted(args.files))
    cdr_upscaler.create_cdr_dataset()
    cdr_upscaler.populate_cdr_dataset()
    cdr_upscaler.save()



