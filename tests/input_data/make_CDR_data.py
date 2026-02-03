import netCDF4
s_rho = 50
xi_rho = 201
eta_rho = 101
cdr_time = [3653.0,3655.0]

nt = 34
ncdr = 1
t0=3653.0
t1=3655.0


times = len(cdr_time)

# 3D forcing
f = netCDF4.Dataset('cdr_forcing_3d.nc', 'w', format='NETCDF4')

tmp = f.createDimension('xi_rho', xi_rho)
tmp = f.createDimension('eta_rho', eta_rho)
tmp = f.createDimension('s_rho', s_rho)
tmp = f.createDimension('cdr_time', times)

tmp_zi = f.createVariable('cdr_trcflx_3d_ALK', 'f8', ('cdr_time','s_rho','eta_rho','xi_rho'))
setattr(tmp_zi, 'long_name', "tracer flux [mmol/s]")
setattr(tmp_zi, 'units', "mmol/s")
f['cdr_trcflx_3d_ALK'][:,:,:,:] = 0
f['cdr_trcflx_3d_ALK'][:,:,40:60,90:110] = 1e-3

tmp_zi = f.createVariable('cdr_trcflx_3d_DIC', 'f8', ('cdr_time','s_rho','eta_rho','xi_rho'))
setattr(tmp_zi, 'long_name', "tracer flux [mmol/s]")
setattr(tmp_zi, 'units', "mmol/s")
f['cdr_trcflx_3d_DIC'][:,:,:,:] = 0

tmp_zi = f.createVariable('cdr_time', 'f8', ('cdr_time'))
setattr(tmp_zi, 'long_name', "Time for CDR release")
setattr(tmp_zi, 'units', "days")
f['cdr_time'][:] = cdr_time[:]

f.close()

# Depth profiles

f = netCDF4.Dataset('cdr_forcing_dp.nc', 'w', format='NETCDF4')

tmp = f.createDimension('s_rho', s_rho)
tmp = f.createDimension('cdr_time', times)
tmp = f.createDimension('ncdr_prof', ncdr)
tmp = f.createDimension('nt', nt)
tmp = f.createDimension('one', 1)
tmp = f.createDimension('two', 2)

tmp_zi = f.createVariable('depth_profiles', 'S1', ('one'))
setattr(tmp_zi, 'long_name', "depth profiles (T) or Gaussian (F)")
setattr(tmp_zi, 'units', "nondim")
f['depth_profiles'][:] = 'T'

tmp_zi = f.createVariable('cdr_trcflx_profile', 'f8', ('cdr_time','s_rho','two','ncdr_prof'))
setattr(tmp_zi, 'long_name', "tracer flux [mmol/s]")
setattr(tmp_zi, 'units', "mmol/s")
f['cdr_trcflx_profile'][:,:,:,:] = 0
f['cdr_trcflx_profile'][:,:,0,:] = 1e-3

tmp_zi = f.createVariable('cdr_time', 'f8', ('cdr_time'))
setattr(tmp_zi, 'long_name', "Time for CDR release")
setattr(tmp_zi, 'units', "days")
f['cdr_time'][:] = cdr_time[:]

tmp_zi = f.createVariable('cdr_lon', 'f8', ('ncdr_prof'))
setattr(tmp_zi, 'long_name', "longitude of CDR release [degrees East]")
setattr(tmp_zi, 'units', "deg E")
f['cdr_lon'][:] = -120.64

tmp_zi = f.createVariable('cdr_lat', 'f8', ('ncdr_prof'))
setattr(tmp_zi, 'long_name', "latitude of CDR release [degrees North]")
setattr(tmp_zi, 'units', "deg N")
f['cdr_lat'][:] = 34.5349

tmp_zi = f.createVariable('cdr_layer_thickness', 'f8', ('cdr_time','s_rho','ncdr_prof'))
setattr(tmp_zi, 'long_name', "layer thicknesses of CDR release given in a vertical profile [m]")
setattr(tmp_zi, 'units', "m")
f['cdr_layer_thickness'][:,:,:] = 1

f.close()

# Parameterized
f = netCDF4.Dataset('cdr_forcing_parm.nc', 'w', format='NETCDF4')

tmp = f.createDimension('cdr_time', times)
tmp = f.createDimension('ncdr_parm', ncdr)
tmp = f.createDimension('nt', nt)

tmp_zi = f.createVariable('cdr_trcflx', 'f8', ('cdr_time','nt','ncdr_parm'))
setattr(tmp_zi, 'long_name', "tracer flux [mmol/s]")
setattr(tmp_zi, 'units', "mmol/s")
f['cdr_trcflx'][:,:,:] = 0
f['cdr_trcflx'][:,11,:] = 1e-3

tmp_zi = f.createVariable('cdr_time', 'f8', ('cdr_time'))
setattr(tmp_zi, 'long_name', "Time for CDR release")
setattr(tmp_zi, 'units', "days")
f['cdr_time'][:] = cdr_time[:]

tmp_zi = f.createVariable('cdr_lon', 'f8', ('ncdr_parm'))
setattr(tmp_zi, 'long_name', "longitude of CDR release [degrees East]")
setattr(tmp_zi, 'units', "deg E")
f['cdr_lon'][:] = -120.64

tmp_zi = f.createVariable('cdr_lat', 'f8', ('ncdr_parm'))
setattr(tmp_zi, 'long_name', "latitude of CDR release [degrees North]")
setattr(tmp_zi, 'units', "deg E")
f['cdr_lat'][:] = 34.5349

tmp_zi = f.createVariable('cdr_dep', 'f8', ('ncdr_parm'))
setattr(tmp_zi, 'long_name', "depth of CDR release [m]")
setattr(tmp_zi, 'units', "m")
f['cdr_dep'][:] = 10.0

tmp_zi = f.createVariable('cdr_hsc', 'f8', ('ncdr_parm'))
setattr(tmp_zi, 'long_name', "horizontal scale of CDR release")
setattr(tmp_zi, 'units', "m")
f['cdr_hsc'][:] = 360.0

tmp_zi = f.createVariable('cdr_vsc', 'f8', ('ncdr_parm'))
setattr(tmp_zi, 'long_name', "vertical scale of CDR release")
setattr(tmp_zi, 'units', "m")
f['cdr_vsc'][:] = 5.0

f.close()
