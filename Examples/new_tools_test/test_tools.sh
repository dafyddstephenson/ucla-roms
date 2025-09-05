#!/bin/bash

testdir=$(pwd)

# Compile
make compile_clean
make FFLAGS="-O0 -fallow-argument-mismatch"
gridfile="sample_grd_riv.nc"

if [ -L input_datasets ];then
    rm input_datasets
fi


# Run with old tools
if [ ! -d input_datasets_old_tools ];then
    mkdir $testdir/input_datasets_old_tools
    cd $testdir/input_datasets_old_tools

    for f in {"${gridfile}","sample_flux_frc.nc","sample_wwv_riv.nc","roms_bry_trace.nc","roms_init_trace.nc"};do
        ln -s $ROMS_ROOT/Examples/input_data/${f} .
        $ROMS_ROOT/Tools-Roms/partit 5 8 ${f}
    done
    cd $testdir
fi


ln -s input_datasets_old_tools/ input_datasets

mpirun -n 40 ./roms tools.in 2>&1 | tee old_tools.log
if [ -L input_datasets ];then
    rm input_datasets
fi
for F in tools*.nc;do
    mv ${F} old_${F}
done


for F in old_tools*.00.nc;do
    $ROMS_ROOT/New-tools/ncjoin ${F/.00.nc}.??.nc;
    # if [ -e ${F/.00.nc}.nc ];then
    #     rm ${F/.00.nc}.??.nc
    # fi
done
# Run with new tools
if [ ! -d input_datasets_new_tools ];then
    mkdir $testdir/input_datasets_new_tools
    cd $testdir/input_datasets_new_tools

    for f in {"${gridfile}","sample_flux_frc.nc","sample_wwv_riv.nc","roms_bry_trace.nc","roms_init_trace.nc"};do
        ln -s $ROMS_ROOT/Examples/input_data/${f} .
        $ROMS_ROOT/New-tools/partit 5 8 $gridfile ${f}
    done
fi
cd $testdir

ln -s input_datasets_new_tools/ input_datasets

mpirun -n 39 ./roms tools.in 2>&1 | tee new_tools.log

if [ -L input_datasets ];then
    rm input_datasets
fi
for F in tools*.nc;do
    mv ${F} new_${F}
done


for F in new_tools*.00.nc;do
    $ROMS_ROOT/New-tools/ncjoin ${F/.00.nc}.??.nc;
    # if [ -e ${F/.00.nc}.nc ];then
    #     rm ${F/.00.nc}.??.nc
    # fi
done

python compare_values.py old_tools_his.20121209133435.nc new_tools_his.20121209133435.nc
