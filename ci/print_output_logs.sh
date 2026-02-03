#!/bin/bash

#

# Get the "tests" array from tests/scripts/do_test_all.sh
{
  IFS= read -r shebang_line
  while IFS= read -r line || [[ -n "$line" ]]; do
    [[ "$line" == *"USER INPUT END"* ]] && break
    eval "$line"
  done
} < $ROMS_ROOT/tests/scripts/do_test_all.sh

if [ ${#Examples[@]} -eq 0 ]; then
    echo "The tests array is empty."
    echo "This script expects a bash array `tests` to be defined in"
    echo "tests/code_check/do_test_all.sh"
    echo "above the line containing `USER INPUT END`."
    echo "Please verify the contents of this file."
    exit 1
fi

# Loop over the "tests" array
for example in "${tests[@]}"; do
    echo "###############################################################################################"
    echo "$example"
    echo "###############################################################################################"
    if [ -e "${ROMS_ROOT}/tests/${example}/test_old.log" ];then
	cat ${ROMS_ROOT}/tests/${example}/test_old.log
    fi
    if $example == "bgc_real";then
       echo "------------------------------"
       echo "MARBL"
       echo "------------------------------"
       if [ -e "${ROMS_ROOT}/tests/${example}/test_old_MARBL.log" ] ;then
	   cat ${ROMS_ROOT}/tests/${example}/test_old_MARBL.log
       fi
       echo "------------------------------"
       echo "BEC"
       echo "------------------------------"
       if [ -e "${ROMS_ROOT}/tests/${example}/test_old_BEC.log" ];then
	   cat ${ROMS_ROOT}/tests/${example}/test_old_BEC.log
       fi
    fi

done
