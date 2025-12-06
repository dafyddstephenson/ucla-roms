import pytest
import glob
import subprocess
from pathlib import Path
from mpc import MPCConfig, run_mpc

# Collect all .F test inputs
test_inputs = sorted(glob.glob("../src//*.F"))

@pytest.mark.parametrize("fname", test_inputs)
def test_src(fname):
    # Generate expected output with actual mpc
    # expected_path = tmp_path/Path(fname).name.replace(".F","_expected.fpp")

    # spr=subprocess.run(f"mpc {fname} > {expected_path}", text=True, shell=True,check=True)
    old=subprocess.run(f"mpc {fname}", text=True, shell=True,check=True, capture_output=True)

    expected=old.stdout.splitlines(keepends=True)


    # Read input .F file
    file_in = Path(fname).read_text().splitlines()

    # Build MPC config
    cfg = MPCConfig(
        trap_unmatched_quotes=True,
        f77_to_f95=True,
        double_constants=True,
        zig_zag=True,
        trap_barriers=False,
        fold_lines=True,
    )

    # Run python MPC
    file_out = run_mpc(file_in, cfg)
    actual = [line + "\n" for line in file_out]

    # Determine expected output file name
    # expected = Path(expected_path).read_text().splitlines(keepends=True)

    # Assertion with nice error message
    assert actual == expected, (
        f"\nFAILED on {fname}\n"
        f"\nEXPECTED:\n{''.join(expected)}\n"
        f"\nGOT:\n{''.join(actual)}"
    )
