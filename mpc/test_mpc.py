import pytest
import glob
from pathlib import Path
from mpc import MPCConfig, run_mpc

# Collect all .F test inputs
test_inputs = sorted(glob.glob("test_files/*.F"))

@pytest.mark.parametrize("fname", test_inputs)
def test_mpc(fname):
    # Read input .F file
    file_in = Path(fname).read_text().splitlines()

    # Build MPC config
    cfg = MPCConfig(
        trap_unmatched_quotes=True,
        f77_to_f95=True,
        double_constants=True,
        zig_zag=True,
        trap_barriers=True,
        fold_lines=True,
    )

    # Run python MPC
    file_out = run_mpc(file_in, cfg)
    actual = [line + "\n" for line in file_out]

    # Determine expected output file name
    expected_path = fname.replace(".F", "_expected.fpp")
    expected = Path(expected_path).read_text().splitlines(keepends=True)

    # Assertion with nice error message
    assert actual == expected, (
        f"\nFAILED on {fname}\n"
        f"\nEXPECTED:\n{''.join(expected)}\n"
        f"\nGOT:\n{''.join(actual)}"
    )
