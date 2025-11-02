import re
import sys
from pathlib import Path
from typing import Callable


_parser_registry: list[Callable] = []


def register_logparser(func: Callable) -> Callable:
    """Decorator to register a parser function"""
    _parser_registry.append(func)
    return func


class ROMSSimulationLog:
    def __init__(self,
                 log_file: Path,
                 jobid: int | None = None):
        self.log_file = log_file
        with open(self.log_file, "r") as f:
            self.lines = f.readlines()

        # storage
        self.cppdefs: list[str] = []
        self.ntimes: int | None = None
        self.dt: float | None = None
        self.ndtfast: int | None = None
        self.ninfo: int | None = None
        self.theta_s: float | None = None
        self.theta_b: float | None = None
        self.hc: float | None = None

        self.grid_file: Path | None = None
        self.forcing_files: list[Path] = []
        self.initial_condition_rec: int | None = None
        self.initial_condition_file: Path | None = None

        self._parse_logfile()
        self.jobid=jobid
        if self.jobid:
            self._query_jobid()

    def _parse_logfile(self):
        current_line = 0
        while current_line < len(self.lines):
            for parser in _parser_registry:
                next_line = parser(self, current_line)
                if next_line != current_line:
                    current_line = next_line
                    break
            else:
                current_line += 1

    def _query_jobid(self):
        pass

    @property
    def ntracers(self):
        return len(self.tracers)

@register_logparser
def parse_git_hash(log, i):
    line = log.lines[i].strip().lower()
    if "git hash" in line:
        log.git_hash = line.split(":")[-1].strip()
        return i+1
    return i

@register_logparser
def parse_job_id(log, i):
    line = log.lines[i].strip()

    # job id: SLURM Job ID: <id>   OR   PBS Job ID: <id>
    if line.startswith(("SLURM Job ID:", "PBS Job ID:")):
        log.job_id = line.split(":", 1)[1].strip()
        return i+1

    # array id: SLURM Array Task ID: <id>   OR   PBS Array Index: <id>
    if line.startswith(("SLURM Array Task ID:", "PBS Array Index:")):
        log.job_array_task_id = line.split(":", 1)[1].strip()
        return i+1

    return i
    
@register_logparser
def parse_cppdefs(log: ROMSSimulationLog, current_line: int) -> int:
    line = log.lines[current_line]
    if "<cppdefs.opt>" not in line:
        return current_line
    j = current_line + 1
    while j < len(log.lines):
        l = log.lines[j].strip()
        if not l or (l.startswith("<") and l.endswith(">")):
            break
        log.cppdefs.extend(l.split())
        j += 1
    return j


KV_PARAMS = {"ntimes", "dt", "ndtfast", "ninfo", "theta_s", "theta_b", "hc", "rho0","visc2"}


@register_logparser
def parse_key_value_params(log: "ROMSSimulationLog", current_line: int) -> int:
    line = log.lines[current_line]
    if "=" not in line:
        return current_line

    parts = line.split("=", 1)
    key = parts[0].strip()
    if key not in KV_PARAMS:
        return current_line

    rhs = parts[1].strip()
    token = rhs.split()[0]

    try:
        val = float(token) if "." in token or "E" in token.upper() else int(token)
    except ValueError:
        val = token

    setattr(log, key, val)
    return current_line + 1


@register_logparser
def parse_grid_file(log: "ROMSSimulationLog", current_line: int) -> int:
    line = log.lines[current_line]
    if not line.strip().startswith("grid file:"):
        return current_line
    log.grid_file = Path(line.split(":", 1)[1].strip())
    return current_line + 1


@register_logparser
def parse_forcing_files(log: "ROMSSimulationLog", current_line: int) -> int:
    line = log.lines[current_line]
    if not line.strip().startswith("forcing data file(s):"):
        return current_line

    files = [Path(line.split(":", 1)[1].strip())]
    j = current_line + 1
    while j < len(log.lines):
        nxt = log.lines[j]
        stripped = nxt.strip()
        if not stripped or not nxt.startswith(" " * 2):
            break
        files.append(Path(stripped))
        j += 1
    log.forcing_files = files
    return j


@register_logparser
def parse_initial_condition(log: "ROMSSimulationLog", current_line: int) -> int:
    line = log.lines[current_line]
    if not line.strip().startswith("initial condition"):
        return current_line

    tokens = line.split()
    if "rec" in tokens:
        idx = tokens.index("rec")
        log.initial_condition_rec = int(tokens[idx + 2])  # skip '=' then value
    if "file" in tokens:
        idx = tokens.index("file")
        log.initial_condition_file = Path(tokens[idx + 2].strip("'"))
    return current_line + 1

@register_logparser
def parse_node_grid_info(log: "ROMSSimulationLog", current_line: int) -> int:
    line = log.lines[current_line].strip()

    if not line.startswith("NUMBER OF NODES:"):
        return current_line

    # compile locally (readable, one and done)
    nodes_re = re.compile(r"NUMBER OF NODES:\s*(?P<ncpus>\d+)")
    procs_re = re.compile(r"\((?P<np_xi>\d+)\s*x\s*(?P<np_eta>\d+)\)")
    grid_re = re.compile(r"GRID:\s*(?P<nx>\d+)\s*x\s*(?P<ny>\d+)\s*x\s*(?P<nz>\d+)")

    m = nodes_re.search(line)
    if m:
        log.ncpus = int(m.group("ncpus"))

    m = procs_re.search(line)
    if m:
        log.np_xi  = int(m.group("np_xi"))
        log.np_eta = int(m.group("np_eta"))

    m = grid_re.search(line)
    if m:
        log.nx = int(m.group("nx"))
        log.ny = int(m.group("ny"))
        log.nz = int(m.group("nz"))

    return current_line + 1

@register_logparser
def parse_tracers(log: "ROMSSimulationLog", current_line: int) -> int:
    line = log.lines[current_line].strip()
    if not line.startswith("TRACER NO.:"):
        return current_line

    if not hasattr(log, "tracers"):
        log.tracers = []

    j = current_line
    while j < len(log.lines):
        line = log.lines[j].strip()
        if not line.startswith("TRACER NO.:"):
            break

        # SHORT NAME is always the next line
        short_line = log.lines[j + 1].strip()
        if short_line.startswith("SHORT NAME:"):
            short_name = short_line.split(":", 1)[1].strip()
            log.tracers.append(short_name)

        # skip ahead until the end of this block (the dashed line)
        while j < len(log.lines) and not log.lines[j].strip().startswith("-----------"):
            j += 1
        j += 1  # move past the dashes

    return j

################################################################################
# OUTPUT SETTINGS:
@register_logparser
def parse_rst(log, i):
    if "ocean_vars :: restart file" not in log.lines[i]:
        return i

    line = log.lines[i]

    # recs/file
    m = re.search(r"recs/file\s*=\s*(\d+)", line)
    log.nrpf_rst = int(m.group(1)) if m else None

    # period or monthly
    if "monthly_restarts" in line:
        log.output_period_rst = "monthly"
    else:
        m = re.search(r"output_period_rst\s*=\s*([0-9.]+)", line)
        log.output_period_rst = float(m.group(1)) if m else None

    return i + 1


# Generic parsing function for bgc/phys/cstar:
def _parse_output_block(log, start_i: int,
                        period_attr: str,
                        nrpf_attr: str,
                        vars_attr: str | None):
    """
    generic reader
    - period_attr: attribute on log to store output period (float or "monthly")
    - nrpf_attr: attribute on log to store recs/file (int)
    - vars_attr: attribute to store list of var names (list[str]) — or None to skip
    returns new index
    """
    line0 = log.lines[start_i]

    # detect period, nrpf, monthly in header line
    monthly = False
    period = None
    nrpf = None

    # recs/file
    m = re.search(r"recs/file\s*=\s*(\d+)", line0)
    if m: nrpf = int(m.group(1))

    # period
    m = re.search(r"output_period\s*=\s*([0-9.]+)", line0)
    if m: period = float(m.group(1))

    # monthly flags
    if "monthly_averages" in line0:
        monthly = True

    # assign period/nrpf on the spot
    setattr(log, period_attr, "monthly" if monthly else period)
    setattr(log, nrpf_attr, nrpf)

    # scan down to first dashed line
    dash_re = re.compile(r"^-{4,}$")  # 4+ hyphens
    i = start_i + 1
    while i < len(log.lines) and not dash_re.match(log.lines[i].strip()):
        i += 1
    # skip 2+ dash header block lines
    seen = 0
    while i < len(log.lines) and dash_re.match(log.lines[i].strip()) and seen < 2:
        seen += 1
        i += 1
    # skip optional header titles
    while i < len(log.lines) and not dash_re.match(log.lines[i].strip()):
        i += 1
    # this dashed row ends header
    while i < len(log.lines) and dash_re.match(log.lines[i].strip()):
        i += 1

    # now variable rows until next dashed
    vars = []
    while i < len(log.lines):
        s = log.lines[i].strip()
        if dash_re.match(s):
            break
        # if row looks like "<name> ... T ..."
        if vars_attr:
            parts = s.split()
            if len(parts) >= 2 and parts[1].upper().startswith("T"):
                vars.append(parts[0])
        i += 1

    if vars_attr and vars:
        setattr(log, vars_attr, vars)

    # skip final dashed lines
    while i < len(log.lines) and dash_re.match(log.lines[i].strip()):
        i += 1

    return i


# ------------------- ocean vars -------------------

@register_logparser
def parse_phys_his(log, i):
    if "ocean_vars :: history file" not in log.lines[i]:
        return i
    return _parse_output_block(
        log, i,
        period_attr="output_period_phys_his",
        nrpf_attr="nrpf_phys_his",
        vars_attr="output_variables_phys"
    )

@register_logparser
def parse_phys_avg(log, i):
    if "ocean_vars :: average file" not in log.lines[i]:
        return i
    return _parse_output_block(
        log, i,
        period_attr="output_period_phys_avg",
        nrpf_attr="output_nrpf_phys_avg",
        vars_attr="output_variables_phys"
    )

# ------------------- bgc -------------------

@register_logparser
def parse_bgc_his(log, i):
    if "bgc :: history file" not in log.lines[i].strip().lower():
        return i
    return _parse_output_block(
        log, i,
        period_attr="output_period_bgc_his",
        nrpf_attr="output_nrpf_bgc_his",
        vars_attr="output_variables_bgc"
    )

@register_logparser
def parse_bgc_avg(log, i):
    if "bgc :: average file" not in log.lines[i].strip().lower():
        return i
    return _parse_output_block(
        log, i,
        period_attr="output_period_bgc_avg",
        nrpf_attr="output_nrpf_bgc_avg",
        vars_attr="output_variables_bgc"
    )

# ------------------- cstar -------------------

@register_logparser
def parse_cstar(log, i):
    if "cstar_output ::" not in log.lines[i]:
        return i
    return _parse_output_block(
        log, i,
        period_attr="output_period_cstar",
        nrpf_attr="output_nrpf_cstar",
        vars_attr="output_variables_cstar"
    )

@register_logparser
def parse_cdr_releases(log: "ROMSSimulationLog", i: int) -> int:
    line = log.lines[i]
    if not line.lstrip().startswith("The minimum distance to Release"):
        return i

    if not hasattr(log, "cdr_releases"):
        log.cdr_releases = []

    # we expect the next few lines to contain everything
    # pattern:
    # The minimum distance to Release           N is   <float>
    # This is on rank:         RANK
    # at point          I          J
    # The intended release location was Lon: <lon> Lat: <lat>
    # The release will take place at Lon: <lon> Lat: <lat>

    # release number and distance
    m = re.search(r"Release\s+(\d+)\s+is\s+([0-9.Ee+-]+)", line)
    if not m:
        return i+1
    nr = int(m.group(1))
    dist = float(m.group(2))

    # rank
    rank_line = log.lines[i+1]
    m = re.search(r"rank:\s*(\d+)", rank_line)
    rank = int(m.group(1)) if m else None

    # grid ij
    ij_line = log.lines[i+2]
    m = re.search(r"point\s+(\d+)\s+(\d+)", ij_line)
    ij = (int(m.group(1)), int(m.group(2))) if m else None

    # intended
    intended_line = log.lines[i+3]
    m = re.search(r"Lon:\s*([0-9.Ee+-]+)\s+Lat:\s*([0-9.Ee+-]+)", intended_line)
    intended = (float(m.group(1)), float(m.group(2))) if m else None

    # actual
    actual_line = log.lines[i+4]
    m = re.search(r"Lon:\s*([0-9.Ee+-]+)\s+Lat:\s*([0-9.Ee+-]+)", actual_line)
    actual = (float(m.group(1)), float(m.group(2))) if m else None

    log.cdr_releases.append(
        dict(
            release_number=nr,
            distance=dist,
            grid_location=dict(rank=rank, ij=ij),
            intended_location=dict(lon=intended[0], lat=intended[1]) if intended else None,
            location=dict(lon=actual[0], lat=actual[1]) if actual else None,
        )
    )

    # advance past this 5–6 line block
    return i + 5

if __name__ == "__main__":
    log = ROMSSimulationLog(Path(sys.argv[1]))
    # log = ROMSSimulationLog(Path("roms_out.o14209895"))
    #log = ROMSSimulationLog(Path("../Examples/bgc_real/marbl.log"))

    

    
