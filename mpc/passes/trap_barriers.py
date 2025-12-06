# passes/trap_barriers.py

def pass_trap_barriers(records, cfg):
    """
    Literal translation of MPC's TRAP_BARRIERS logic.

    Conditions (mirroring the Fortran):

      - Column 1 must be 'C', 'c', or '!'.
      - Columns 2–5 must be '$OMP' or '$omp'.
      - Starting from column 6, skip spaces.
      - If you can read 7 characters and they spell 'BARRIER'
        (case-insensitive), it's a barrier directive.

    Anything after 'BARRIER' is allowed (including 'nowait',
    comments, etc.). No token-boundary checks.
    """

    out = []
    ibarr = 0  # barrier counter

    for rec in records:
        text = rec["text"]
        lstr = rec.get("lstr", len(text) - 1)  # last significant char (0-based)

        # Need at least 6 characters (C$OMP + something)
        if lstr < 5:
            out.append(rec)
            continue

        # Column 1 (Fortran 1-based) -> text[0]
        first = text[0]
        if first not in ("C", "c", "!"):
            out.append(rec)
            continue

        # Columns 2–5 (Fortran) -> text[1:5]
        sentinel = text[1:5]
        if sentinel not in ("$OMP", "$omp"):
            out.append(rec)
            continue

        # is = 6 (Fortran) -> i = 5 (0-based)
        i = 5
        # Skip spaces while i maps to is < lstr and char is ' '
        while i <= lstr and text[i] == " ":
            i += 1

        # Need 7 characters from i: text[i:i+7] must be 'BARRIER'
        # Safest: check bounds directly.
        if i + 6 > lstr:
            out.append(rec)
            continue

        candidate = text[i:i+7]
        if candidate.lower() != "barrier":
            out.append(rec)
            continue

        # Valid barrier directive: insert sync_trap before it
        ibarr += 1
        # Fortran writes '(I4)' into scratch and then strips leading blanks,
        # so effectively just the integer as a string.
        trap_line = f"C$    call sync_trap({ibarr})"

        trap_rec = {
            "raw": "",
            "line": rec["line"],
            "text": trap_line,
            "istr": 0,
            "lstr": len(trap_line) - 1,
            "omp_dir": False,
            "unmatched_quotes": False,
        }

        out.append(trap_rec)
        out.append(rec)

    return out
