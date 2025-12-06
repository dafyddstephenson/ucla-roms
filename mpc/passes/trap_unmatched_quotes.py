# passes/trap_unmatched_quotes.py

def pass_trap_unmatched_quotes(records, cfg):
    """
    Faithful recreation of MPC's TRAP_UNMATCHED_QUOTES:

        /6x, '### ERROR: Unmatched quote on line ', I4, /

    Which emits:
        <blank line>
              ### ERROR: Unmatched quote on line XXX
        <blank line>
    """

    out = []

    for rec in records:
        if rec.get("unmatched_quotes", False):
            line_no = rec["line"]
            formatted = f"      ### ERROR: Unmatched quote on line{line_no:4d}"

            # --- Blank line BEFORE ---
            out.append({
                "raw": "",
                "line": line_no,
                "text": "",
                "istr": 0,
                "lstr": -1,
                "omp_dir": False,
                "unmatched_quotes": False,
            })

            # --- ERROR line itself ---
            out.append({
                "raw": "",
                "line": line_no,
                "text": formatted,
                "istr": 0,
                "lstr": len(formatted) - 1,
                "omp_dir": False,
                "unmatched_quotes": False,
            })

            # --- Blank line AFTER ---
            out.append({
                "raw": "",
                "line": line_no,
                "text": "",
                "istr": 0,
                "lstr": -1,
                "omp_dir": False,
                "unmatched_quotes": False,
            })

        # Always append the original record
        out.append(rec)

    return out
