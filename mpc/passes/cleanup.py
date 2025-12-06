# passes/cleanup.py

def pass_cleanup(lines, cfg):
    """
    Translation-faithful implementation of the initial MPC preprocessing stage.

    Replicates:
      - comment stripping rules
      - detection of OpenMP compiler directives (C$OMP, !$OMP, *$OMP)
      - quote state machine to determine where '!' starts a comment
      - locating first/last nonblank
      - skipping blank lines
    """

    processed = []
    line_number = 0

    for raw in lines:
        line_number += 1

        # Normalize to fixed-width, like the Fortran code’s 2*max_length buffer
        s = raw.rstrip("\n")
        # MPC logic assumes 1-based indexing; Python uses 0-based,
        # so we shift when needed but keep internal positions 0-based.

        # Detect OpenMP directive sentinel
        omp_dir = False
        if len(s) > 0:
            c0 = s[0]
            if c0 in ("C", "c", "!", "*"):
                # Check columns 2–5 (Fortran 1-based → Python 1–4)
                if len(s) >= 5 and (
                    s[1] == '$' or s[2] == '$' or s[3] == '$' or s[4] == '$'
                ):
                    omp_dir = True
                else:
                    # This is an ordinary comment → skip line
                    continue

        # Begin quote state machine
        # type values ported from Fortran:
        #   ' ' — normal, outside quotes
        #   's' — inside single quotes
        #   'd' — inside double quotes
        #   'S' — inside double inside single (nested)
        #   'D' — inside single inside double (nested)
        type_state = ' '
        quote = "'"
        double_quote = '"'

        last_nonblank = -1
        truncated = []  # character list

        i = 0
        while i < len(s):
            ch = s[i]

            # Handle single quotes
            if ch == quote:
                if type_state == ' ':
                    type_state = 's'
                elif type_state == 's':
                    type_state = ' '
                elif type_state == 'd':
                    type_state = 'S'
                elif type_state == 'S':
                    type_state = 'd'
                elif type_state == 'D':
                    # illegal in original, toggles differently
                    type_state = 'd'

            # Handle double quotes
            elif ch == double_quote:
                if type_state == ' ':
                    type_state = 'd'
                elif type_state == 'd':
                    type_state = ' '
                elif type_state == 's':
                    type_state = 'D'
                elif type_state == 'D':
                    type_state = 's'
                elif type_state == 'S':
                    type_state = 's'

            # Handle comment '!'
            elif ch == '!' and type_state == ' ' and not (omp_dir and i == 0):
                # Stop copying at start of comment
                break

            # Record position of last nonblank
            if ch not in (" ", "\t"):
                last_nonblank = i

            truncated.append(ch)
            i += 1

        # If nothing remains (empty after stripping), skip
        if last_nonblank < 0:
            continue

        text = "".join(truncated[:last_nonblank+1])

        # Find first nonblank
        first_nonblank = 0
        while first_nonblank < len(text) and text[first_nonblank] in (" ", "\t"):
            first_nonblank += 1

        if first_nonblank >= len(text):
            continue  # fully blank

        # Store quote error flag (actual error reporting handled in pass_trap_unmatched_quotes)
        unmatched = (type_state != ' ')

        processed.append({
            "raw": raw,
            "line": line_number,
            "text": text,
            "istr": first_nonblank,
            "lstr": len(text) - 1,
            "omp_dir": omp_dir,
            "unmatched_quotes": unmatched
        })

    return processed
