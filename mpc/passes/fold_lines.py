def pass_fold_lines(records, cfg):
    """
    Faithful translation of the MPC line-folding logic.
    Produces new record dicts, NOT raw strings.
    """

    out_records = []

    for rec in records:
        text = rec["text"]
        omp_dir = rec["omp_dir"]

        # No folding needed:
        if len(text) <= 72:
            new_rec = rec.copy()
            new_rec["lstr"] = len(text) - 1

            # first nonblank
            istr = 0
            while istr < len(text) and text[istr] in (" ", "\t"):
                istr += 1
            new_rec["istr"] = istr

            out_records.append(new_rec)
            continue

        # Work with char list
        s = list(text)
        lstr = len(s) - 1  # last index (0-based)

        # ----------------------------------------------------------
        # Step 1: find candidate split positions  (ks0..ks5)
        # ----------------------------------------------------------
        ks0 = ks1 = ks2 = ks3 = ks4 = ks5 = 0

        # Fortran: DO k = 7, 72   (1-based)
        # Python: indices 6..72 inclusive (0-based)
        for k in range(6, min(72, lstr) + 1):
            ch = s[k]
            if ch == ';':
                ks0 = k
            elif ch == ' ':
                ks1 = k
            elif ch == ',':
                ks2 = k
            elif ch == '=':
                ks3 = k
            elif ch == '/':
                ks4 = k
            elif ch in ['+', '-', '*']:
                ks5 = k

        def filter_ks(ks):
            # Fortran: IF (lstr - ks1 > 66) ks1 = 0
            # lstr and ks are both 1-based there, difference is the same
            if ks > 0 and (lstr - ks > 66):
                return 0
            return ks

        ks1 = filter_ks(ks1)
        ks2 = filter_ks(ks2)
        ks3 = filter_ks(ks3)
        ks4 = filter_ks(ks4)
        ks5 = filter_ks(ks5)

        # ----------------------------------------------------------
        # Step 2: choose split point exactly as MPC
        # ----------------------------------------------------------
        # Fortran (1-based):
        #   IF (ks0 > 6)  k = ks0
        #   ELSEIF (ks1 > 34) k = ks1
        #   ELSEIF (ks4 > 6)  k = ks4-1
        #   ELSEIF (ks2 > 54) k = ks2
        #   ELSEIF (ks3 > 60) k = ks3
        #   ELSEIF (ks5 > 6)  k = ks5-1
        #
        # Our ks* are 0-based, so thresholds are shifted by 1.
        if ks0 > 5:           # 6 (0-based) == column 7
            split = ks0
        elif ks1 > 33:        # column >34 → index >33
            split = ks1
        elif ks4 > 5:
            split = ks4 - 1
        elif ks2 > 53:
            split = ks2
        elif ks3 > 59:
            split = ks3
        elif ks5 > 5:
            split = ks5 - 1
        else:
            # Cannot split → emit as is
            new_rec = rec.copy()
            new_rec["lstr"] = len(text) - 1
            out_records.append(new_rec)
            continue

        # ----------------------------------------------------------
        # Step 3: First line segment
        # ----------------------------------------------------------
        # MPC keeps trailing whitespace on the line being wrapped.
        first_text = "".join(s[:split + 1])

        first_rec = rec.copy()
        first_rec["text"] = first_text
        first_rec["lstr"] = len(first_text) - 1

        istr = 0
        while istr < len(first_text) and first_text[istr] in (" ", "\t"):
            istr += 1
        first_rec["istr"] = istr

        out_records.append(first_rec)

        # ----------------------------------------------------------
        # Step 4: Build continuation line buffer
        # ----------------------------------------------------------
        # Continuation line is always:
        #   cols 1–5: blanks
        #   col 6: '&'
        #   col >=7: centered tail
        # regardless of omp_dir. That’s what MPC does.
        s2 = [' '] * max(len(s) + 80, 80)
        s2[5] = '&'  # column 6

        # ----------------------------------------------------------
        # Step 5: recenter tail (MPC logic)
        # ----------------------------------------------------------
        # We keep the literal arithmetic but in 0-based indices, which
        # matches MPC’s integer math for m when combined with split/lstr.
        m = (lstr + split - 79) // 2
        if (lstr - m) > 72:
            m = lstr - 72

        tail = s[split + 1: lstr + 1]

        # Your empirical fix: start = split - m (no +1).
        start = split - m
        if start < 6:
            start = 6  # don’t overwrite the '&' in col 6 (idx 5)

        end = start + len(tail)
        if len(s2) < end:
            s2.extend([' '] * (end - len(s2)))

        s2[start:end] = tail

        lcont = lstr - m
        if lcont < 0:
            lcont = 0

        # MPC trims the continuation line’s trailing spaces.
        cont_text = "".join(s2[:lcont + 1]).rstrip()

        cont_rec = {
            "raw": rec["raw"],
            "line": rec["line"],  # continuation inherits original line index
            "text": cont_text,
            "omp_dir": omp_dir,
            "unmatched_quotes": rec["unmatched_quotes"],
        }

        cont_rec["lstr"] = len(cont_text) - 1
        istr = 0
        while istr < len(cont_text) and cont_text[istr] in (" ", "\t"):
            istr += 1
        cont_rec["istr"] = istr

        out_records.append(cont_rec)

    return out_records
