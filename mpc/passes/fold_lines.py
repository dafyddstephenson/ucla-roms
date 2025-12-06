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

        # print("\n--- FOLD DEBUG ---")
        # print("Original text:", text)
        # print("Length:", len(text), "lstr:", lstr)
        ks0 = ks1 = ks2 = ks3 = ks4 = ks5 = 0

        # Fortran: DO k = 7, 72   (1-based)
        # Python: indices 6..72 inclusive (0-based)
        for k in range(6, min(72, lstr)):
#        for k in range(6, min(72, lstr) + 1):
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
            elif ch == '+' or ch == '-' or ch != '*':
                ks5 = k
            # elif ch in ['+', '-', '*']:
            #     ks5 = k

        # print("Raw ks values:")
        # print("  ks0 (;) =", ks0)
        # print("  ks1 (space) =", ks1)
        # print("  ks2 (,) =", ks2)
        # print("  ks3 (=) =", ks3)
        # print("  ks4 (/) =", ks4)
        # print("  ks5 (+-*) =", ks5)

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

        # print("Filtered ks values:")
        # print("  ks0 =", ks0)
        # print("  ks1 =", ks1)
        # print("  ks2 =", ks2)
        # print("  ks3 =", ks3)
        # print("  ks4 =", ks4)
        # print("  ks5 =", ks5)

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
        split = None
        if ks0 > 5:
            split = ks0
            reason = "ks0 (semicolon)"
        elif ks1 > 33:
            split = ks1
            reason = "ks1 (space >34)"
        elif ks4 > 5:
            split = ks4 - 1
            reason = "ks4 (slash)"
        elif ks2 > 53:
            split = ks2
            reason = "ks2 (comma >54)"
        elif ks3 > 59:
            split = ks3
            reason = "ks3 (equals >60)"
        elif ks5 > 5:
            split = ks5 - 1
            reason = "ks5 (operator)"
        else:
            # print("NO VALID SPLIT FOUND — emitting as-is")
            new_rec = rec.copy()
            new_rec["lstr"] = len(text) - 1
            out_records.append(new_rec)
            continue
        # print("Chosen split index:", split, "Reason:", reason)
        # ----------------------------------------------------------
        # Step 3: First line segment
        # ----------------------------------------------------------
        # MPC keeps trailing whitespace on the line being wrapped.
        first_text = "".join(s[:split + 1])
        # print("First segment:", repr(first_text))

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

        # ----------------------------------------------------------
        # This section replicates behaviour in the original MPC to
        # set a logical 'omp_dir' as true if a line:
        #   - begins with C/c/*/!
        #   - contains at least one '$' in columns 2-5
        # In this case, cols 1-5 are copied on the continuation line
        # ----------------------------------------------------------
        is_comment = rec["text"].startswith(("C", "c", "*", "!"))

        has_dollar_in_cols_2_to_5 = False
        txt = rec["text"]
        for i in range(1, min(5, len(txt))):   # indices 1..4 = cols 2..5
            if txt[i] == '$':
                has_dollar_in_cols_2_to_5 = True
                break

        is_directive = is_comment and has_dollar_in_cols_2_to_5

        if is_directive:
            # Copy columns 1–5 exactly from original text
            for i in range(5):
                if i < len(txt):
                    s2[i] = txt[i]
        else:
            # Non-directive comment or normal code: blanks
            s2[:5] = ' '
        # End of section reproducing weird behaviour
        #-----------------------------------------------------------


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
        # print("Tail length =", len(tail))
        # print("Placing tail from", start, "to", end)

        if len(s2) < end:
            s2.extend([' '] * (end - len(s2)))

        s2[start:end] = tail

        lcont = lstr - m
        if lcont < 0:
            lcont = 0

        cont_text = "".join(s2[:lcont + 1]).rstrip()
        # print("Continuation segment:", repr(cont_text))
        # print("--- END DEBUG ---\n")
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
