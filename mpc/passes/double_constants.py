# passes/double_constants.py

def pass_double_constants(records, cfg):
    """
    Faithful translation of the DOUBLE_CONST block from MPC.
    Text is assumed 0-based; entire line is considered valid
    search space (Fortran fixed columns no longer apply).
    """

    KIND_STYLE = True    # corresponds to #define KIND_STYLE
    RUTGERS = False      # corresponds to #define RUTGERS

    def is_digit(ch):
        return '0' <= ch <= '9'

    def is_letter(ch):
        return ('A' <= ch <= 'Z') or ('a' <= ch <= 'z')

    for rec in records:
        text = rec["text"]
        s = list(text)
        lstr = rec.get("lstr", len(s) - 1)

        # Safe char access (blank beyond bounds)
        def char_at(i):
            if 0 <= i < len(s):
                return s[i]
            return ' '

        # -------------------------------------------------------
        # (1) Collect positions of dots that are OUTSIDE quotes.
        # -------------------------------------------------------
        dots = []
        in_string = False
        open_quote = None  # track quote type (' or ")

        for i in range(0, lstr + 1):
            ch = char_at(i)
            if ch in ("'", '"'):
                if not in_string:
                    in_string = True
                    open_quote = ch
                elif open_quote == ch:
                    in_string = False
                    open_quote = None
            elif ch == '.' and not in_string:
                dots.append(i)

        # -------------------------------------------------------
        # Process from RIGHT to LEFT
        # -------------------------------------------------------
        for dot_index in reversed(dots):
            m = dot_index
            is_idx = m  # leftmost digit
            # Step (2): scan LEFT for digits; stop on first nonblank nondigit
            left_scan_ptr = m
            hit_col0 = False

            while left_scan_ptr > 0:
                left_scan_ptr -= 1
                ch = char_at(left_scan_ptr)
                if is_digit(ch):
                    is_idx = left_scan_ptr
                elif ch != ' ':
                    break
                if left_scan_ptr == 0:
                    hit_col0 = True
                    break

            prev_char = char_at(left_scan_ptr)

            # Step (3): reject if previous symbol is a letter (unless hit col0)
            if (not hit_col0) and is_letter(prev_char):
                continue  # reject candidate

            # ---------------------------------------------------
            # Step (4): scan RIGHT for digits & first nonblank nondigit
            # ---------------------------------------------------
            right_scan_ptr = dot_index
            ie_idx = dot_index

            while right_scan_ptr < lstr:
                right_scan_ptr += 1
                ch = char_at(right_scan_ptr)
                if is_digit(ch):
                    ie_idx = right_scan_ptr
                elif ch != ' ':
                    break

            # After loop, right_scan_ptr is first nonblank nondigit
            after = char_at(right_scan_ptr)

            # If no digits around dot → not a real constant
            if not (is_idx < ie_idx):
                continue

            # ---------------------------------------------------
            # Step (5a): exponent letter e/E/d/D
            # ---------------------------------------------------
            if after in ('e', 'E', 'd', 'D'):
                # skip blanks to find exponent sign or digit
                p = right_scan_ptr + 1
                while p <= lstr and char_at(p) == ' ':
                    p += 1
                chp = char_at(p)
                if chp in ('+', '-') or is_digit(chp):
                    # valid exponent → convert to D
                    if 0 <= right_scan_ptr < len(s):
                        s[right_scan_ptr] = 'D'
                continue

            # ---------------------------------------------------
            # Step (5b): underscore suffix (RUTGERS not used here)
            # ---------------------------------------------------
            if after == '_':
                # Standard F90 kind: 1.0_4 or 1.0_8 → leave unchanged
                # Optional RUTGERS mode not enabled
                continue

            # ---------------------------------------------------
            # Step (5c): no exponent letter → append D0 or _8
            # ---------------------------------------------------
            if not is_letter(after):
                insert_at = ie_idx + 1

                # # Move left to skip blanks before insertion
                # insert_at = right_scan_ptr
                # while char_at(insert_at - 1) == ' ' and insert_at > 0:
                #     insert_at -= 1

                annotation = '_8' if KIND_STYLE else 'D0'
                tail = s[insert_at:lstr + 1]
                s = s[:insert_at] + list(annotation) + tail
                lstr += 2
                # continue scanning leftward dots
                continue

        # Finalize record
        rec["text"] = "".join(s)
        rec["lstr"] = len(s) - 1

    return records
