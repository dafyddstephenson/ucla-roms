# passes/zig_zag.py

def pass_zig_zag(records, cfg):
    """
    Literal translation of the ZIG_ZAG block in MPC.
    Not the old ZIG_ZAG_OLD variant. This is the 'modern' one:

        if (str(i:i+1) == 'do') ...
        if (str(i:i+3) == 'tile') ...
        if (str(i:i+2) == 'my_') ...

    The transformation toggles a global direction flag and rewrites:
        do tile = my_first, my_last
    into:
        do tile = my_first, my_last, +1
    or:
        do tile = my_last, my_first, -1
    depending on the toggle state.

    Notes:
      - Must track a persistent direction toggle across lines.
      - Must only match literal 'do tile=' followed by 'my_'.
      - Uses rec["istr"] / rec["lstr"] already computed by cleanup pass.
      - Only rewrites the single line; does not inject new loops here.
    """

    out = []
    dir_switch = False  # Persistent across lines, just like in MPC

    # helper: replace substring at index range (inclusive) with a given new text
    def replace_slice(text, start, end, replacement):
        return text[:start] + replacement + text[end+1:]

    for rec in records:
        text = rec["text"]
        istr = rec.get("istr", 0)
        lstr = rec.get("lstr", len(text)-1)

        # Defensive filtering: skip lines too short to match
        if lstr < istr + 5:
            out.append(rec)
            continue

        # Literal match of Fortran code:
        #
        #    do tile = my_xxxxxxx
        #
        # The original MPC matches:
        #   str(i:i+1) == 'do'
        #   skip spaces
        #   str(i:i+3) == 'tile'
        #   skip spaces
        #   expect '='
        #   skip spaces
        #   str(i:i+2) == 'my_'
        #
        i = istr

        # Step 1: match 'do'
        if i + 1 <= lstr and text[i:i+2] == "do":
            i += 2
        else:
            out.append(rec)
            continue

        # Skip spaces
        while i < lstr and text[i] == ' ':
            i += 1

        # Step 2: match 'tile'
        if i + 3 <= lstr and text[i:i+4] == "tile":
            i += 4
        else:
            out.append(rec)
            continue

        # Skip spaces
        while i < lstr and text[i] == ' ':
            i += 1

        # Step 3: expect '='
        if i <= lstr and text[i] == '=':
            i += 1
        else:
            out.append(rec)
            continue

        # Skip spaces
        while i < lstr and text[i] == ' ':
            i += 1

        # Step 4: match 'my_'
        if i + 2 > lstr or text[i:i+3] != "my_":
            out.append(rec)
            continue

        # ✔ At this point, the line matches the ZIG_ZAG pattern.
        # Toggle direction:
        dir_switch = not dir_switch

        # MPC inserts one of:
        #
        #   my_first,my_last,+1
        #   my_last,my_first,-1
        #
        # It literally overwrites characters starting at position i.
        #
        # In the Fortran, this insertion is:
        #
        #   scratch='my_first,my_last,+1'
        # or
        #   scratch='my_last,my_first,-1'
        #
        # then:
        #   str(i:i+18) = scratch(1:19)
        #
        # And lstr = i+18.

        if dir_switch:
            replacement = "my_first,my_last,+1"
        else:
            replacement = "my_last,my_first,-1"

        # replacement length is 19 chars (index 0..18)
        new_text = (
            text[:i] +
            replacement +
            text[i+19:]  # Overwrite 19 characters exactly, like MPC does
        )

        # But MPC actually guarantees the string is exactly i..i+18 long,
        # and truncates the rest of the line by setting:
        #    lstr = i+18
        #
        # So we truncate fully (literal behavior):
        new_text = text[:i] + replacement

        new_rec = dict(rec)
        new_rec["text"] = new_text
        new_rec["lstr"] = len(new_text) - 1

        out.append(new_rec)

    return out
