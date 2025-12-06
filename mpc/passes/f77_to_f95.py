# passes/f77_to_f95.py

def _first_nonblank(text):
    i = 0
    while i < len(text) and text[i] in (" ", "\t"):
        i += 1
    return i


def _fix_integer(text):
    """
    INTEGER*X → INTEGER(kind=X)
    default INTEGER → INTEGER(kind=4)
    INTEGER(kind=...) left unchanged
    """
    l = len(text)
    istr = _first_nonblank(text)
    if istr + 6 >= l:
        return text

    if text[istr:istr+7].lower() != "integer":
        return text

    i = istr + 6   # index of 'R'
    j = i + 1

    # skip blanks
    while j < l and text[j] == " ":
        j += 1

    # INTEGER*X
    if j < l and text[j] == "*":
        k = j + 1
        while k < l and text[k].isdigit():
            k += 1
        size = text[j+1:k]
        if size:
            return text[:i+1] + f"(kind={size})" + text[k:]
        return text

    # INTEGER(...)
    if j < l and text[j] == "(":
        return text  # already F90-ish

    # default-size INTEGER
    is_default = (j > i+1) or j >= l or text[j] in (" ", ",")
    if is_default:
        return text[:i+1] + "(kind=4)" + text[i+1:]

    return text


def _fix_real(text):
    """
    REAL*X → REAL(kind=X)
    default REAL → REAL(kind=8)
    REAL(kind=...) left unchanged
    """
    l = len(text)
    istr = _first_nonblank(text)
    if istr + 3 >= l:
        return text

    if text[istr:istr+4].lower() != "real":
        return text

    i = istr + 3
    j = i + 1

    while j < l and text[j] == " ":
        j += 1


    # REAL*X  where X may be digits or a CPP macro (e.g. QUAD)
    if j < l and text[j] == "*":
        k = j + 1
        # Accept any token up to comma, whitespace, or parenthesis.
        while k < l and text[k] not in (" ", "\t", ",", ")"):
            k += 1

        size = text[j+1:k]
        if size:
            return text[:i+1] + f"(kind={size})" + text[k:]
        return text

    # REAL(...)
    if j < l and text[j] == "(":
        return text

    # default REAL
    is_default = (j > i+1) or j >= l or text[j] in (" ", ",")
    if is_default:
        return text[:i+1] + "(kind=8)" + text[i+1:]

    return text


def _fix_character(text):
    """
    CHARACTER*X → CHARACTER(len=X)
    CHARACTER(len=...) left unchanged
    """
    l = len(text)
    istr = _first_nonblank(text)
    if istr + 8 >= l:
        return text

    if text[istr:istr+9].lower() != "character":
        return text

    i = istr + 8
    j = i + 1

    while j < l and text[j] == " ":
        j += 1

    # CHARACTER*(X) or CHARACTER*X
    if j < l and text[j] == "*":
        k = j + 1

        # parenthesized form: CHARACTER*(10)
        if k < l and text[k] == "(":
            k2 = k + 1
            while k2 < l and text[k2].isdigit():
                k2 += 1
            if k2 < l and text[k2] == ")":
                size = text[k+1:k2]
                end = k2 + 1
                return text[:i+1] + f"(len={size})" + text[end:]
            return text

        # simple form: CHARACTER*10
        k2 = k
        while k2 < l and text[k2].isdigit():
            k2 += 1
        size = text[k:k2]
        if size:
            return text[:i+1] + f"(len={size})" + text[k2:]
        return text

    # CHARACTER(...)
    if j < l and text[j] == "(":
        return text  # leave as-is

    # default CHARACTER — MPC does NOT assign defaults
    return text


def pass_f77_to_f95(records, cfg):
    """
    Apply full F77→F95 conversion:
      - INTEGER*size → INTEGER(kind=size)
      - REAL*size    → REAL(kind=size)
      - CHARACTER*size → CHARACTER(len=size)
      - INTEGER → INTEGER(kind=4)
      - REAL → REAL(kind=8)

    This mirrors the structure of MPC's giant F77_TO_F95 block.
    """

    out = []
    for rec in records:
        text = rec["text"]

        # try each type transform in order
        new_text = _fix_integer(text)
        if new_text == text:
            new_text = _fix_real(text)
        if new_text == text:
            new_text = _fix_character(text)

        if new_text != text:
            new_rec = dict(rec)
            new_rec["text"] = new_text
            new_rec["lstr"] = len(new_text) - 1

            istr = 0
            while istr < len(new_text) and new_text[istr] in (" ", "\t"):
                istr += 1
            new_rec["istr"] = istr

            out.append(new_rec)
        else:
            out.append(rec)

    return out
