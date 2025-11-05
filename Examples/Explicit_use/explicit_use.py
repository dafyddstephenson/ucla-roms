import subprocess
import re

def find_first_use_statement(file_path):
    with open(file_path, 'r') as f:
        module_lines = f.readlines()

    for i, line in enumerate(module_lines):
        # Skip comments and explicit 'only:' lines
        if line.strip().startswith('!') or ', only:' in line.lower():
            continue
        if line.strip().lower().startswith('use '):
            # Store the original indent so it never changes
            indent = line[:len(line) - len(line.lstrip())]
            return i, line, indent
            # Get full statement for debugging
            # j=i
            # statement_lines=[line,]
            # while j < len(module_lines):
            #    if len(module_lines[j+1] >=6) and module_lines[j+1][5] == "&":
            #        statement_lines.append(module_lines[j+1])
            #        j=j+1
            # return j , statement_lines, indent
        
    return None, None, None


def update_use_statement(file_path, start_line, missing, indent):
    with open(file_path, 'r') as f:
        lines = f.readlines()

    # Flatten multi-line use statements into one line (we can wrap again later)
    flat_use_statement = lines[start_line]
    j = start_line + 1
    while j < len(lines):
        if len(lines[j]) >= 6 and lines[j][5] == "&":
            flat_use_statement += lines[j][6:]
            j += 1
        else:
            break
    end_line = j
    base = flat_use_statement.split(', only:')[0].strip()
    existing = []
    if ', only:' in flat_use_statement:
        existing_part = flat_use_statement.split(', only:')[1]
        existing = [s.strip() for s in existing_part.split(',') if s.strip()]
    new_objs = existing + [x for x in missing if x not in existing]

    # Wrap according to fixed-form rules
    new_use_lines = wrap_fixed_form_use_statement(base, new_objs, indent)

    # Replace the old lines
    lines[start_line:end_line] = new_use_lines
    with open(file_path, 'w') as f:
        f.writelines(lines)

def wrap_fixed_form_use_statement(base, objlist, indent):
    use_line = f"{indent}{base}, only: "
    output_lines = []
    current_line = use_line
    char_limit = 72

    for i, obj in enumerate(objlist):
        part = obj
        if i < len(objlist) - 1:
            part += ", "
        # Would adding this part exceed the limit?
        if len(current_line) + len(part) > char_limit:
            output_lines.append(current_line.rstrip() + "\n")
            # Continuation line: 5 spaces, & in col 6, then pad to match indent after &
            continuation = "     & "
            pad = ""
            current_line = continuation + pad
        current_line += part

    output_lines.append(current_line.rstrip() + "\n")
    return output_lines
        
def comment_line(file_path, line_num):
    with open(file_path, 'r') as f:
        lines = f.readlines()
    if not lines[line_num].lstrip().startswith('!'):
        lines[line_num] = '!' + lines[line_num]
    with open(file_path, 'w') as f:
        f.writelines(lines)
        
def uncomment_line(file_path, line_num):
    with open(file_path, 'r') as f:
        lines = f.readlines()
    if lines[line_num].startswith('!'):
        lines[line_num] = lines[line_num][1:]
    with open(file_path, 'w') as f:
        f.writelines(lines)
        

def run_make_and_report_first_error():
    process = subprocess.run(['make'], capture_output=True, text=True)
    output = process.stdout + process.stderr

    for line in output.splitlines():
        if 'has no IMPLICIT type' in line:
            m = re.search(r"'([^']+)'", line)
            if m:
                return [line], [m.group(1)], process.returncode

    # If no "IMPLICIT type" error found, but non-zero exit code, raise
    if process.returncode != 0:
        raise RuntimeError(
            "Build failed and no missing implicit type was detected.\n"
            f"Output: {output}"
        )

    # If build succeeded or no errors, return normally
    return [], [], process.returncode

if __name__ == "__main__":
    file_path = "basic_output.F"
    line_num, use_statement_first_line, indent = find_first_use_statement(file_path)
    print(f"Found use statement at line {line_num}: {use_statement_first_line} (...)")
    comment_line(file_path,line_num)
    # print(f"found {original_use} at line {line_num}")
    if line_num is None:
        print("No non-explicit use statement found.")
        exit(1)
    while True:
        error_lines, missing, code = run_make_and_report_first_error()
        print("First error:", error_lines)
        print("Missing symbols:", missing)
        print("Make exit code:", code)
        uncomment_line(file_path, line_num)
        if code == 0 or not missing:
            print(f"make completed successfully, `{use_statement_first_line.strip()}` updated")
            break
        update_use_statement(file_path, line_num, missing, indent)

# TODO this just looks for the word "error" which is a red herring - we should be finding 'no IMPLICIT type' errors only, and raising only if there's an exit code 1 and we didn't have any
