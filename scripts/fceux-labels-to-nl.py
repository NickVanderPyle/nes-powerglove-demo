import sys
import os

if len(sys.argv) < 2:
    sys.exit()

filename = sys.argv[1]
filename = os.path.abspath(filename)

nl_filename = filename.rsplit('.', 1)[0] + '.ram.nl'

with open(filename, 'r') as infile:
    with open(nl_filename, 'w') as nl_file:
        for line in infile:
            if line.startswith("al"):
                parts = line.strip().split()
                address = int(parts[1], 16)
                name = parts[2].lstrip(".")
                if not name.startswith("__"):
                    nl_file.write(f"${address:04X}#{name}#\n")

print(f'Conversion complete. Output file: {nl_filename}')