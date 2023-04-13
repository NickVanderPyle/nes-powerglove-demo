import sys
import os

if len(sys.argv) < 2:
    sys.exit()

filename = sys.argv[1]
filename = os.path.abspath(filename)

nl_filename = filename.rsplit('.', 1)[0] + '.0.nl'

with open(filename, 'r') as infile:
    with open(nl_filename, 'w') as nl_file:
        for line in infile:
            if line.startswith("sym"):
                parts = line.split(",")
                sym_name = next((x.split("=")[1].strip('"').lstrip('.') for x in parts if x.startswith("name=")), None)
                sym_addr = next((x.split("=")[1].strip('"').lstrip('0x') for x in parts if x.startswith("val=")), None)
                if not sym_name.startswith("__"):
                    address = int(sym_addr, 16)
                    nl_file.write(f"${address:04X}#{sym_name}#\n")

print(f'Conversion complete. Output file: {nl_filename}')