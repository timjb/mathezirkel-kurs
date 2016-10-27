#!/usr/bin/env python3

# Call with `python3 frequency-analysis.py skript.tex`

import fileinput
import re
import string
from subprocess import Popen, PIPE

pandoc = Popen('pandoc -f latex -t markdown', shell=True, executable='/bin/bash', stdin=PIPE, stdout=PIPE)

pfa = 'pandoc-frequency-analysis'

pandocState = True
for line in fileinput.input():
    if pandocState:
        if line == '% ' + pfa + ' off\n':
            pandocState = False
        else:
            pandoc.stdin.write(bytes(line, 'UTF-8'))
    elif line == '% ' + pfa + ' on\n':
        pandocState = True

out, err = pandoc.communicate()
pandoc.stdin.close()
pandoc.wait()

text = out.decode(encoding='UTF-8')
filtered = re.sub('\$[^\$]*\$', '', re.sub('</?span>', '', text))
replaced = filtered.replace('ß', 'ss').replace('ä', 'ae').replace('ü', 'ue').replace('ö', 'oe').replace('Ä', 'AE').replace('Ü', 'UE').replace('Ö', 'OE')
only_chars = re.sub('[^a-zA-Z]', '', replaced)
only_upper_chars = only_chars.upper()

frequencies = dict()

for letter in string.ascii_uppercase:
    frequencies[letter] = 0

for letter in only_upper_chars:
    frequencies[letter] += 1

i = 0
print('X-Position Letter Count Frequency')
l = len(only_upper_chars)
for letter in string.ascii_uppercase:
    f = frequencies[letter]
    print(str(i).ljust(11) + letter + '      ' + str(f).ljust(6) + str(f/l))
    i += 1
