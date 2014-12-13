#!/bin/bash
set -e
# ./dependencies.sh commands criterion

./map.sh ./depends.sh $@ | gsort | uniq | python3 -c 'import sys; [print("-package " + "-".join(line.strip().split("-")[:-1])) for line in sys.stdin]' | tr "\n" " "
