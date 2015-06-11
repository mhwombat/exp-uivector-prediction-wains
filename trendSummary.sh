#!/bin/bash
source ./config.sh

grep Summary $log | \
  sed 'N; N; N; N; s/\n[^,]*Summary - /,/g' | \
  sed 's/.*\t\([0-9]*\)\tSummary - /round=\1,/; 1d; 2s/\(.*\)/\1\n\1/' | \
  sed '1s/=[^,]*//g; 2,$s/round=//; 2,$s/,[^=]*=/,/g'

