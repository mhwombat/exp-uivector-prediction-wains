#!/bin/bash
source ./config.sh
./trendSummary.sh | csvcut -c 1,2,45 | csvlook

