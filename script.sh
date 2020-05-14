#!/bin/bash
set -e
for i in 1 2 3
do
  echo "sleeping for $i seconds"
  sleep $i
done
echo "done power napping!"
exit 0
