#!/bin/bash

find tests/$1 -name '*.sol' |
    xargs -n 1 bash -c "echo \"\$0: \"; cat \$0 | ./Test" 
