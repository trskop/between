#!/bin/bash

# Colorize output if it's a terminal
if [ -t 1 ]; then
    HLINT_FLAGS='--colour'
fi

DIRS=''
for PKG in ''; do
    for D in 'src' 'test' 'tests'; do
        if [ -n "$PKG" ]; then
            D="$PKG/$D"
        fi
        if [ -d "$D" ]; then
            if [ -z "$DIRS" ]; then
                DIRS="$D"
            else
                DIRS="$DIRS $D"
            fi
        fi
    done
done

# Use current directory if no known subdirectories with source code were found.
if [ -z "$DIRS" ]; then
    DIRS='.'
fi

hlint $HLINT_FLAGS $DIRS
