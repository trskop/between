#!/bin/sh

HLINT_FLAGS='--colour'

if [ -d 'src' ]; then
    hlint $HLINT_FLAGS src

    if [ -d 'test' ]; then
        hlint $HLINT_FLAGS --ignore='Use camelCase' test
    fi
    
    if [ -d 'tests' ]; then
        hlint $HLINT_FLAGS --ignore='Use camelCase' tests
    fi
else
    hlint $HLINT_FLAGS .
fi
