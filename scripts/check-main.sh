#!/bin/sh

if git branch | grep -q 'main'; then
    echo "'main' branch exists."
else
    echo "'main' branch does not exist."
    exit 1
fi