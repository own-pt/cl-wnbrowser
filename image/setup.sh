#!/usr/bin/env bash

cd quicklisp/local-projects
git clone https://github.com/own-pt/cl-wnbrowser.git

while IFS= read -r line
do
    echo "cloning $line"
    git clone $line
done < cl-wnbrowser/dependecies

cd /root
