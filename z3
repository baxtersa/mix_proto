#!/bin/bash

rm -f log.txt
tee -a log.txt | z3 $@ | tee -a log.txt