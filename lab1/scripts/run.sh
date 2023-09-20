#!/bin/bash

INPUT_FILE=$1
OUTPUT_FILE="./out.smt2"
PREAMBLE_FILE="./templates/preamble.smt2"
END_FILE="./templates/end.smt2"

echo $SMT_CODE
cat $PREAMBLE_FILE > $OUTPUT_FILE 
cargo run -q < "$1" >> $OUTPUT_FILE
cat $END_FILE >> $OUTPUT_FILE

z3 -smt2 $OUTPUT_FILE




