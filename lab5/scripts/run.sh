
GRAMMAR_FILE=$1
WORD=$2
# OUTPUT_FILE="./out.smt2"
# PREAMBLE_FILE="./templates/preamble.smt2"
# END_FILE="./templates/end.smt2"

cargo run -- --word "$2" < "$1"
