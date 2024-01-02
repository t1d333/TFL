
GRAMMAR_FILE=$1
WORD=$2

cargo run -- --word "$2" < "$1"
