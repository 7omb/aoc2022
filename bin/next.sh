#!/usr/bin/env sh

# Create directory for next day from project root

NEXT=$(find . -name 'day*' -type d | tail -n 1 | sed 's|\./day||' | xargs expr 1 + | xargs printf "day%02d")
mkdir "$NEXT"

printf "Name of main function: "
read -r MAIN
sed "s/zzzzz/$MAIN/" Main.hs > "$NEXT/Main.hs"

echo "Created $NEXT"
