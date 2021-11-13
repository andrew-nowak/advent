#!/bin/bash

set -x

if [[ -z "$1" ]]; then
  echo "Usage: [year] <day>"
  exit 1
fi

if [[ -z "$2" ]]; then
  YEAR=nineteen
  DAY="$1"
else
  YEAR="$1"
  DAY="$2"
fi

if ! grep -q "object $YEAR " build.sc; then
  echo "First, set up module for year $YEAR"
  exit 2
fi

LOC="$YEAR"/"$DAY"/src/"$YEAR"/"$DAY"

mkdir -p "$LOC"
> "$LOC"/Main.scala cat <<EOF
package $YEAR.$DAY

import lib.Support

object Main extends App with Support {
  println(load)
}
EOF

mkdir -p "$YEAR"/"$DAY"/resources
> "$YEAR"/"$DAY"/resources/input.txt cat <<EOF
hello world
EOF

awk -i inplace -v x="  object $DAY extends ScalaModule with Common" "/object[[:space:]]+$YEAR"'([[:space:]]|[[:alnum:]])+\{/{f=1} !/}/{print} /}/{if (f) print x; print "}"; f=0}' build.sc

./gen

mill "$YEAR.$DAY.run"