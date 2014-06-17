#!/usr/bin/env sh
set -e

GEN="./$1"
ICO=favicon.ico

if [ $# -ne 1 ]
then
    echo 'usage: mkicon favicon.hs'
    exit 1;
fi

TMP_PREF=$(basename $0 .sh)
SVG=$(mktemp --tmpdir $TMP_PREF.XXXX --suffix=.svg)
trap "rm $SVG" EXIT

echo "Rendering svg..."
$GEN -o $SVG -w 64

echo "Converting to png..."
PNG=${SVG%.*}.png
convert -background transparent +antialias $SVG $PNG
trap "rm $PNG" EXIT

echo "Assembling ico..."
convert $PNG $ICO
#    \( -clone 0 -resize 16x16 \) \
#    \( -clone 0 -resize 24x24 \) \
#    \( -clone 0 -resize 32x32 \) \
#    \( -clone 0 -resize 48x48 \) \
#    \( -clone 0 -resize 64x64 \) \
#    -delete 0 -background transparent -colors 256 $OUT_ICO


echo "Favicon updated."