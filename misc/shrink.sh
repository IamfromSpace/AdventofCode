set -e

# Simple script to shrink down screen captures based on known good settings

# Convert
CRF=36
PRESET=veryslow
CODEC=libx265
NAME=${1%.*}
EXT=${1##*.}
OUTPUT="${NAME}_${CODEC}_${CRF}_${PRESET}.${EXT}"
ffmpeg -i $1 -c:v $CODEC -crf $CRF -preset $PRESET -c:a copy $OUTPUT

echo "Conversion complete, checking result." 1>&2

# Check Result
TEMP_FILE=$(mktemp)
ffmpeg -i $1 -i $OUTPUT -lavfi  "ssim=$TEMP_FILE" -f null -
COUNT=0
SUM="0"
# TODO: This is hilariously slow
while read line
do
  N=$(echo $line | sed -E 's/.*All:([^ ]*).*/\1/');
  COUNT=$(calc "$COUNT+1");
  SUM=$(calc "$N + $SUM");
done < $TEMP_FILE
IS_GOOD=$(calc "$SUM * $COUNT > 0.99 * $COUNT")

# Clean-up old file
if [[ $IS_GOOD -eq 1 ]]; then
  echo "Check successful." 1>&2
  rm $1
else
  MATCH_PERCENT=$(calc "$SUM / $COUNT")
  echo "Error in conversion, file is not a close enough match: $MATCH_PERCENT" 1>&2
  exit 1
fi
