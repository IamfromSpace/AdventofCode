# Simple script to shrink down screen captures based on known good settings
CRF=36
PRESET=veryslow
CODEC=libx265
NAME=${1%.*}
EXT=${1##*.}
ffmpeg -i $1 -c:v $CODEC -crf $CRF -preset $PRESET -c:a copy "${NAME}_${CODEC}_${CRF}_${PRESET}.${EXT}"
