#!/bin/bash


for f in *.raw
do
   res=${f%.raw}.png
   convert -depth 8 -size 32x32+0 rgb:$f -scale 512x512 $res 
done
