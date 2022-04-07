#!/usr/bin/env bash

cabal build
cabal run > out.html
cp out.html in.html

sed -i '1,3d' in.html
sed -i 's/&amp;/\&/g' in.html
sed -i 's/&lt;/</g' in.html
sed -i 's/&gt;/>/g' in.html
sed -i 's/&quot;/"/g' in.html
docker run --rm -v "`pwd`":/data michaelperrin/prince:latest -o /data/out.pdf /data/in.html
rm in.html
