#!/bin/sh

js=./dist/main.js
min=./dist/main.min.js

rm -Rf ./dist/

cp -r ./public/ ./dist/

elm make src/Main.elm --optimize --output=$js

uglifyjs $js --compress 'pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output $min

echo "Compiled size: $(wc $js -c)"
echo "Minified size: $(wc $min -c)"
echo "Gzipped size:  $(gzip $min -c | wc -c)"

rm -f $js