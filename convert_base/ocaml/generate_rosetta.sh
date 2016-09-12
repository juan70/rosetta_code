# Create the text for rosetta code.
# Copy-paste where appropriate.
# For the output, indent so using <pre> is not necessary
interp='ocaml'
code='convert.ml'

cat << EOF
=={{header|OCaml}}==
Well, I am no expert in OCaml, and my code may seem a bit messy, but I actually
took a rather naive aproach... Anyway, the program seems to work, but the
algorithm(s) can probably be improved.
After reading the discussion, I took into account the suggestion that the
program should perform conversions from any base to any other base.
{{works with|OCaml|4.03+}}
<lang ocaml>
$(cat ${code})
</lang>
{{out}}
$(${interp} ${code} | sed 's/^/ /')
EOF

