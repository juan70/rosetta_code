# Create the text for rosetta code.
# Copy-paste where appropriate.
# For the output, indent so using <pre> is not necessary
interp='ocaml'
code='seconds.ml'

cat << EOF
=={{header|OCaml}}==
{{works with|OCaml|4.03+}}
<lang ocaml> $(cat ${code}) </lang>
{{out}}
$(${interp} ${code} | sed 's/^/ /')
EOF

