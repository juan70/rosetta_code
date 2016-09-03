# Create the text for rosetta code.
# Copy-paste where appropriate.
interp='ocaml'
code='rodec.ml'

cat << EOF
=== Another implementation ===
Another implementation, a bit more OCaml-esque: no mutable variables, and a recursive function instead of a for loop.
{{works with|OCaml|4.03+}}
<lang ocaml>
$(cat ${code})
</lang>
Output:
<pre>
$(${interp} ${code})
</pre>
EOF

