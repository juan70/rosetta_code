# Create the text for rosetta code.
# Copy-paste where appropriate.
# For the output, indent so using <pre> is not necessary
interp='ocaml'
code='fasta.ml'
data='data_task.txt'

cat << EOF
=={{header|OCaml}}==
I keep it simple by sticking to the description of the FASTA format described in the task. 

The program reads and processes the input one line at a time, and directly prints out the chunk of data available. The long strings are not concatenated in memory but just examined and processed as necessary: either printed out as is in the case of part of a sequence, or formatted in the case of the name (what I call the label), and managing the new lines where needed.
{{works with|OCaml|4.03+}}
<lang ocaml>
$(cat ${code})
</lang>
{{out}}
$(${interp} ${code} < ${data} | sed 's/^/ /')
EOF

