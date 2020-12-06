#!/usr/bin/fish
cat input6.txt | awk 'BEGIN{RS="\n\n"}; { gsub("\n", "-", $0) ;printf("%s %s", $0, "\n") }' > input6-reformat.txt
