#!/usr/bin/fish
cat input6.txt | awk 'BEGIN{RS="\n\n"}; { gsub("\n", "", $0) ;printf("%s %s", $0, "\n") }' \
   	       | sed ':1;s/\(.\)\(.*\)\1/\1\2/;t1'\
	       | awk 'BEGIN{RS="\n"}; { sub("\n", "", $0); sub(" ", "") ;printf("%s", $0) }' \
	       | wc -c
