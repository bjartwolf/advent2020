#!/usr/bin/env fish
function findbags 
#  set bags (cat foo.txt | grep $argv' bags\?'| sed 's/ bags.*//g')
  echo 'called with '$argv'x'
  set bagsInbag (cat foo.txt | grep '^'$argv' bags\?'| grep -o '[1-9][a-z ]*bag' | sed 's/ bag.*//g')
  for bagAndCount in $bagsInbag
    set nrOfBags (echo $bagAndCount | grep '[1-9]' -o)
    set bag (echo $bagAndCount | sed 's/[1-9] //g')
    echo 'nrofbags: '$nrOfBags
    echo 'bag: '$bag
    findbags $bag
  end
end

#echo 'Sum is '(findbags "shiny gold" | sort | uniq | wc -l)
findbags "shiny gold"
