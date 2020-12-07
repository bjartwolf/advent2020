#!/usr/bin/env fish
function findbags 
#  set bags (cat foo.txt | grep $argv' bags\?'| sed 's/ bags.*//g')
  set bagsInbag (cat input7.txt | grep '^'$argv' bags\?'| grep -o '[1-9][a-z ]*bag' | sed 's/ bag.*//g')
  for bagAndCount in $bagsInbag
    set nrOfBags (echo $bagAndCount | grep '[1-9]' -o)
    set bag (echo $bagAndCount | sed 's/[1-9] //g')
#    echo 'nrofbags: '$nrOfBags
#    echo 'bag: '$bag
    for i in (seq $nrOfBags)
      echo $i$bag
      findbags $bag
    end
  end
end

#echo 'Sum is '(findbags "shiny gold" | sort | uniq | wc -l)
findbags "shiny gold"
