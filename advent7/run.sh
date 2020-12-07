#!/usr/bin/env fish
function findbags 
  for bag in (cat input7.txt | grep $argv' bags\?[.,]'| sed 's/ bags.*//g')
    echo $bag
    findbags $bag
  end
end

echo 'Sum is '(findbags "shiny gold" | sort | uniq | wc -l)



