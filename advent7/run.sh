#!/usr/bin/env fish
function findbags 
  for contained_bag in (cat input7.txt | grep $argv' bags\?[.,]'| sed 's/ bags.*//g')
    echo $contained_bag
    findbags $contained_bag
  end
end

echo 'Sum is '(findbags "shiny gold" | sort | uniq | wc -l)



