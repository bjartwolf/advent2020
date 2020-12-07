#!/usr/bin/env fish
function findbags 
  for bagAndCount in (cat input7.txt | grep '^'$argv' bags\?'| grep -o '[1-9][a-z ]*bag' | sed 's/ bag.*//g')
    for i in (seq (echo $bagAndCount | grep '[1-9]' -o))
      echo $bagAndCount
      findbags (echo $bagAndCount | sed 's/[1-9] //g')
    end
  end
end

echo 'Sum is '(findbags "shiny gold" | wc -l)
