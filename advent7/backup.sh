#!/usr/bin/env fish
function findbags 
	echo '\n'
	set bag $argv
	set regex $bag' bags[.,]'
	set contained_bags (cat input7.txt | grep $regex | sed 's/ bags.*//g')
	for contained_bag in $contained_bags
		echo 'bag is '$bag' the one we look at is '$contained_bag
#		cat input7.txt | grep '^'$bag | grep '[0-9] ' #$newbag -o | grep '[0-9]' -o
#		cat input7.txt | grep '^'$regex | grep '[0-9] '$bag -o | grep '[0-9]' -o
#		cat input7.txt | grep $regex | sed 's/ bags.*//g'
#		cat input7.txt |grep '^'$newbag 
#		echo $count
#		set count (cat input7.txt |grep '^'$bag | grep $newbagregex -o | grep '[0-9]' -o)
#		echo $count
#		for i in (seq $count)
		findbags $contained_bag
#		end
	end
end

findbags "shiny gold" 
echo 'Sum is '(findbags "shiny gold" | wc -l)



