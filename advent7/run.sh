#!/usr/bin/env fish
function findbags 
	set bag $argv
	set regex $bag' bags\?[.,]'
	set contained_bags (cat input7.txt | grep $regex | sed 's/ bags.*//g')
	for contained_bag in $contained_bags
		echo $contained_bag
		findbags $contained_bag
	end
end

echo 'Sum is '(findbags "shiny gold" | sort | uniq | wc -l)



