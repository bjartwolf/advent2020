#!/usr/bin/env fish
set sum 0
function findbags 
	for bag in $argv
		set regex $bag' bags[.,]'
		set nr (cat input7.txt | grep $regex | wc -l)
		cat input7.txt | grep $regex 
		set sum (math $nr + $sum)
		set indirect_bags (cat input7.txt | grep $regex | sed 's/ bags.*//g')
		if [ -n "$indirect_bags" ]
			echo $indirect_bags
			findbags $indirect_bags
		end
	end
end

findbags "shiny gold" 
echo $sum 
