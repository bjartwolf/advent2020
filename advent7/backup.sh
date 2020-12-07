#!/usr/bin/env fish
function findbags 
	set bag $argv
	set regex $bag' bags[.,]'
	set indirect_bags (cat input7.txt | grep $regex | sed 's/ bags.*//g')
	if [ -n "$indirect_bags" ]
		for newbag in $indirect_bags
			set regex $bag' bags[.,]'
			set count (cat input7.txt | grep '[0-9] '$bag -o | grep '[0-9]' -o)
#			echo (cat input7.txt | grep $regex) 
			for i in $count
				echo 'Bag is at: '$bag
				findbags $newbag
			end
		end
	else 
		# do something if not
	end
end

findbags "shiny gold" 
echo 'Sum is '(findbags "shiny gold" | wc -l)



