#!/usr/bin/env fish
# can hold directly

set sum (math (cat input7.txt | grep 'shiny gold bags[,.]'  | wc -l))
echo "bags that can hold directly " $sum
cat input7.txt | grep 'shiny gold bags[,.]' 

# bags that can hold directly
echo "bags that can hold in indirectly in one step"
function findbags 
	set bags $argv
	for bag in $indirect_bags;
		echo "****"
		echo "bag : " $bag
		set regex $bag'bags[,.]' 
		cat input7.txt | grep $regex 
		set nr (cat input7.txt | grep $regex | wc -l)
		set sum (math $nr + $sum) 

		# continue in the tree
		# set indirect_bags (cat input7.txt | grep 'shiny gold bags[,.]' | sed 's/bags.*//g')
	end
end

set indirect_bags (cat input7.txt | grep 'shiny gold bags[,.]' | sed 's/bags.*//g')
findbags $indirect_bags
echo $sum
