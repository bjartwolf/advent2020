#!/usr/bin/env fish
# can hold directly

set sum (math (cat input7.txt | grep 'shiny gold bags[,.]'  | wc -l))
echo "bags that can hold directly " $sum

cat input7.txt | grep 'shiny gold bags[,.]' 

# bags that can hold directly
echo "bags that can hold in indirectly in one step"
set indirect_bags (cat input7.txt | grep 'shiny gold bags[,.]' | sed 's/bags.*//g')
for bag in $indirect_bags;
	echo "bag : " $bag
	set regex $bag'bags[,.]' 
	cat input7.txt | grep $regex 
	set nr (cat input7.txt | grep $regex | wc -l)
	set sum (math $nr + $sum) 
end

echo $sum
