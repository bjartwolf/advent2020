
The idea here is to write a unix-pipeline tool that reads in the FLBRGL-codes from a textfile and uses only bitmanipulations and things without
actually looking at the characters, only looking at the char-codes. I use the fact that R (82, 0101_0010) and B (66, 0100_0010) does not have a set bit in position 3 (0b_0000_0100) and F (70, 0100_0110) and L (76, 0100_1100) has a bit in that position. So by first masking that bit with AND and then XORing i get a bit set if it is R or B. Then I shift that so that it becomes a 1 and then put it in the right position by bitshifting it again. THis could surely be optimized, but it is the core idea of not branching but just doing bit operations to find the right value. It only works on unix because I only look for charcode 10 (\n)

```
 dotnet build
 cat input5.txt | bin/Debug/net5.0/advent5stream | sort -rn | head -n 1
 cat input5.txt | bin/Debug/net5.0/advent5stream | sort -n | awk '{for(i=p+1; i<$1; i++) print i} {p=$1}' | sort -rn | head -n 1
```

![bitshift doc](https://user-images.githubusercontent.com/1174441/101262463-3c5b0f00-373f-11eb-9ece-42f9ee7a1a66.png)
