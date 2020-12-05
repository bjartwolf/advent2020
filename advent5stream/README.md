
The idea here is to write a unix-pipeline tool that reads in the FLBRGL-codes from a textfile and uses only bitmanipulations and things without
actually looking at the characters, only looking at the char-codes. I use the fact that R and B has a set bit in position 3 (0b_0000_0100) and 
then just moves things according too that and the position in the buffer... It only works on unix because I only look for charcode 10 (\n)

```
 dotnet build
 cat input5.txt | bin/Debug/net5.0/advent5stream | sort -rn | head -n 1
 cat input5.txt | bin/Debug/net5.0/advent5stream | sort -n | awk '{for(i=p+1; i<$1; i++) print i} {p=$1}' | sort -rn | head -n 1
```
