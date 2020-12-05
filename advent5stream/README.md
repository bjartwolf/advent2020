


```
 dotnet build
 cat input5.txt | bin/Debug/net5.0/advent5stream | sort -rn | head -n 1
 cat input5.txt | bin/Debug/net5.0/advent5stream | sort -n | awk '{for(i=p+1; i<$1; i++) print i} {p=$1}'

```
