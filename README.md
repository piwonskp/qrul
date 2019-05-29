# qrul
Simple language compiler using llvm-hs & megaparsec.

# Usage
Run commands:
```
docker-compose run --rm app /bin/bash
qrul test.txt
lli-8 test.ll
echo $?
```

# Test example
```
Int as
Int n
Float b

as 6
n as
b 2.3
n
```
