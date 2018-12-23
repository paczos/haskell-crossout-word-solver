## works here:
```
docker run -v `pwd`:/puzzle_solver/ -it -w /puzzle_solver/ --rm  haskell /bin/bash
```
## compilation
```
ghc -o prog prog.hs
```
## run
```
./prog puzzle_1_board.txt puzzle_1_words.txt
```