# Huffman
-- Author WRN 2020

huffman codes, with heavy usage of Data.Map, which is steadily becoming my
favorite library ever.

stack init

stack build

usage: stack run encrypt source destination
       stack run decrypt source destination

source /= destination, because the thing is lazy evaluated, and starts writing
before its done reading. you could do some renaming stuff if thats your thing

I have supplied my favorite book: the sorrows of young Werther, as a good test
for the alg.

It saves about 40% space, takes less than 1/5 seconds to process, and is exactly
the same after decryption


(I originally made everything in 1 file, proHoffEncode.hs. I left that file
  here too, in case its easier to follow):

stack exec -- ghc -O2 proHoffEncode.hs

./proHoffEncode encrypt .... ....

(which somehow also runs a lot faster)

-----


stack run encrypt testFile.txt output.txt

stack run decrypt output.txt comparable.txt


diff testFile.txt comparable.txt

(should give no differences)
