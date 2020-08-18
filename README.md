# phll
compressed but unfiltered RGBA PNGs in Haskell

## usage
```
Phll.png_rgba :: [[(Word8, Word8, Word8, Word8)]] -> [Word8]
```
example: [./example.hs](https://github.com/hywn/phll/blob/master/example.hs)

## background/notes
- I basically copied the [Haskell wiki](https://web.archive.org/web/20200818184851/https://wiki.haskell.org/Library/PNG) in a learning experience but not exactly
- used more `[Word8]`s than `ByteString`s not sure if this is super inefficient or something