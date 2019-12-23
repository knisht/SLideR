# SLideR
Parser generator for SLR(1) grammars. Synthesized attributes and lexer are supported.

## Installation
```
git clone https://github.com/knisht/SLideR.git
cd SLideR
stack install
./SLideR path/to/file/with/grammar.slr
```    
This will append path to executable `SLideR` to your `$PATH`.   
You should provide `regex-pcre >= 0.94` and `mtl >= 2.0` to compile generated file.

## Usage
You may see [here](example/Grammar.slr) an example of file describing generated grammar. Syntax is similar to Happy parser generator, but you can also describe tokenizer in the same file.

## Tips and tricks
You may use monadic computations for simulating inherited attributes behavior.
