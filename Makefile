RESULT = reversi
SOURCES = color.ml command.ml commandParser.mly commandLexer.mll const.ml board.ml eval.ml search.ml book.ml new.ml play.ml main.ml
LIBS = unix

# all: byte-code
all: native-code 

-include OCamlMakefile