gcc src/interp.c src/parse.c src/lisp.c -O2 -ldl -rdynamic -fopenmp -o bin/freelisp -flto -march=native -Wall -Wextra -Wno-missing-field-initializers
gcc src/prelude.c -O2 -flto -fno-partial-inlining -fPIC --shared -fopenmp -o bin/prelude.so -Wall -Wextra -Wno-missing-field-initializers -lreadline -lncurses
