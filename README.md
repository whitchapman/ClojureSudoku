# sudoku

A Clojure library designed to solve sudoku puzzles.


## Setup in Emacs

I use Emacs Live with the following steps to prepare this project for hacking in the REPL:

1. M-x cider-jack-in
2. C-x o (switch to non-REPL buffer)
3. C-x C-f src/sudoku/core.clj (open code file)
4. C-c C-k (compile src)
5. C-c M-n (change REPL namespace to sudoku.core)
6. switch to REPL buffer and start hacking...


## Usage

In this namespace, "data" is a blank sudoku data structure. To see a visual represenation of this data structure, write html to a file and open in any browser, like so:

```clojure
(write_data "/absolute/path/works/best/data.html" data)
```

The image at this link shows that every cell can have the possible values [1-9]: [Blank Puzzle Data Structure](images/puzzle1/data.png)


## Example Puzzle

Given this puzzle: [Puzzle #1](images/puzzle1/puzzle1.png)

Load the encoded puzzle1 into a data structure, and then write html to a file to view in the browser:

```clojure
(def data2 (assign_values data puzzle1))
(write_data "/absolute/path/works/best/data.html" data2)
```

[Loaded Puzzle Data Structure](images/puzzle1/data2.png)

The next step simplifies the puzzle by removing as many values as possible from the cells associated with any naked singles (this can take a couple of iterations through all the groups):

```clojure
(def data3 (simplify_data data3))
(write_data "/absolute/path/works/best/data.html" data3)
```

[Simplified Puzzle Data Structure](images/puzzle1/data3.png)

At this point, additional algorithms like Hidden Singles, Doubles, Triples, etc..., are needed to continue simplifying the problem space.


## License

This project is a POC demonstrating what can be done with Clojure.
Please contact me if you have any comments or suggestions.

Copyright © 2015 Whit Chapman
