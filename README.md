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

In this namespace, "data" is a blank sudoku data structure. To see a visual represenation of this data structure, write html to a file, per code below, and then open the resultant file in any browser:

```clojure
(write-data "/absolute/path/works/best/data.html" data)
```

The image at this link shows that every cell can have the possible values [1-9]:

[Blank Puzzle Data Structure](images/puzzle1/data.png)


## Example Puzzle

Given this puzzle: [Puzzle #1](images/puzzle1/puzzle1.png)

Load the encoded puzzle1 into a data structure, and then write html to a file to view in a browser:

```clojure
(def data2 (assign-values data puzzle1))
(write-data "/absolute/path/works/best/data.html" data2)
```

[Loaded Puzzle Data Structure](images/puzzle1/data2.png)

Solving the puzzle involves incrementally simplifying the data structure using increasingly complex algorithms dependent on puzzle difficulty.

The next step simplifies the puzzle by removing as many values as possible from the cells associated with any naked singles. For this puzzle, 2 iterations of the Naked Singles algorithm reduces the data to the point where a more complex algorithm is needed:

```clojure
(def data3 (simplify-data 2 data2))
(write-data "/absolute/path/works/best/data.html" data3)
```

[Simplified Puzzle Data Structure (2 Iterations)](images/puzzle1/data3.png)


The puzzle state after 2 iterations of Naked Singles necessitates the Hidden Singles algorithm:

```clojure
(def data4 (simplify-data 1 data3))
(write-data "/absolute/path/works/best/data.html" data4)
```

[Simplified Puzzle Data Structure (3 Iterations)](images/puzzle1/data4.png)


After running the Hidden Singles algorithm on the 3rd iteration, the remainder of the puzzle is solved using sucessive Naked Singles iterations:

```clojure
(def data5 (solve-puzzle puzzle1))
(write-data "/absolute/path/works/best/data.html" data5)
(data-is-solved data5)
```

[Solved Puzzle](images/puzzle1/data5.png)


In the case of harder puzzles, additional inter and intra-group algorithms are needed to continue simplifying the problem space.

Inter-Group Algorithms to be encoded:
* Naked Doubles
* Hidden Doubles
* Naked Triples
* Hidden Triples
* Naked Quadruples
* Hidden Quadruples

Intra-Group Algorithms to be encoded:
* X-Wing
* Y-Wing
* ...


## License

This project is a POC demonstrating what can be done with Clojure.
Please contact me if you have any comments or suggestions.

Copyright © 2015 Whit Chapman
