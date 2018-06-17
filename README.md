# clojure-sudoku-solver

A Clojure library designed to solve sudoku puzzles.


## Setup

Start a REPL, compile `sudoku.core`, and then switch to that namespace:
```clojure
> (ns sudoku.core)
```

## Usage

To see a visual representation of a blank sudoku data structure, initialize the data structure and write html to a file, per code below, and then open the generated file in any browser:

```clojure
> (def data (initialize-data))
> (write-html "/absolute/path/works/best/data.html" data)

$ open data.html
```

The image at this link shows that every cell can have the possible values [1-9]:

[Blank Puzzle Data Structure](images/puzzle1/data.png)


## Example Puzzle

Given this puzzle: [Puzzle #1](images/puzzle1/puzzle1.png)

Load the encoded puzzle1 into a data structure, and then write html to a file to view in a browser:

```clojure
> (def data2 (assign-values data puzzle1))
> (write-html "/absolute/path/works/best/data.html" data2)
```

[Loaded Puzzle Data Structure](images/puzzle1/data2.png)


Solving the puzzle involves incrementally simplifying the data structure using increasingly complex algorithms dependent on puzzle difficulty.

This next step simplifies the puzzle by iterating over all groups and applying the intra-group algorithm.

The first iteration:

```clojure
> (def data3 (solve-puzzle puzzle1 :max-iterations 1))
> (write-html "/absolute/path/works/best/data.html" data3)
```

[Simplified Puzzle Data Structure (1 Iteration)](images/simplify/data1.png)


An iteration of the algorithm attempts to simplify each group in turn by creating sets within the group that contain exclusive values. The simplest case is a naked single: a solved cell results in removing that cell's value from every other cell in the group. The algorithm continues by attempting to create exclusive value sets up to a size of one less than the number of unsolved cells in the group (e.g. max size 8 for 9 unsolved cells).

The second iteration:

```clojure
> (def data4 (solve-puzzle puzzle1 :max-iterations 2))
> (write-html "/absolute/path/works/best/data.html" data4)
```

[Simplified Puzzle Data Structure (2 Iterations)](images/simplify/data2.png)


After the 3rd iteration, the puzzle is solved:

```clojure
> (def data5 (solve-puzzle puzzle1))
> (write-html "/absolute/path/works/best/data.html" data5)
> (data-is-solved data5)
```

[Solved Puzzle](images/simplify/data3.png)


The intra-group algorithm addresses all of these possible occurrences (previously addressed in separate algorithms):
* Naked Singles
* Naked Doubles
* Naked Triples
* Naked Quadruples
* Hidden Singles
* Hidden Doubles
* Hidden Triples
* Hidden Quadruples


In the case of harder puzzles, inter-group algorithms are needed to continue simplifying the problem space:
* Locked Candidates
* X-Wing
* XY-Wing (TODO)
* Swordfish (TODO)


## License

This project is a POC demonstrating what can be done with Clojure.
Please contact me if you have any comments or suggestions.

Copyright © 2018 Whit Chapman
