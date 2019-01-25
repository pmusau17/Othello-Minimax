# Othello-Minimax

The basis of this project was to write code for a pair of othello agents to play against each other. The functions that returned the next move were written for us. Our job was is to write the helpeer functions for minimax and minimax with alpha beta pruning. The main code for this project is stored in the ``strategies.rkt`` file.

You can visualize the minimax strategy run: 

``racket main.rkt -l <path to layout in layouts dir> -w <white player’s strategy> -b <black player’s strategy> -t <length of
time in seconds you want each move to take>``

As an example: racket main.rkt -l layouts/basic.lay -w maximize-difference -b  maximize-weight -t .5

**Available Player Strategies:** 
1. random-strategy
2. maximize-difference
3. maximize-weight
4. minimax-searcher-1-weighted-squares
5. minimax-searcher-1-count-difference
6. minimax-searcher-2-weighted-squares
7. minimax-searcher-2-count-difference
8. minimax-searcher-3-weighted-squares
9. minimax-searcher-3-count-difference
10. minimax-searcher-4-weighted-squares
11. minimax-searcher-4-count-difference
12. minimax-searcher-5-weighted-squares
13. minimax-searcher-5-count-difference
14. alpha-beta-searcher-1-weighted-squares
15. alpha-beta-searcher-1-count-difference
16. alpha-beta-searcher-2-weighted-squares
17. alpha-beta-searcher-2-count-difference
18. alpha-beta-searcher-3-weighted-squares
19. alpha-beta-searcher-3-count-difference
20. alpha-beta-searcher-4-weighted-squares
21. alpha-beta-searcher-4-count-difference
22. alpha-beta-searcher-5-weighted-squares
23. alpha-beta-searcher-5-count-difference

To observe a set of strategies played in a round robin, use the s flag to name each strategy (if
there is at least 1 s flag, the code will run a round robin):

``racket main.rkt -l <path to layout in layouts dir> -s <strategy 1> -s <strategy 2> -s <strategy 3>``

For example: 

``racket main.rkt -l layouts/basic.lay -s maximize-difference -s maximize-weight -s minimax-searcher-2-weighted-squares -s minimax-searcher-2-count-difference -s alpha-beta-searcher-2-weighted-squares -s alpha-beta-searcher-2-count-difference``
