# Hirschberg

I implemented Hirschberg's Algorithm, a dynamic programming algorithm that
extends the functionality of other optimal sequence alignment algorithms by reducing
space complexity from O(mn) to O(min{n,m}). Hirschberg's Algorithm may be best
viewed as an extension to standard Alignment algorithms (e.g. Needleman–Wunsch
and Smith–Waterman) which reduces the space complexity to linear. This is
accomplished by using a divide and conquer approach to the dynamic programming
matrix, where only the current and previous row of the dynamic programming table are
needed to be kept in memory. The algorithm takes advantage of the fact that a cost (or
score) may be calculated accurately on the reverse of a string. Therefore, for some
arbitrary partition (e.g. the halfway point) for a string A, there exists an optimal partition
in string B such that the optimal alignment of A and B is the optimal alignment of A_{left}
and B_{left} combined with the optimal alignment of A_{right} and B_{right} where "left"
and "right" signify the resulting sub-strings from the making of partitions. This is run
recursively to build an optimal solution from optimal sub-solutions. I implemented
Hirschberg's Algorithm in Haskell, a purely functional programming language. As far as I
can tell, this is the first time ever that this algorithm has been implemented in Haskell.
The results of running 10 runs at each data point for string size n = [100, 200, 400, 800,
1000] came out as expected. The time taken grew quadratically with n while the space
taken stayed linear. This trajectory follows what is expected to occur with Hirschberg's
Algorithm, as no time optimizations are made to the standard alignment algorithms. The
algorithm still requires n*m steps to be taken to calculate the score/cost of each cell in
the dynamic programming table, where n and m are the length of the strings.
