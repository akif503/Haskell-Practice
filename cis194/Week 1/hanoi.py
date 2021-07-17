from math import pow, sqrt

def hanoi(n, cur, support, to):
    move = (cur, to)
    if n == 1:
        return [move]

    return hanoi(n-1, cur, to, support) + [move] + hanoi(n-1, support, cur, to)


# print(len(hanoi(15, 'a', 'b', 'c')))

# S1 -> (n-1)th
# S2 -> (n-2)
def hanoiPlus(n, cur, s1, s2, to):
    move = (cur, to)

    if n == 0:
        return []
    if n == 1:
        return [move]

    else:
        m1 = hanoiPlus(n-2, cur, s2, to, s1)
        m2 = (cur, s2)
        m3 = move
        m4 = (s2, to)
        m5 = hanoiPlus(n-2, s1, cur, s2, to)

        return m1 + [m2, m3, m4] + m5


# print(len(hanoiPlus(int(input()), 'a', 'b', 'c', 'd')))


# Although the 3 pegged version has a simple recursive solution, the board with 4 or more pegs does not. 
# The Frame-Stewart Algorithm is conjectured (not proved) to be optimal for 4 or more pegs 

# Frame-Stewart Algorithm:
# n -> disks
# r -> pegs
# T(n,r) -> min no. of moves required to transfer n disks using r pegs

# The recursive Algo:
# 1. For k; 1 <= k < n, transfer the k disks to a single peg other than the start or dest pegs, taking T(k,r) moves
# 2. Transfer the remaining disks, to the destination peg ; T (n-k, r-1)
# 3. Transfer the previously seperated

# Total Moves = 2T(k,r) + T(n-k,r-1)
# k = n - rnd(sqrt(2n+1)) + 1

# Series with pegs 4: https://oeis.org/A007664
valueOfk = lambda x: x - round(sqrt(2*x+1)) + 1

def fsa1(n, cur, s1, s2, to):
    if n == 0:
        return []
    if n == 1:
        return [(cur, to)]

    else:
        k = valueOfk(n)
        m1 = fsa1(k, cur, s2, to, s1)
        m2 = fsa1(n-k, cur, s1, s2, to)
        m3 = fsa1(k, s1, cur, s2, to)

        return m1 + m2 + m3

def fsa(n, r):
    if n == 1:
        return 1
    
    if r == 3:
        return pow(2,n) - 1

    else: 
        k = valueOfk(n)

        return 2 * fsa(k, r-2) + fsa(n-k, r-3)


print(fsa(int(input()), 4))
