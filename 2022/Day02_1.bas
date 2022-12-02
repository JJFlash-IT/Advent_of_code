'A for Rock, B for Paper, and C for Scissors   (opponent)
'X for Rock, Y for Paper, and Z for Scissors   (you)
'X defeats C, Z defeats B, and Y defeats A

'score for the shape you selected (1 for Rock, 2 for Paper, and 3 for Scissors)
'PLUS the score for the outcome of the round (0 if you lost, 3 if the round was a draw, and 6 if you won)
Dim lScoreTable(3, 3) as LONG
lScoreTable(0, 0) = 3  'opponent: rock ; you: rock -> draw
lScoreTable(0, 1) = 6  'opponent: rock ; you: paper -> win
lScoreTable(0, 2) = 0  'opponent: rock ; you: scissors -> lose
lScoreTable(1, 0) = 0  'opponent: paper ; you: rock -> lose
lScoreTable(1, 1) = 3  'opponent: paper ; you: paper -> draw
lScoreTable(1, 2) = 6  'opponent: paper ; you: scissors -> win
lScoreTable(2, 0) = 6  'opponent: scissors ; you: rock -> win
lScoreTable(2, 1) = 0  'opponent: scissors ; you: paper -> lose
lScoreTable(2, 2) = 3  'opponent: scissors ; you: scissors -> draw 

Dim sBuffer as STRING * 3
Dim bOpponentMove as BYTE
Dim bPlayerMove as BYTE
Dim lTotalScore as LONG
lTotalScore = 0

open 2,8,2,"input02.txt,p,r"
    do
        input #2, sBuffer
        bOpponentMove = peek(@sBuffer + 1) - asc("a")
        bPlayerMove = peek(@sBuffer + 3) - asc("x")
        lTotalScore = lTotalScore + ( (bPlayerMove + 1) + lScoreTable(bOpponentMove, bPlayerMove) )
    loop until st() = 64 'EOF
close 2
print "total score: " ; lTotalScore
