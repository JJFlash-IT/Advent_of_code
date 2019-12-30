'BIG HINTS from Reddit:
'https://www.reddit.com/r/adventofcode/comments/ebd3o3/day_16_part_2_help/fb3vbci/
'https://www.reddit.com/r/adventofcode/comments/ebq8kq/day_16_part_2_solution_too_slow_even_after/fb6n0gg/
'C concise code: https://github.com/ednl/aoc2019/blob/master/16/16b.c

Dim As Integer nInputLen, aOffsetInput(Any), nOffsetArrSize, nMessageOffset
Dim sInputSignalString As String
Open Exepath + "\input16.txt" For Input As #1
	Line Input #1, sInputSignalString	
Close #1
nMessageOffset = Valint(Left(sInputSignalString, 7)) 'The first seven digits of your initial input signal also represent the message offset
nInputLen = Len(sInputSignalString)
nOffsetArrSize = (nInputLen * 10000) - nMessageOffset
ReDim aOffsetInput(1 To nOffsetArrSize)
nMessageOffset Mod= nInputLen 'This "adapts" the offset from 1-based indexing to 0-based indexing
For nInputScan As Integer = 1 To nOffsetArrSize
	aOffsetInput(nInputScan) = sInputSignalString[nMessageOffset] - Asc("0")
	nMessageOffset = (nMessageOffset + 1) Mod nInputLen
Next nInputScan

'you have OFFSET zeros at the start, meaning for the digit at position OFFSET
'you don't have to care about preceding numbers (they will add up to 0), so you can start computing at the OFFSET position;
'another big optimization help comes from just how large the offset is:
'more than half the digits, meaning that at position OFFSET you only have leading 0s and then 1s.
'Last element is constant, because it predecessors all have 0 multiplier,
'then second to last element is a sum of the last and itself, then third to last is a sum of second to last and itself, and so on.
For nPhaseCount As Integer = 1 To 100
	For nInputSum As Integer = nOffsetArrSize - 1 To 1 Step -1
		aOffsetInput(nInputSum) = (aOffsetInput(nInputSum) + aOffsetInput(nInputSum + 1)) Mod 10 'The last digit of the sum is taken
	Next nInputSum
Next nPhaseCount

Print "The eight-digit message IS: " ; : Color 15
For nMessageScan As Integer = 1 To 8
	Print Str(aOffsetInput(nMessageScan));
Next nMessageScan 
Print : Color 7
