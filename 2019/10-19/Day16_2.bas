'BIG HINTS from Reddit:
'https://www.reddit.com/r/adventofcode/comments/ebd3o3/day_16_part_2_help/fb3vbci/
'https://www.reddit.com/r/adventofcode/comments/ebq8kq/day_16_part_2_solution_too_slow_even_after/fb6n0gg/

Dim As Integer aInputSignal(Any), nInputLen, a8digitMessage(1 To 8), nMessageOffset
Open Exepath + "\input16_test2Part2.txt" For Input As #1
	Scope
		Dim sInputSignalString As String
		Line Input #1, sInputSignalString
		Print """" & sInputSignalString & """"
		nInputLen = Len(sInputSignalString)
		'The first seven digits of your initial input signal also represent the message offset
		nMessageOffset = Valint(Left(sInputSignalString, 7))
		ReDim aInputSignal(0 To nInputLen - 1)
		For K As Integer = 0 To nInputLen - 1
			aInputSignal(K) = sInputSignalString[K] - Asc("0")
		Next K
	End Scope
Close #1
Print nMessageOffset
Print nInputLen * 10000
Print
Print nMessageOffset Mod nInputLen
nMessageOffset Mod= nInputLen
Print nMessageOffset

For nInputScan As Integer = 1 To 8
'	a8digitMessage(nInputScan) = aInputSignal(nMessageOffset)
'	Print "" & nMessageOffset & ": " & Str(a8digitMessage(nInputScan))
	Print "" & nMessageOffset & ": " & Str(aInputSignal(nMessageOffset))
	nMessageOffset = (nMessageOffset + 1) Mod nInputLen
Next nInputScan
Print

'Dim As Integer nPhaseCount

'For nPhaseCount = 1 To 100

'Next nPhaseCount