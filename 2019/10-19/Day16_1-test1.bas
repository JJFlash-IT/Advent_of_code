Dim aInputSignal(Any, Any) As Integer 
Open Exepath + "\input16_test1.txt" For Input As #1
	Scope
		Dim sInputSignalString As String
		Line Input #1, sInputSignalString 
		ReDim aInputSignal(1 To Len(sInputSignalString), 0 To 1) '2 = One for input, one for output
		For K As Integer = 1 To Ubound(aInputSignal)
			aInputSignal(K, 0) = sInputSignalString[K-1] - Asc("0")
		Next K
	End Scope
Close #1

Type strPattern
	Value As Const Integer
	Repeat As Integer
End Type
'The base pattern is 0, 1, 0, -1
Dim aPattern(0 To 3) As strPattern => {(0, 0), (1, 0), (0, 0), (-1, 0)}
Dim As Integer nSourceSignal, nDestSignal = 1, nPhaseCount, nOutElementIndex, nInElementIndex, nElementResult, nPatternIndex

For nPhaseCount = 1 To 4
	For nOutElementIndex = 1 To Ubound(aInputSignal)
		'INIT: repeat each value in the pattern *a number of times equal to the position* in the *output* list being considered
		For nPatternPos As Integer = 0 To 3
			aPattern(nPatternPos).Repeat = nOutElementIndex
		Next nPatternPos
		'When applying the pattern, skip the very first value exactly once. (This is ONLY for the initialization!)
		aPattern(0).Repeat -= 1
		nElementResult = 0 : nPatternIndex = 0
		For nInElementIndex = 1 To Ubound(aInputSignal)
			If aPattern(nPatternIndex).Repeat = 0 Then
				aPattern(nPatternIndex).Repeat = nOutElementIndex
				nPatternIndex = (nPatternIndex + 1) And 3
			End If
			nElementResult += aInputSignal(nInElementIndex, nSourceSignal) * aPattern(nPatternIndex).Value
			aPattern(nPatternIndex).Repeat -= 1
		Next nInElementIndex
		nElementResult = Abs(nElementResult Mod 10) 'Extract ones digit, get the absolute value
		aInputSignal(nOutElementIndex, nDestSignal) = nElementResult
	Next nOutElementIndex
	Print "After phase " & nPhaseCount & ": " ;
	For K As Integer = 1 To Ubound(aInputSignal)
		Print Str(aInputSignal(K, nDestSignal));
	Next K
	Print
	Swap nSourceSignal, nDestSignal
Next nPhaseCount