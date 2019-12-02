Dim As Integer aProgramIntCode(Any), nProgramCounter, nPosition1, nPosition2, nPosition3, nTempVar, nNoun, nVerb

Open Exepath + "\input02.txt" For Input As #1
	Do
		Input #1, nTempVar
		Redim Preserve aProgramIntCode(nProgramCounter)
		aProgramIntCode(nProgramCounter) = nTempVar
		If Eof(1) Then Exit Do
		nProgramCounter += 1
	Loop
Close #1
Dim nUpperBound As Const Integer = nProgramCounter
Dim As Integer aProgramWorkingCopy(nUpperBound)

'Each of the two input values will be between 0 and 99, inclusive.
For nNoun = 0 To 99
	For nVerb = 0 To 99
		'reset the computer's memory to the values in the program
		For nIndex As Integer = 0 To nUpperBound
			aProgramWorkingCopy(nIndex) = aProgramIntCode(nIndex)
		Next nIndex
		aProgramWorkingCopy(1) = nNoun : aProgramWorkingCopy(2) = nVerb
		nProgramCounter = 0

		Do
			If nProgramCounter > nUpperBound Then Print "?OUT OF PROGRAM  ERROR" : Print "nProgramCounter: " & nProgramCounter : System
			Select Case As Const aProgramWorkingCopy(nProgramCounter)
				Case 1, 2 'Add, Multiply
					nPosition1 = aProgramWorkingCopy(nProgramCounter + 1)
					nPosition2 = aProgramWorkingCopy(nProgramCounter + 2)
					nPosition3 = aProgramWorkingCopy(nProgramCounter + 3)
					Select Case As Const aProgramWorkingCopy(nProgramCounter)
						Case 1 ' Add
							'Opcode 1 adds together numbers read from two positions and stores the result in a third position.
							aProgramWorkingCopy(nPosition3) = aProgramWorkingCopy(nPosition1) + aProgramWorkingCopy(nPosition2)
						Case 2 ' Multiply
							'Opcode 2 multiplies the two inputs instead of adding them
							aProgramWorkingCopy(nPosition3) = aProgramWorkingCopy(nPosition1) * aProgramWorkingCopy(nPosition2)
					End Select
				Case 99 'HALT! (and DON'T catch fire!)
					Exit Do
				Case Else
					Print "?ILLEGAL OPCODE  ERROR" : Print "nProgramCounter: " & nProgramCounter & " - Opcode: " & aProgramWorkingCopy(nProgramCounter)
					System
			End Select
			nProgramCounter += 4 'Once you're done processing an opcode, move to the next one by stepping forward 4 positions
		Loop
		If aProgramWorkingCopy(0) = 19690720 Then Exit For, For 'EUREKA!
	Next nVerb
Next nNoun
Print "nNoun: " & nNoun
Print "nVerb: " & nVerb
Print : Print "ANSWER: " & 100 * nNoun + nVerb
