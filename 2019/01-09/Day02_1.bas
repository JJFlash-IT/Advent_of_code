Dim As Integer aProgramIntCode(Any), nProgramCounter, nPosition1, nPosition2, nPosition3, nTempVar

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
nProgramCounter = 0

'**ONLY REAL INPUT** before running the program, replace position 1 with the value 12 and replace position 2 with the value 2.
aProgramIntCode(1) = 12 : aProgramIntCode(2) = 2

Do
	If nProgramCounter > nUpperBound Then Print "?OUT OF PROGRAM  ERROR" : Print "nProgramCounter: " & nProgramCounter : System
	Select Case As Const aProgramIntCode(nProgramCounter)
		Case 1, 2 'Add, Multiply
			nPosition1 = aProgramIntCode(nProgramCounter + 1)
			nPosition2 = aProgramIntCode(nProgramCounter + 2)
			nPosition3 = aProgramIntCode(nProgramCounter + 3)
			Select Case As Const aProgramIntCode(nProgramCounter)
				Case 1 ' Add
					'Opcode 1 adds together numbers read from two positions and stores the result in a third position.
					aProgramIntCode(nPosition3) = aProgramIntCode(nPosition1) + aProgramIntCode(nPosition2)
				Case 2 ' Multiply
					'Opcode 2 multiplies the two inputs instead of adding them
					aProgramIntCode(nPosition3) = aProgramIntCode(nPosition1) * aProgramIntCode(nPosition2)
			End Select
		Case 99 'HALT! (and DON'T catch fire!)
			Print "Program halted at location: " & nProgramCounter
			Exit Do
		Case Else
			Print "?ILLEGAL OPCODE  ERROR" : Print "nProgramCounter: " & nProgramCounter & " - Opcode: " & aProgramIntCode(nProgramCounter)
			System
	End Select
	nProgramCounter += 4 'Once you're done processing an opcode, move to the next one by stepping forward 4 positions
Loop
Print: Print "Value at location of interest: " & aProgramIntCode(0) 'What value is left at position 0 after the program halts?
'Print: Print "Value at location of interest: " & aProgramIntCode(3) ' Test 3
'Print: Print "Value at location of interest: " & aProgramIntCode(nUpperBound) ' Test 4