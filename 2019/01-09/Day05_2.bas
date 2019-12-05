Dim As Integer aProgramIntCode(Any), nInstructionInt, nOpcode, nProgramCounter, nParameterCount, nTempVar

Type strOperands
	Value As Integer
	Mode As Integer
End Type
Dim aParameters(1 To 3) As strOperands 'Max number of parameters: THREE

Open Exepath + "\input05.txt" For Input As #1
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

Do
	If nProgramCounter > nUpperBound Then Print "?OUT OF PROGRAM  ERROR" : Print "nProgramCounter: " & nProgramCounter : System
	Erase aParameters : nParameterCount = 0
	nInstructionInt = aProgramIntCode(nProgramCounter)
	nOpcode = nInstructionInt Mod 10 'Gets ones digit
	nInstructionInt \= 10 '"Right shift" the number
	nOpcode = ((nInstructionInt Mod 10) * 10) + nOpcode 'Extracts tens digit, shifts it to the left, adds the ones digit - tada, the Opcode!
	nInstructionInt \= 10 '"Right shift" the number
	
	While nInstructionInt 'Are there any parameter mode inside this integer?
		nParameterCount += 1		
		aParameters(nParameterCount).Mode = nInstructionInt Mod 10 'Gets this parameter mode
		nInstructionInt \= 10 '"Right shift" the number
	Wend
	
	Select Case As Const nOpcode
		Case 1, 2, 7, 8 'Add, Multiply, less than, equals (three-parameter instructions)
			nProgramCounter += 1
			aParameters(1).Value = Iif(aParameters(1).Mode, aProgramIntCode(nProgramCounter), aProgramIntCode(aProgramIntCode(nProgramCounter)))
			nProgramCounter += 1
			aParameters(2).Value = Iif(aParameters(2).Mode, aProgramIntCode(nProgramCounter), aProgramIntCode(aProgramIntCode(nProgramCounter)))
			nProgramCounter += 1
			'Parameters that an instruction writes to *will never be in immediate mode* !
			aParameters(3).Value = aProgramIntCode(nProgramCounter)
			Select Case As Const nOpcode
				Case 1 'Add
					'Opcode 1 adds together numbers read from two positions and stores the result in a third position.
					aProgramIntCode(aParameters(3).Value) = aParameters(1).Value + aParameters(2).Value
				Case 2 'Multiply
					'Opcode 2 multiplies the two inputs instead of adding them
					aProgramIntCode(aParameters(3).Value) = aParameters(1).Value * aParameters(2).Value
				Case 7 'Less than
					'if the first parameter is less than the second parameter, it stores 1 in the position given by the third parameter. Otherwise, it stores 0.
					aProgramIntCode(aParameters(3).Value) = (aParameters(1).Value < aParameters(2).Value) And 1 'If true, removes the minus sign
				Case 8 'Equals
					'if the first parameter is equal to the second parameter, it stores 1 in the position given by the third parameter. Otherwise, it stores 0.
					aProgramIntCode(aParameters(3).Value) = (aParameters(1).Value = aParameters(2).Value) And 1 'If true, removes the minus sign
			End Select
		Case 3 'Input - takes a single integer as input and saves it to the position given by its only parameter
			nProgramCounter += 1
			'Parameters that an instruction writes to *will never be in immediate mode* !
			aParameters(1).Value = aProgramIntCode(nProgramCounter)
			Input "Input" ; nTempVar
			aProgramIntCode(aParameters(1).Value) = nTempVar
		Case 4 'Output - outputs the value of its only parameter.
			nProgramCounter += 1
			aParameters(1).Value = Iif(aParameters(1).Mode, aProgramIntCode(nProgramCounter), aProgramIntCode(aProgramIntCode(nProgramCounter)))
			Print aParameters(1).Value
		Case 5, 6 'jump-if-true, jump-if-false (two-parameter instructions)
			nProgramCounter += 1
			aParameters(1).Value = Iif(aParameters(1).Mode, aProgramIntCode(nProgramCounter), aProgramIntCode(aProgramIntCode(nProgramCounter)))
			nProgramCounter += 1
			aParameters(2).Value = Iif(aParameters(2).Mode, aProgramIntCode(nProgramCounter), aProgramIntCode(aProgramIntCode(nProgramCounter)))
			Select Case As Const nOpcode
				Case 5 'jump-if-true - if the first parameter is *non-zero*, it sets the instruction pointer to the value from the second parameter.
					If aParameters(1).Value Then
						nProgramCounter = aParameters(2).Value
						'if the instruction modifies the instruction pointer, the instruction pointer is not automatically increased!
						Continue Do 'Skips the final programcounter increment
					End If
				Case 6 'jump-if-false - if the first parameter is *zero*, it sets the instruction pointer to the value from the second parameter.
					If aParameters(1).Value = 0 Then
						nProgramCounter = aParameters(2).Value
						'if the instruction modifies the instruction pointer, the instruction pointer is not automatically increased!
						Continue Do 'Skips the final programcounter increment
					End If
			End Select
		Case 99 'HALT! (and DON'T catch fire!)
			Print "Program halted at location: " & nProgramCounter
			Exit Do
		Case Else
			Print "?ILLEGAL OPCODE  ERROR" : Print "nProgramCounter: " & nProgramCounter & " - Opcode: " & aProgramIntCode(nProgramCounter) & "[" & nOpcode & "]"
			System
	End Select
	nProgramCounter += 1
Loop