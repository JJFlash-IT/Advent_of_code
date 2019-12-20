'Hint from Reddit:
'https://www.reddit.com/r/adventofcode/comments/ecogl3/2019_day_19_solutions/fbfjeyv/

Dim Shared As Longint aProgramIntCode(Any), nTempVar 'The computer should have support for large numbers...
Dim Shared As Integer nProgramCounter

Type strOperands
	Value As Longint 'The computer should have support for large numbers...
	Mode As Integer
End Type

Dim Shared aOriginalProgram(Any) As Longint
Open Exepath + "\input19.txt" For Input As #1
	Do
		Input #1, nTempVar
		Redim Preserve aOriginalProgram(nProgramCounter)
		aOriginalProgram(nProgramCounter) = nTempVar
		If Eof(1) Then Exit Do
		nProgramCounter += 1
	Loop
Close #1
Dim Shared nUpperBound As Integer : nUpperBound = nProgramCounter

Sub ResetProgram()
	Redim aProgramIntCode(nUpperBound) 'Reset the whole memory!
	For K As Integer = 0 To nUpperBound
		aProgramIntCode(K) = aOriginalProgram(K)
	Next K
	nProgramCounter = 0
End Sub

ResetProgram()

Function RunProgram(Byval nInput As Longint, Byref nOutput As Longint) As Integer
	Static As Longint nRelativeBase 'The computer should have support for large numbers... The relative base starts at 0
	Static As Integer nInstructionInt, nOpcode, nParameterCount
	Static aParameters(1 To 3) As strOperands 'Max number of parameters: THREE
	Static aArgCount(1 To 9) As Const Integer => {3, 3, 1, 1, 2, 2, 3, 3, 1}
	'                                             1, 2, 3, 4, 5, 6, 7, 8, 9 
	Static aWritingParam(1 To 9) As Const Integer => {3, 3, 1, 0, 0, 0, 3, 3, 0}
	'                                                 1, 2, 3, 4, 5, 6, 7, 8, 9
	
	Do
		If nProgramCounter > nUpperBound Then Print "?OUT OF PROGRAM  ERROR" : Print "nProgramCounter: " & nProgramCounter : System
		Erase aParameters : nParameterCount = 0
		nInstructionInt = aProgramIntCode(nProgramCounter)
		nOpcode = nInstructionInt Mod 100 'Gets tens and ones digits, the Opcode!
		
		If nOpcode = 99 Then 'HALT! (and DON'T catch fire!)
			nRelativeBase = 0 'This must be reset on each run of the program!
			Exit Do
		Endif

		nInstructionInt \= 100 '"Right shift" the number twice
		While nInstructionInt 'Are there any parameter mode inside this integer?
			nParameterCount += 1		
			aParameters(nParameterCount).Mode = nInstructionInt Mod 10 'Gets this parameter mode
			nInstructionInt \= 10 '"Right shift" the number
		Wend
		
		For nParameterCount = 1 To aArgCount(nOpcode)
			nProgramCounter += 1
			With aParameters(nParameterCount)
				.Value = aProgramIntCode(nProgramCounter)
				If .Mode <> 1 Then 'If NOT in immediate mode...
					If .Mode = 2 Then .Value += nRelativeBase 'If in relative mode, .Value is a relative address that must be added to the relative base
					'The computer's available memory should be much larger than the initial program
					If .Value > Ubound(aProgramIntCode) Then Redim Preserve aProgramIntCode(.Value)
					'Parameters that an instruction writes to *will never be in immediate mode* !
					If nParameterCount <> aWritingParam(nOpcode) Then .Value = aProgramIntCode(.Value) 'Dereferencing the address...
				Endif
			End With
		Next nParameterCount
		nProgramCounter += 1 'The programcounter now points at the *next* instruction...

		Select Case As Const nOpcode
			Case 1 'Add
				'Opcode 1 adds together numbers read from two positions and stores the result in a third position.
				aProgramIntCode(aParameters(3).Value) = aParameters(1).Value + aParameters(2).Value
			Case 2 'Multiply
				'Opcode 2 multiplies the two inputs instead of adding them
				aProgramIntCode(aParameters(3).Value) = aParameters(1).Value * aParameters(2).Value
			Case 3 'Input - takes a single integer as input and saves it to the position given by its only parameter
				aProgramIntCode(aParameters(1).Value) = nInput
				Exit Do 'The VM "pauses", awaiting input
			Case 4 'Output - outputs the value of its only parameter.
				nOutput = aParameters(1).Value
'				If (nOutput >= Asc(" ") And nOutput <= Asc ("z")) Or nOutput = 10 Then Print Chr(nOutput);
				Exit Do 'The VM "pauses" after outputting stuff
			Case 5 'jump-if-true - if the first parameter is *non-zero*, it sets the instruction pointer to the value from the second parameter.
				If aParameters(1).Value Then
					nProgramCounter = aParameters(2).Value
					'if the instruction modifies the instruction pointer, the instruction pointer is not automatically increased!
				End If
			Case 6 'jump-if-false - if the first parameter is *zero*, it sets the instruction pointer to the value from the second parameter.
				If aParameters(1).Value = 0 Then
					nProgramCounter = aParameters(2).Value
					'if the instruction modifies the instruction pointer, the instruction pointer is not automatically increased!
				End If
			Case 7 'Less than
				'if the first parameter is less than the second parameter, it stores 1 in the position given by the third parameter. Otherwise, it stores 0.
				aProgramIntCode(aParameters(3).Value) = (aParameters(1).Value < aParameters(2).Value) And 1 'If true, removes the minus sign
			Case 8 'Equals
				'if the first parameter is equal to the second parameter, it stores 1 in the position given by the third parameter. Otherwise, it stores 0.
				aProgramIntCode(aParameters(3).Value) = (aParameters(1).Value = aParameters(2).Value) And 1 'If true, removes the minus sign
			Case 9 'Relative base modifier
				'adjusts the relative base by the value of its only parameter.
				'The relative base increases (or decreases, if the value is negative) by the value of the parameter.
				nRelativeBase += aParameters(1).Value
			Case Else
				Print "?ILLEGAL OPCODE  ERROR" : Print "nProgramCounter: " & nProgramCounter & " - Opcode: " & aProgramIntCode(nProgramCounter) & "[" & nOpcode & "]"
				System
		End Select
	Loop
	Function = nOpcode
End Function

Dim As Integer nGridY = 1000, nGridX = 590, nGridY_Corner, nGridX_Corner
Do
	'The program uses two input instructions to request the X and Y position
	RunProgram(Clngint(nGridX), nTempVar)
	RunProgram(Clngint(nGridY), nTempVar)
	RunProgram(0, nTempVar) 'This to get the output value
	RunProgram(0, nTempVar) 'This is for the program to exit gracefully	
	ResetProgram()
	If nTempVar = 0 Then
		nGridX += 1 'Try the next column...
	Else
		'Is there another tractor-beam point 99 rows up and 99 columns right (the right-upper corner) ?
		nGridY_Corner = nGridY - 99 : nGridX_Corner = nGridX + 99
		RunProgram(Clngint(nGridX_Corner), nTempVar)
		RunProgram(Clngint(nGridY_Corner), nTempVar)
		RunProgram(0, nTempVar) 'This to get the output value
		RunProgram(0, nTempVar) 'This is for the program to exit gracefully	
		ResetProgram()
		If nTempVar Then 'EUREKA! We have the 100x100 square inside the tractor beam!
			Exit Do
		Else 'Try next row...
			nGridY += 1
		EndIf
	Endif
Loop
Print "Final Coordinates:"
Print "Y = " & nGridY_Corner
Print "X = " & nGridX
Print
'take that point's X coordinate, multiply it by 10000, then add the point's Y coordinate
Print "Answer:"; : Color 15: Print (nGridX * 10000) + nGridY_Corner : Color 7
