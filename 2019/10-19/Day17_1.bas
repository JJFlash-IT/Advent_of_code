#Include "crt.bi"

Dim Shared As Longint aProgramIntCode(Any), nTempVar 'The computer should have support for large numbers...
Dim Shared As Integer nProgramCounter
Dim Shared bHalted As Boolean

Type strOperands
	Value As Longint 'The computer should have support for large numbers...
	Mode As Integer
End Type

Open Exepath + "\input17.txt" For Input As #1
	Do
		Input #1, nTempVar
		Redim Preserve aProgramIntCode(nProgramCounter)
		aProgramIntCode(nProgramCounter) = nTempVar
		If Eof(1) Then Exit Do
		nProgramCounter += 1
	Loop
Close #1
Dim Shared nUpperBound As Integer : nUpperBound = nProgramCounter
nProgramCounter = 0

Sub RunProgram(Byref nInput As Longint, Byref nOutput As Longint)
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
			bHalted = True
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

		Select Case As Const nOpcode
			Case 1 'Add
				'Opcode 1 adds together numbers read from two positions and stores the result in a third position.
				aProgramIntCode(aParameters(3).Value) = aParameters(1).Value + aParameters(2).Value
			Case 2 'Multiply
				'Opcode 2 multiplies the two inputs instead of adding them
				aProgramIntCode(aParameters(3).Value) = aParameters(1).Value * aParameters(2).Value
			Case 3 'Input - takes a single integer as input and saves it to the position given by its only parameter
				aProgramIntCode(aParameters(1).Value) = nInput
			Case 4 'Output - outputs the value of its only parameter.
				nOutput = aParameters(1).Value
				nProgramCounter += 1 'I have to do this since I'm exiting the loop...
				Exit Do 'The VM "pauses" after outputting stuff
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
		nProgramCounter += 1
	Loop
End Sub

Const As Integer SCAFFOLD = Asc("#"), ROBOT = Asc("^")

Dim As String sInputString, sOneLine
Dim nChar As Longint
Dim As Integer nRow, nCol, nAlignSum
Do
	RunProgram(nChar, nChar)
	sInputString &= Chr(nChar)
Loop Until bHalted
sOneLine = *Strtok(sInputString, Chr(10))
nCol = Len(sOneLine) - 1
Redim aScaffoldMap(-1 To 0, -1 To nCol+1) As Integer 'Map padding, top/bottom and left/right
Do
	For nColIndex As Integer = 0 To nCol
		aScaffoldMap(nRow, nColIndex) = sOneLine[nColIndex]
		If aScaffoldMap(nRow, nColIndex) = ROBOT Then aScaffoldMap(nRow, nColIndex) = SCAFFOLD
	Next nColIndex
	sOneLine = *Strtok(0, Chr(10))
	If sOneLine = "" Then Exit Do
	nRow += 1
	Redim Preserve aScaffoldMap(-1 To nRow, -1 To nCol+1)
Loop
Redim Preserve aScaffoldMap(-1 To nRow+1, -1 To nCol+1) 'Complete padding on rows

For nRowScan As Integer = 0 To nRow
	For nColScan As Integer = 0 To nCol
		If aScaffoldMap(nRowScan, nColScan) = SCAFFOLD Then
			If aScaffoldMap(nRowScan-1, nColScan) = SCAFFOLD And _
			aScaffoldMap(nRowScan+1, nColScan) = SCAFFOLD And _
			aScaffoldMap(nRowScan, nColScan-1) = SCAFFOLD And _
			aScaffoldMap(nRowScan, nColScan+1) = SCAFFOLD Then
				nAlignSum += nRowScan * nColScan
			Endif
		EndIf
	Next nColScan
Next nRowScan

Print "Alignment parameter sum: " & nAlignSum : Print