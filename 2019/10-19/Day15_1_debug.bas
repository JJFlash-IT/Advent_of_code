Dim Shared As Longint aProgramIntCode(Any), nTempVar, nRelativeBase 'The computer should have support for large numbers... The relative base starts at 0
Dim Shared As Integer nProgramCounter
Dim Shared bHalted As Boolean

Type strOperands
	Value As Longint 'The computer should have support for large numbers...
	Mode As Integer
End Type

Open Exepath + "\input15.txt" For Input As #1
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

Type strCoordinates
	Row As Integer
	Col As Integer
	Declare Operator += (rhs As strCoordinates)
End Type
Dim aDroid As strCoordinates
Dim nSteps As Integer

'Only four movement commands are understood: north (1), south (2), west (3), and east (4)
Dim aDirections(1 To 4) As strCoordinates => {(-1, 0), (1, 0), (0, -1), (0, 1)}
Dim aDirBacktrack (1 To 4) As Integer => {2, 1, 4, 3}
Dim aDirectionNames(1 To 4) As String => {"North", "South", "West", "East"}

Type strMap
	LastDir As Integer
	BacktrackDir As Integer
End Type
ReDim aMap(-250 To 250, -250 To 250) As strMap

Operator strCoordinates.+= (rhs As strCoordinates)
	Row += rhs.Row
	Col += rhs.Col
End Operator

Dim As Longint nDroidStatus, nThisDirection
Do
	For nThisDirection = aMap(aDroid.Row, aDroid.Col).LastDir + 1 To 4
		With aDroid
			Print "Droid location: " & .Row & "," & .Col & " -- Steps: " & nSteps
			Print , "LastDir: " & aMap(.Row, .Col).LastDir
			Print , "BacktrackDir: " & aMap(.Row, .Col).BacktrackDir
			If nThisDirection = aMap(aDroid.Row, aDroid.Col).BacktrackDir Then
				Print , "! EARLY BACKTRACK!    " & aDirectionNames(nThisDirection) & " (" & nThisDirection & ")"
			Else
				Print , "* Trying direction: " & aDirectionNames(nThisDirection) & " (" & nThisDirection & ")"
			Endif
			Print
			Getkey
		End With
		If nThisDirection = aMap(aDroid.Row, aDroid.Col).BacktrackDir Then Continue For 'It's useless exploring the location the droid has just come from!
		RunProgram(nThisDirection, nDroidStatus)
		If nDroidStatus = 0 Then Continue For '0: The repair droid hit a wall. Its position *has not changed*
		nSteps += 1                           '1 or 2: The repair droid has moved one step in the requested direction
		If nDroidStatus = 2 Then              '2: The repair droid has moved, its new position is the *target*
			Print "Steps to the oxygen tank: " & nSteps
			System
		Endif
		aMap(aDroid.Row, aDroid.Col).LastDir = nThisDirection
		aDroid += aDirections(nThisDirection)
		aMap(aDroid.Row, aDroid.Col).BacktrackDir = aDirBacktrack(nThisDirection)
		Continue Do 'Next recursion!
	Next nThisDirection
	Print "** BACKTRACK! ** " 'Backtrack!
	nThisDirection = aMap(aDroid.Row, aDroid.Col).BacktrackDir
	RunProgram(nThisDirection, nDroidStatus)
	If nDroidStatus <> 1 Then Print "*** CONTEXT SYNC ERROR!  " & nDroidStatus : System
	nSteps -= 1 'Since the droid is backtracking, so is the number of steps...
	aDroid += aDirections(nThisDirection)
Loop
