#Include "crt.bi"

Dim Shared As Longint aProgramIntCode(Any), nTempVar 'The computer should have support for large numbers...
Dim Shared As Integer nProgramCounter

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

Function RunProgram(Byref sInput As String, Byref nOutput As Longint) As Integer
	Static As Longint nRelativeBase 'The computer should have support for large numbers... The relative base starts at 0
	Static As Integer nInstructionInt, nOpcode, nParameterCount
	Static aParameters(1 To 3) As strOperands 'Max number of parameters: THREE
	Static aArgCount(1 To 9) As Const Integer => {3, 3, 1, 1, 2, 2, 3, 3, 1}
	'                                             1, 2, 3, 4, 5, 6, 7, 8, 9 
	Static aWritingParam(1 To 9) As Const Integer => {3, 3, 1, 0, 0, 0, 3, 3, 0}
	'                                                 1, 2, 3, 4, 5, 6, 7, 8, 9
	Static sInputBuffer As String
	
	sInputBuffer &= sInput
	
	Do
		If nProgramCounter > nUpperBound Then Print "?OUT OF PROGRAM  ERROR" : Print "nProgramCounter: " & nProgramCounter : System
		Erase aParameters : nParameterCount = 0
		nInstructionInt = aProgramIntCode(nProgramCounter)
		nOpcode = nInstructionInt Mod 100 'Gets tens and ones digits, the Opcode!
		
		If nOpcode = 99 Then 'HALT! (and DON'T catch fire!)
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
				If sInputBuffer = "" Then
					nProgramCounter -= 2 'Go back to the Input instruction, so it can be tried again later
					Exit Do 'The VM "pauses", awaiting input
				Else
					aProgramIntCode(aParameters(1).Value) = Clngint(sInputBuffer[0])
					Print Chr(sInputBuffer[0]);
					sInputBuffer = Mid(sInputBuffer, 2) 'Eat the character just read
				Endif
			Case 4 'Output - outputs the value of its only parameter.
				nOutput = aParameters(1).Value
				Print Chr(nOutput);
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

Const As Integer SCAFFOLD = Asc("#"), ROBOT = Asc("^")

Type strDirection
	As Integer IncrRow, IncrCol
End Type
Dim aDirections(0 To 3) As Const strDirection => {(-1,0), (0,1), (1,0), (0,-1)} 'North, east, south, west

Type strRobot
	Row As Integer
	Col As Integer
	Direction As Integer
End Type
Dim As strRobot tRobot, tRobotTemp

Dim As String sInputString, sOneLine
Dim nChar As Longint
Dim As Integer nRow, nCol, nTurnDirection
'Force the robot to wake up by changing the value at address 0 from 1 to 2.
aProgramIntCode(0) = 2
Do
	If RunProgram("", nChar) <> 4 Then Exit Do 'If the VM pauses NOT with an Output instruction, the map has been entirely sent
	sInputString &= Chr(nChar)
Loop

sOneLine = *Strtok(sInputString, Chr(10))
nCol = Len(sOneLine) - 1
Redim aScaffoldMap(-1 To 0, -1 To nCol+1) As Integer 'Map padding, top and left/right
Do
	For nColIndex As Integer = 0 To nCol
		If sOneLine[nColIndex] = ROBOT Then tRobot.Row = nRow : tRobot.Col = nColIndex 'The robot starts facing up, so Direction = 0 is ok
		aScaffoldMap(nRow, nColIndex) = sOneLine[nColIndex]
	Next nColIndex
	sOneLine = *Strtok(0, Chr(10)) 'This gets ugly: Strtok eats the blank line after the initial map, and so the input prompt "Main:" will be included in the map. Fortunately, this won't affect the "pathfinding"
	If sOneLine = "" Then Exit Do
	nRow += 1 'It will end with one more Row than necessary...
	Redim Preserve aScaffoldMap(-1 To nRow, -1 To nCol+1)
Loop
Redim Preserve aScaffoldMap(-1 To nRow+1, -1 To nCol+1) 'Complete padding on rows

Dim nRobotSteps As Integer
Do
	'** This works because the path has NO "T" intersection! So once there's a void sector ahead, it's either left or right (OR the path has ended!) **
	'* This assumes that the robot HAS to turn first (and has to turn only once)! *
	'Where should the robot turn?
	For nTurnDirection= -1 To 1 Step 2 'First on its left, then on its right
		tRobotTemp = tRobot 'Copy the position & direction of the real robot...
		tRobotTemp.Row += aDirections((tRobotTemp.Direction + nTurnDirection) And 3).IncrRow
		tRobotTemp.Col += aDirections((tRobotTemp.Direction + nTurnDirection) And 3).IncrCol
		If aScaffoldMap(tRobotTemp.Row, tRobotTemp.Col) = SCAFFOLD Then Exit For 'Found a scaffold sector in this direction!
	Next nTurnDirection
	If nTurnDirection = 3 Then Exit Do 'No way to turn, the path has ended!
'	Print Iif(nTurnDirection = 1, "R,", "L,"); 'This Print and the next couple of Prints in this part will print out the "unpacked" solution...
	
	tRobot.Direction = (tRobot.Direction + nTurnDirection) And 3
	nRobotSteps = 0
	Do
		tRobotTemp = tRobot 'Copy the position & direction of the real robot...
		tRobotTemp.Row += aDirections(tRobotTemp.Direction).IncrRow
		tRobotTemp.Col += aDirections(tRobotTemp.Direction).IncrCol
		If aScaffoldMap(tRobotTemp.Row, tRobotTemp.Col) <> SCAFFOLD Then Exit Do 'Found a void sector! :-o
		nRobotSteps += 1
		tRobot = tRobotTemp 'Move confirmed!
	Loop
'	Print Str(nRobotSteps) & ",";
Loop
'Print

'*** HERE COMES THE CHEATING!
'These were extracted by hand, using Notepad++ . This is _probably_ easy to do with regular expressions, maybe SOMEDAY I'll do it the proper way...
Dim As String sMainFunc  = "A,B,A,C,B,C,A,B,A,C"
Dim As String sFunctionA = "R,6,L,10,R,8,R,8"
Dim As String sFunctionB = "R,12,L,8,L,10"
Dim As String sFunctionC = "R,12,L,10,R,6,L,10"

'First, you will be prompted for the main movement routine.
'Then, you will be prompted for each movement function.
'Finally, you will be asked whether you want to see a continuous video feed; provide either y or n and a newline
RunProgram(sMainFunc & Chr(10) & sFunctionA & Chr(10) & sFunctionB & Chr(10) & sFunctionC & Chr(10) & "n" & Chr(10), nChar)

Do
Loop Until RunProgram("", nChar) = 99

'Once it finishes, the robot will report the amount of space dust it collected as a large, non-ASCII value in a single output instruction.
'The end answer will get initially printed as a char, so a "4" appears, then it is printed correctly here. This is ugly, but I'm tired of this... 
Print : Print "---END---" : Print nChar

