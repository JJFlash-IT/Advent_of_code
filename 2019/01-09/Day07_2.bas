'*** MY FIRST VENTURE IN OBJECT ORIENTED PROGRAMMING !! ***
Dim Shared As Integer aOriginalProgram(Any)
Dim As Integer nMaxThrusterSignal, nInstructionPointer, nTempVar

Type strOperands
	Value As Integer
	Mode As Integer
End Type

'Open Exepath + "\input07_test2-Part2.txt" For Input As #1
Open Exepath + "\input07.txt" For Input As #1
	Do
		Input #1, nTempVar
		Redim Preserve aOriginalProgram(nInstructionPointer)
		aOriginalProgram(nInstructionPointer) = nTempVar
		If Eof(1) Then Exit Do
		nInstructionPointer += 1
	Loop
Close #1
Dim Shared nUpperBound As Integer : nUpperBound = nInstructionPointer

Const nQueueMaxIndex As Integer = 31

Type tAmplifier
	aProgramIntCode(nUpperBound) As Integer 
	nProgramCounter As Integer
	Declare Constructor(nPhaseSetting As Integer)
	aInputBuffer(nQueueMaxIndex) As Integer
	As Integer nQueueHead = -1, nQueueTail = -1
	Declare Sub AddToInputBuffer(nOutputSignal As Integer)
	Declare Function GetFromInputBuffer() As Integer
	bHalted As Boolean
	Declare Sub RunProgram(nOutputAmplifier As tAmplifier)
End Type

Constructor tAmplifier(nPhaseSetting As Integer)
	For K As Integer = 0 To nUpperBound
		aProgramIntCode(K) = aOriginalProgram(K)
	Next K
	AddToInputBuffer(nPhaseSetting) 'Provide each amplifier its phase setting at its first input instruction
End Constructor

Sub tAmplifier.AddToInputBuffer(nOutputSignal As Integer)
	If ((nQueueTail + 1) And nQueueMaxIndex) = nQueueHead Then Print "********** QUEUE FULL *********** " & nQueueTail & "->" & nQueueHead : System
	nQueueTail = (nQueueTail + 1) And nQueueMaxIndex
	aInputBuffer(nQueueTail) = nOutputSignal
End Sub

Function tAmplifier.GetFromInputBuffer() As Integer
	If nQueueHead = nQueueTail Then Return &H80000000 'Empty Queue!
	nQueueHead = (nQueueHead + 1) And nQueueMaxIndex
	Return aInputBuffer(nQueueHead)
End Function

Sub tAmplifier.RunProgram(oOutputAmplifier As tAmplifier)
	Dim As Integer nInstructionInt, nOpcode, nParameterCount, nInputBufValue
	Dim aParameters(1 To 3) As strOperands 'Max number of parameters: THREE
	
	If bHalted Then Exit Sub
	
	'(re)start the program!
	Do
		If nProgramCounter > nUpperBound Then Print "?OUT OF PROGRAM  ERROR" : Print "nProgramCounter: " & nProgramCounter : System
		Erase aParameters : nParameterCount = 0
		nInstructionInt = aProgramIntCode(nProgramCounter)
		nOpcode = nInstructionInt Mod 100 'Gets tens and ones digits, the Opcode!
		nInstructionInt \= 100 '"Right shift" the number twice
		
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
				nInputBufValue = GetFromInputBuffer()
				If nInputBufValue = &H80000000 Then Exit Do 'Empty Buffer!
				nProgramCounter += 1
				'Parameters that an instruction writes to *will never be in immediate mode* !
				aParameters(1).Value = aProgramIntCode(nProgramCounter)
				aProgramIntCode(aParameters(1).Value) = nInputBufValue
			Case 4 'Output - outputs the value of its only parameter.
				nProgramCounter += 1
				aParameters(1).Value = Iif(aParameters(1).Mode, aProgramIntCode(nProgramCounter), aProgramIntCode(aProgramIntCode(nProgramCounter)))
				oOutputAmplifier.AddToInputBuffer(aParameters(1).Value)
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
				bHalted = True
				Exit Do
			Case Else
				Print "?ILLEGAL OPCODE  ERROR" : Print "nProgramCounter: " & nProgramCounter & " - Opcode: " & aProgramIntCode(nProgramCounter) & "[" & nOpcode & "]"
				System
		End Select
		nProgramCounter += 1
	Loop
End Sub

'In feedback loop mode, the amplifiers need totally different phase settings: integers from 5 to 9
'Try every combination of the new phase settings on the amplifier feedback loop.
For nPhaseSetting1 As Integer = 5 To 9
	For nPhaseSetting2 As Integer = 5 To 9
		If nPhaseSetting2 = nPhaseSetting1 Then Continue For
		For nPhaseSetting3 As Integer = 5 To 9
			If nPhaseSetting3 = nPhaseSetting2 Or nPhaseSetting3 = nPhaseSetting1 Then Continue For
			For nPhaseSetting4 As Integer = 5 To 9
				If nPhaseSetting4 = nPhaseSetting3 Or nPhaseSetting4 = nPhaseSetting2 Or nPhaseSetting4 = nPhaseSetting1 Then Continue For
				For nPhaseSetting5 As Integer = 5 To 9
					If nPhaseSetting5 = nPhaseSetting4 Or nPhaseSetting5 = nPhaseSetting3 Or nPhaseSetting5 = nPhaseSetting2 Or nPhaseSetting5 = nPhaseSetting1 Then Continue For
					'The program will compute the correct output signal and *supply it back* to the amplifier
					Scope
						Dim aAmplifiers(1 To 5) As tAmplifier => {nPhaseSetting1,nPhaseSetting2,nPhaseSetting3,nPhaseSetting4,nPhaseSetting5}
						aAmplifiers(1).AddToInputBuffer(0) 'To start the process, a 0 signal is sent to amplifier A's input *exactly once*
						Do
							aAmplifiers(1).RunProgram(aAmplifiers(2))
							aAmplifiers(2).RunProgram(aAmplifiers(3))
							aAmplifiers(3).RunProgram(aAmplifiers(4))
							aAmplifiers(4).RunProgram(aAmplifiers(5))
							aAmplifiers(5).RunProgram(aAmplifiers(1)) 'the output from amplifier E is now connected into amplifier A's input
						Loop Until aAmplifiers(5).bHalted
						nTempVar = aAmplifiers(1).GetFromInputBuffer 'the last value from Amplifier E is in Amplifier A's buffer!
						If nTempVar > nMaxThrusterSignal Then nMaxThrusterSignal = nTempVar
					End Scope
				Next nPhaseSetting5
			Next nPhaseSetting4
		Next nPhaseSetting3
	Next nPhaseSetting2
Next nPhaseSetting1

Print : Print ">>> Max Thruster Signal : " & nMaxThrusterSignal

'Test 1: Max thruster signal 139629729 (from phase setting sequence 9,8,7,6,5)
'Dim aAmplifiers(1 To 5) As tAmplifier => {9,8,7,6,5}
'aAmplifiers(1).AddToInputBuffer(0) 'To start the process, a 0 signal is sent to amplifier A's input *exactly once*
'Do
'	aAmplifiers(1).RunProgram(aAmplifiers(2))
'	aAmplifiers(2).RunProgram(aAmplifiers(3))
'	aAmplifiers(3).RunProgram(aAmplifiers(4))
'	aAmplifiers(4).RunProgram(aAmplifiers(5))
'	aAmplifiers(5).RunProgram(aAmplifiers(1)) 'the output from amplifier E is now connected into amplifier A's input
'Loop Until aAmplifiers(5).bHalted
'For K As Integer = 0 To aAmplifiers(5).nQueueTail
'	Print aAmplifiers(5).aInputBuffer(K)
'Next K : Print "-" & aAmplifiers(5).nQueueHead & "-": Sleep : Print
'For K As Integer = 0 To aAmplifiers(1).nQueueTail
'	Print aAmplifiers(1).aInputBuffer(K)
'Next K : Print "-" & aAmplifiers(1).nQueueHead & "-": Sleep : Print
'Print : Print ">>> Final Output : " & aAmplifiers(1).GetFromInputBuffer

'Test 2: Max thruster signal 18216 (from phase setting sequence 9,7,8,5,6)
'Dim aAmplifiers(1 To 5) As tAmplifier => {9,7,8,5,6}
'aAmplifiers(1).AddToInputBuffer(0) 'To start the process, a 0 signal is sent to amplifier A's input *exactly once*
'Do
'	aAmplifiers(1).RunProgram(aAmplifiers(2))
'	aAmplifiers(2).RunProgram(aAmplifiers(3))
'	aAmplifiers(3).RunProgram(aAmplifiers(4))
'	aAmplifiers(4).RunProgram(aAmplifiers(5))
'	aAmplifiers(5).RunProgram(aAmplifiers(1)) 'the output from amplifier E is now connected into amplifier A's input
'Loop Until aAmplifiers(5).bHalted
'For K As Integer = 0 To aAmplifiers(5).nQueueTail
'	Print aAmplifiers(5).aInputBuffer(K)
'Next K : Print "-" & aAmplifiers(5).nQueueHead & "-": Sleep : Print
'For K As Integer = 0 To aAmplifiers(1).nQueueTail
'	Print aAmplifiers(1).aInputBuffer(K)
'Next K : Print "-" & aAmplifiers(1).nQueueHead & "-": Sleep : Print
'Print : Print ">>> Final Output : " & aAmplifiers(1).GetFromInputBuffer