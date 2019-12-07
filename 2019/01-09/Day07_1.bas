Dim Shared As Integer aOriginalProgram(Any)
Dim As Integer nMaxThrusterSignal, nInstructionPointer, nTempVar

Type strOperands
	Value As Integer
	Mode As Integer
End Type

'Open Exepath + "\input07_test3.txt" For Input As #1
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

Function RunProgram(nPhaseSetting As Integer, nAmplifierInput As Integer) As Integer
	Dim As Integer aProgramIntCode(nUpperBound), nInstructionInt, nOpcode, nProgramCounter, nParameterCount, nInputType
	Dim aParameters(1 To 3) As strOperands 'Max number of parameters: THREE
	
	'Load program copy
	For K As Integer = 0 To nUpperBound
		aProgramIntCode(K) = aOriginalProgram(K)
	Next K
	
	'Start the program!
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
				nProgramCounter += 1
				'Parameters that an instruction writes to *will never be in immediate mode* !
				aParameters(1).Value = aProgramIntCode(nProgramCounter)
				Select Case nInputType
					Case 0
						aProgramIntCode(aParameters(1).Value) = nPhaseSetting 'The program will *first* use an input instruction to ask the amplifier for its current *phase setting*
						nInputType = 1
					Case 1
						aProgramIntCode(aParameters(1).Value) = nAmplifierInput 'The program will then call another input instruction to get the amplifier's *input signal*
						nInputType = 2
					Case Else
						Print "?ANOTHER INPUT REQUESTED  ERROR" : System
				End Select
			Case 4 'Output - outputs the value of its only parameter.
				nProgramCounter += 1
				aParameters(1).Value = Iif(aParameters(1).Mode, aProgramIntCode(nProgramCounter), aProgramIntCode(aProgramIntCode(nProgramCounter)))
				RunProgram = aParameters(1).Value
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
'				Print "Program halted at location: " & nProgramCounter & " [" & nPhaseSetting & "]"
				Exit Do
			Case Else
				Print "?ILLEGAL OPCODE  ERROR" : Print "nProgramCounter: " & nProgramCounter & " - Opcode: " & aProgramIntCode(nProgramCounter) & "[" & nOpcode & "]"
				System
		End Select
		nProgramCounter += 1
	Loop
End Function

'phase setting (an integer from 0 to 4) - Try every combination of phase settings on the amplifiers
'Each phase setting is used *exactly once*
For nPhaseSetting1 As Integer = 0 To 4
	For nPhaseSetting2 As Integer = 0 To 4
		If nPhaseSetting2 = nPhaseSetting1 Then Continue For
		For nPhaseSetting3 As Integer = 0 To 4
			If nPhaseSetting3 = nPhaseSetting2 Or nPhaseSetting3 = nPhaseSetting1 Then Continue For
			For nPhaseSetting4 As Integer = 0 To 4
				If nPhaseSetting4 = nPhaseSetting3 Or nPhaseSetting4 = nPhaseSetting2 Or nPhaseSetting4 = nPhaseSetting1 Then Continue For
				For nPhaseSetting5 As Integer = 0 To 4
					If nPhaseSetting5 = nPhaseSetting4 Or nPhaseSetting5 = nPhaseSetting3 Or nPhaseSetting5 = nPhaseSetting2 Or nPhaseSetting5 = nPhaseSetting1 Then Continue For
					'The program will compute the correct output signal and *supply it back* to the amplifier
					nTempVar = RunProgram(nPhaseSetting1,        0) 'The first amplifier's input value is 0
					nTempVar = RunProgram(nPhaseSetting2, nTempVar)
					nTempVar = RunProgram(nPhaseSetting3, nTempVar)
					nTempVar = RunProgram(nPhaseSetting4, nTempVar)
					nTempVar = RunProgram(nPhaseSetting5, nTempVar)
					If nTempVar > nMaxThrusterSignal Then nMaxThrusterSignal = nTempVar
				Next nPhaseSetting5
			Next nPhaseSetting4
		Next nPhaseSetting3
	Next nPhaseSetting2
Next nPhaseSetting1

Print : Print ">>> Max Thruster Signal : " & nMaxThrusterSignal

'Test 1: Max thruster signal 43210 (from phase setting sequence 4,3,2,1,0)
'nTempVar = RunProgram(4, 0) 'The first amplifier's input value is 0
'nTempVar = RunProgram(3, nTempVar)
'nTempVar = RunProgram(2, nTempVar)
'nTempVar = RunProgram(1, nTempVar)
'nTempVar = RunProgram(0, nTempVar)
'Print : Print ">>> Final Output : " & nTempVar

'Test 2: Max thruster signal 54321 (from phase setting sequence 0,1,2,3,4)
'nTempVar = RunProgram(0, 0) 'The first amplifier's input value is 0
'nTempVar = RunProgram(1, nTempVar)
'nTempVar = RunProgram(2, nTempVar)
'nTempVar = RunProgram(3, nTempVar)
'nTempVar = RunProgram(4, nTempVar)
'Print : Print ">>> Final Output : " & nTempVar

'Test 3: Max thruster signal 65210 (from phase setting sequence 1,0,4,3,2)
'nTempVar = RunProgram(1, 0) 'The first amplifier's input value is 0
'nTempVar = RunProgram(0, nTempVar)
'nTempVar = RunProgram(4, nTempVar)
'nTempVar = RunProgram(3, nTempVar)
'nTempVar = RunProgram(2, nTempVar)
'Print : Print ">>> Final Output : " & nTempVar