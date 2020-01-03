Dim Shared As Longint aOriginalProgram(Any) 'The computer should have support for large numbers...

Type strOperands
	Value As Longint 'The computer should have support for large numbers...
	Mode As Integer
End Type

Dim Shared nUpperBound As Integer
Scope
	Dim nInstructionPointer As Integer 
	Dim nTempVar As Longint 'The computer should have support for large numbers...
	Open Exepath + "\input23.txt" For Input As #1
		Do
			Input #1, nTempVar
			Redim Preserve aOriginalProgram(nInstructionPointer)
			aOriginalProgram(nInstructionPointer) = nTempVar
			If Eof(1) Then Exit Do
			nInstructionPointer += 1
		Loop
	Close #1
	nUpperBound = nInstructionPointer
End Scope

Const nQueueMaxIndex As Integer = 15

Type strNICcard
	Address As Longint = &H8000000000000000
	X As Longint = &H8000000000000000
	Y As Longint = &H8000000000000000
End Type

Type strComputer
	aProgramIntCode(nUpperBound) As Longint  'The computer should have support for large numbers...
	nProgramCounter As Integer
	nRelativeBase As Longint 'The computer should have support for large numbers... The relative base starts at 0
	Declare Constructor()
	aInputBuffer(nQueueMaxIndex) As Longint 'The computer should have support for large numbers...
	As Integer nQueueHead = -1, nQueueTail = -1
	Declare Sub AddToInputBuffer(nOutputSignal As Longint) 'The computer should have support for large numbers...
	Declare Function GetFromInputBuffer() As Longint 'The computer should have support for large numbers...
	tNICcard As strNICcard
	Declare Function RunProgram(Byref As Longint) As Integer
End Type

Constructor strComputer()
	For K As Integer = 0 To nUpperBound
		aProgramIntCode(K) = aOriginalProgram(K)
	Next K
End Constructor

Sub strComputer.AddToInputBuffer(nOutputSignal As Longint)
	Dim nQueueTailNew As Integer
	nQueueTailNew = (nQueueTail + 1) And nQueueMaxIndex
	If nQueueTailNew = nQueueHead Then Print "********** QUEUE FULL *********** " & nQueueTail & "->" & nQueueHead : System
	nQueueTail = nQueueTailNew
	aInputBuffer(nQueueTail) = nOutputSignal
End Sub

Function strComputer.GetFromInputBuffer() As Longint
	'if no packet is waiting, input instructions should receive -1
	If nQueueHead = nQueueTail Then Return -1 'Empty Queue!
	nQueueHead = (nQueueHead + 1) And nQueueMaxIndex
	Return aInputBuffer(nQueueHead)
End Function

Function strComputer.RunProgram(Byref nSharedVar As Longint) As Integer
	Static As Integer nInstructionInt, nOpcode, nParameterCount
	Static aParameters(1 To 3) As strOperands 'Max number of parameters: THREE
	Static aArgCount(1 To 9) As Const Integer => {3, 3, 1, 1, 2, 2, 3, 3, 1}
	'                                             1, 2, 3, 4, 5, 6, 7, 8, 9 
	Static aWritingParam(1 To 9) As Const Integer => {3, 3, 1, 0, 0, 0, 3, 3, 0}
	'                                                 1, 2, 3, 4, 5, 6, 7, 8, 9
	
	'There is no loop, there is only ZUUUUL
	If nProgramCounter > nUpperBound Then Print "?OUT OF PROGRAM  ERROR" : Print "nProgramCounter: " & nProgramCounter : System
	Erase aParameters : nParameterCount = 0
	nInstructionInt = aProgramIntCode(nProgramCounter)
	nOpcode = nInstructionInt Mod 100 'Gets tens and ones digits, the Opcode!
	
	If nOpcode = 99 Then Return 99 'HALT! (and DON'T catch fire!)

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
			nSharedVar = GetFromInputBuffer()
			aProgramIntCode(aParameters(1).Value) = nSharedVar
		Case 4 'Output - outputs the value of its only parameter.
			nSharedVar = aParameters(1).Value
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
	Function = nOpcode
End Function

'The computers have network addresses 0 through 49
Dim aComputer(0 To 49) As strComputer
Dim As Longint nThisComputer, nOutputValue
Dim nOpcodeReturned As Integer

'Packets sent to address 255 are handled by a device called a NAT (Not Always Transmitting)
Dim tNAT As strNICcard

'The NAT also monitors all computers on the network, which can be IDLE...
Dim nIdleCounter As Integer

'What is the first Y value delivered by the NAT to the computer at address 0 twice in a row?
Dim nLastYvalueNATsent As Longint = &H8000000000000000

'when each computer boots up, it will request its network address via a single input instruction.
'Be sure to give each computer a unique network address
For nThisComputer = 0 To 49
	aComputer(nThisComputer).AddToInputBuffer(nThisComputer)
Next nThisComputer
nThisComputer = 0

'The 50 computers will run "pseudo-parallelly", one instruction at a time...
Do
	nOpcodeReturned = aComputer(nThisComputer).RunProgram(nOutputValue)
	Select Case nOpcodeReturned
		Case 3 'INPUT
			'If all computers have empty incoming packet queues and are *continuously trying to receive packets* without sending packets,
			'the network is considered idle
			If nOutputValue = -1 Then nIdleCounter += 1 Else nIdleCounter = 0
			If nIdleCounter = 6563 Then 'This 6563 value is *empirical* ; if I choose a lower value, the NAT fires too soon...
				'Once the network is idle, the NAT sends only the last packet it received to address 0
				aComputer(0).AddToInputBuffer(tNAT.X)
				aComputer(0).AddToInputBuffer(tNAT.Y)
'				Print ,, "NAT SENT Y: " & tNAT.Y ' <--- This is somewhat interesting to see
				nIdleCounter = 0 'this will cause the computers on the network to resume activity.
				'Has NAT sent this Y value just before?
				If tNAT.Y = nLastYvalueNATsent Then
					Print "NAT sent this Y value twice in a row:" ; : Color 15 : Print nLastYvalueNATsent : Color 7
					Exit Do
				Else
					nLastYvalueNATsent = tNAT.Y
				EndIf
			EndIf
		Case 4 'OUTPUT
			With aComputer(nThisComputer).tNICcard
				If .Address = &H8000000000000000 Then
					.Address = nOutputValue
				Elseif .X = &H8000000000000000 Then
					.X = nOutputValue
				Else
					.Y = nOutputValue
					'If a packet would be sent to address 255, the NAT receives it instead
					If .Address = 255 Then
						'NAT remembers only the *last packet* it receives
						tNAT.X = .X
						tNAT.Y = .Y
'						Print , "NAT received Y: " & .Y ' <--- This is somewhat interesting to see
					Else
						aComputer(.Address).AddToInputBuffer(.X)
						aComputer(.Address).AddToInputBuffer(.Y)
					Endif
					'The "queue holder" can now be reset
					.Address = &H8000000000000000
					.X = &H8000000000000000
					'No need to reset .Y ...				
				Endif
			End With
	End Select
	nThisComputer = (nThisComputer + 1) Mod 50 'Multitasking simulation...
Loop
