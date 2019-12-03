#Include "crt.bi"

Dim As String sWireString, sDirectionItem
Dim As Integer nWireRow, nWireCol, nRowDirection, nColDirection, nStepsCount
Dim As Integer nMinimumSteps = &H7FFFFFFF, nThisStepTotal

Type strWireMap
	Wire1Visited As Boolean
	Wire1Steps As Ushort
	Wire2Visited As Boolean
End Type

'This *HAS* to be compiled in 64-bit, otherwise the program *E X P L O D E S*
ReDim aWireMap(-13000 To 9000, -8500 To 9000) As strWireMap

Declare Sub SetIncrements(sDirectionString As String, Byref nRowIncr As Integer, Byref nColIncr As Integer)

Open Exepath + "\input03.txt" For Input As #1
	'First wire!
	Line Input #1, sWireString 
	sDirectionItem = *Strtok(sWireString, ",")
	Do
		SetIncrements(sDirectionItem, nRowDirection, nColDirection)
		For nWireTravel As Integer = 1 To Valint(Mid(sDirectionItem, 2))
			nWireRow += nRowDirection : nWireCol += nColDirection
			nStepsCount += 1
			With aWireMap(nWireRow, nWireCol)
				'If a wire visits a position on the grid multiple times, use the steps value from the *FIRST* time it visits that position
				If .Wire1Visited = False Then ' *IF* this "pixel" is OFF...
					.Wire1Visited = True
					.Wire1Steps = nStepsCount
				Endif
			End With
		Next nWireTravel
		sDirectionItem = *Strtok(0, ",")
	Loop Until sDirectionItem = ""
	
	'Second wire!
	nWireRow = 0 : nWireCol = 0  : nStepsCount = 0 'New wire, everything resets
	Line Input #1, sWireString 
	sDirectionItem = *Strtok(sWireString, ",")
	Do
		SetIncrements(sDirectionItem, nRowDirection, nColDirection)
		For nWireTravel As Integer = 1 To Valint(Mid(sDirectionItem, 2))
			nWireRow += nRowDirection : nWireCol += nColDirection
			nStepsCount += 1
			With aWireMap(nWireRow, nWireCol)
				If .Wire1Visited Then '*IF* this "pixel" is ON....
					If Not .Wire2Visited Then 'If the second wire is passing here *for the first time* ...
						Print "Clash! " ; : Write nWireRow, nWireCol
						.Wire2Visited = True
						nThisStepTotal = .Wire1Steps + nStepsCount
						nMinimumSteps = Iif(nThisStepTotal < nMinimumSteps, nThisStepTotal, nMinimumSteps)
					Endif
				Endif
			End With
		Next nWireTravel
		sDirectionItem = *Strtok(0, ",")
	Loop Until sDirectionItem = ""
Close #1

Print: Print "Fewest combined steps: " & nMinimumSteps

Sub SetIncrements (sDirectionString As String, Byref nRowIncr As Integer, Byref nColIncr As Integer)
	Select Case Left(sDirectionString, 1)
		Case "U"
			nRowIncr = -1 : nColIncr = 0
		Case "R"
			nRowIncr = 0 : nColIncr = 1
		Case "D"
			nRowIncr = 1 : nColIncr = 0			
		Case "L"
			nRowIncr = 0 : nColIncr = -1
	End Select
End Sub