#Include "crt.bi"

Dim As String sWireString, sDirectionItem
Dim As Integer nWireRow, nWireCol, nRowDirection, nColDirection
Dim As Integer nMinimumDistance = &H7FFFFFFF, nThisDistance

ReDim aWireMap(-13000 To 9000, -8500 To 9000) As Boolean

Declare Sub SetIncrements(sDirectionString As String, Byref nRowIncr As Integer, Byref nColIncr As Integer)

Open Exepath + "\input03.txt" For Input As #1
	'First wire!
	Line Input #1, sWireString 
	sDirectionItem = *Strtok(sWireString, ",")
	Do
		SetIncrements(sDirectionItem, nRowDirection, nColDirection)
		For nWireTravel As Integer = 1 To Valint(Mid(sDirectionItem, 2))
			nWireRow += nRowDirection : nWireCol += nColDirection
			aWireMap(nWireRow, nWireCol) = True 'This "pixel" is ON
		Next nWireTravel
		sDirectionItem = *Strtok(0, ",")
	Loop Until sDirectionItem = ""
	
	'Second wire!
	nWireRow = 0 : nWireCol = 0 'New wire, coordinates reset
	Line Input #1, sWireString 
	sDirectionItem = *Strtok(sWireString, ",")
	Do
		SetIncrements(sDirectionItem, nRowDirection, nColDirection)
		For nWireTravel As Integer = 1 To Valint(Mid(sDirectionItem, 2))
			nWireRow += nRowDirection : nWireCol += nColDirection
			If aWireMap(nWireRow, nWireCol) Then ' *IF* this "pixel" is ON....
				Print "Clash! " ; : Write nWireRow, nWireCol
				nThisDistance = Abs(nWireRow) + Abs(nWireCol) 'Distance from a 0,0 coordinate, so it's easy!
				nMinimumDistance = Iif(nThisDistance < nMinimumDistance, nThisDistance, nMinimumDistance)
			Endif
		Next nWireTravel
		sDirectionItem = *Strtok(0, ",")
	Loop Until sDirectionItem = ""
Close #1

Print: Print "Closest distance: " & nMinimumDistance

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