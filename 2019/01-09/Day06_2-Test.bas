'Print Asc("A") -- 65
'Print Asc("Y") & Asc("O") & Asc("U") ' -- 897985

#Include "crt.bi"

Dim As String sInputString, sRightString
Dim As Integer nLeftHash, nRightHash

Type strNodes
	aNodesToVisit(Any) As Integer
	Steps As Integer = &H7FFFFFFF
End Type

'This is **EXTREMELY** wasteful but should be EXTREMELY fast...
Dim Shared aNodes(65 To 677977) As strNodes
Dim Shared As Integer nStartingIndex, nEndingIndex

Function BrutalHash(sInput As String) As Integer
	Dim sTempHash As String
	For K As Integer = 0 To Len(sInput) - 1
		sTempHash &= Str(sInput[K])
	Next K
	BrutalHash = Valint(sTempHash)
End Function

Open Exepath + "\input06_testPart2.txt" For Input As #1
	Do
		Input #1, sInputString
		nLeftHash = BrutalHash(*Strtok(sInputString, ")"))
		sRightString = *Strtok(0, ")")
		Select Case sRightString
			Case "YOU" 'The *parent* node of this will be the starting point!
				nStartingIndex = nLeftHash
			Case "SAN" 'The *parent* node of this will be the ENDING point!
				nEndingIndex = nLeftHash
			Case Else
				nRightHash = BrutalHash(sRightString)
				With aNodes(nLeftHash)
					Redim Preserve .aNodesToVisit(Ubound(.aNodesToVisit) + 1)
					.aNodesToVisit(Ubound(.aNodesToVisit)) = nRightHash
				End With
				With aNodes(nRightHash)
					Redim Preserve .aNodesToVisit(Ubound(.aNodesToVisit) + 1)
					.aNodesToVisit(Ubound(.aNodesToVisit)) = nLeftHash
				End With
		End Select
	Loop Until Eof(1)
Close #1

Sub NavigateTree(nNodeIndex As Integer, nDepth As Integer = 0)
	Dim nNextNodeIndex As Integer
	
	aNodes(nNodeIndex).Steps = nDepth
	Print Space(nDepth) & "Entering node " & nNodeIndex ; : If nNodeIndex < 100 Then Print " (" & Chr(nNodeIndex) & ")" ;
	Print

	If nNodeIndex = nEndingIndex Then
		Print Space(nDepth) & "*** SANTA FOUND! *** in " & nDepth & " steps!"
	Else
		With aNodes(nNodeIndex)
			For nNextNodeIndex = 0 To Ubound(.aNodesToVisit)
				If nDepth >= aNodes(.aNodesToVisit(nNextNodeIndex)).Steps Then Continue For 'Already visited with a shorter/equal depth!
				Print Space(nDepth) & "* [" & nNodeIndex & "] Going into node " & .aNodesToVisit(nNextNodeIndex) ; : If .aNodesToVisit(nNextNodeIndex) < 100 Then Print " (" & Chr(.aNodesToVisit(nNextNodeIndex)) & ")" ;
				Print
				NavigateTree(.aNodesToVisit(nNextNodeIndex), nDepth + 1)
			Next nNextNodeIndex
		End With
	Endif
	Print Space(nDepth) & "EXITING NODE " & nNodeIndex ; : If nNodeIndex < 100 Then Print " (" & Chr(nNodeIndex) & ")" ;
	Print
End Sub

NavigateTree(nStartingIndex)
Print
Print ">>> STEPS REQUIRED: " & aNodes(nEndingIndex).Steps & " <<<"