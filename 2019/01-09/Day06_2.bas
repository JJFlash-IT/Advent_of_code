#Include "crt.bi"

Dim As String sInputString, sRightString
Dim As Integer nLeftHash, nRightHash

Type strNodes
	aNodesToVisit(Any) As Integer
	Steps As Integer = &H7FFFFFFF
End Type

'This is **EXTREMELY** wasteful but should be EXTREMELY fast...
Dim Shared aNodes(494954 To 909057) As strNodes
Dim Shared As Integer nStartingIndex, nEndingIndex

Function BrutalHash(sInput As String) As Integer
	Dim sTempHash As String
	For K As Integer = 0 To Len(sInput) - 1
		sTempHash &= Str(sInput[K])
	Next K
	BrutalHash = Valint(sTempHash)
End Function

Open Exepath + "\input06.txt" For Input As #1
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
				'The parent (nLeftHash) node has to visit this child (nRightHash) node
				With aNodes(nLeftHash)
					Redim Preserve .aNodesToVisit(Ubound(.aNodesToVisit) + 1)
					.aNodesToVisit(Ubound(.aNodesToVisit)) = nRightHash
				End With
				'ALSO! The child (nRightHash) node has to visit his parent (nLeftHash) node!
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

	If nNodeIndex <> nEndingIndex Then
		With aNodes(nNodeIndex)
			For nNextNodeIndex = 0 To Ubound(.aNodesToVisit)
				If nDepth >= aNodes(.aNodesToVisit(nNextNodeIndex)).Steps Then Continue For 'Already visited with a shorter/equal depth, don't go again!
				NavigateTree(.aNodesToVisit(nNextNodeIndex), nDepth + 1)
			Next nNextNodeIndex
		End With
	Endif
End Sub

NavigateTree(nStartingIndex)
Print
Print ">>> STEPS REQUIRED: " & aNodes(nEndingIndex).Steps & " <<<"