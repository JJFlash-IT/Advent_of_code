#Include "crt.bi"

Dim Shared nCheckSum As Integer
Dim As String sInputString
Dim As Integer nLeftHash, nRightHash

Type strNodes
	OrbitTotal As Integer
	aChildNodes(Any) As Integer
End Type

'This is **EXTREMELY** wasteful but should be EXTREMELY fast...
Dim Shared aNodes(494954 To 909057) As strNodes

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
		nRightHash = BrutalHash(*Strtok(0, ")"))
		With aNodes(nLeftHash)
			Redim Preserve .aChildNodes(Ubound(.aChildNodes) + 1)
			.aChildNodes(Ubound(.aChildNodes)) = nRightHash
		End With
	Loop Until Eof(1)
Close #1

Sub NavigateTree(nNodeIndex As Integer, nDepth As Integer = 0)
	nCheckSum += nDepth
	Dim nNextNodeIndex As Integer
	With aNodes(nNodeIndex)
		For nNextNodeIndex = 0 To Ubound(.aChildNodes)
			NavigateTree(.aChildNodes(nNextNodeIndex), nDepth + 1)
		Next nNextNodeIndex
	End With
End Sub

NavigateTree(BrutalHash("COM"))
Print
Print "**>> FINAL CHECKSUM: " & nCheckSum & " <<<"
