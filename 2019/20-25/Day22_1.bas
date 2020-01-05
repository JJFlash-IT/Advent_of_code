


'https://www.geeksforgeeks.org/program-for-array-rotation-continued-reversal-algorithm/
Sub Reverse(nFirstIndex As Integer, nSecondIndex As Integer)
	Do While nFirstIndex < nSecondIndex
		Swap sPassword[nFirstIndex], sPassword[nSecondIndex]
		nFirstIndex += 1 : nSecondIndex -= 1
	Loop
End Sub

Sub Rotate(nRotations As Integer, bRotateRight As Boolean)
	nRotations Mod= nPasswordLength
	If nRotations > 0 Then
		If bRotateRight Then nRotations = nPasswordLength - nRotations 'Complementare che simula la rotazione a destra

		Reverse(0, nRotations - 1)
		Reverse(nRotations, nPasswordLength - 1)
		Reverse(0, nPasswordLength - 1)
	End If
End Sub