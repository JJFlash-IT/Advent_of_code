' Your puzzle input is 245318-765747

Dim As Integer nThisPassword, nPasswordPosition, nThisDigit, nLastDigit, nValidPasswordsCount
Dim sPasswordString As String * 6 = String(6, " ")
Dim As Boolean bAdjacentNumber

For nThisPassword = 245318 To 765747
	Mid(sPasswordString, 1) = Str(nThisPassword)
	bAdjacentNumber = False
	nLastDigit = 0
	For nPasswordPosition = 0 To 5
		nThisDigit = sPasswordString[nPasswordPosition]
		If nThisDigit >= nLastDigit Then 'Going from left to right, the digits *never decrease* - only ever *increase* or *stay the same*
			If nThisDigit = nLastDigit Then bAdjacentNumber = True 'Two adjacent digits are the same (like 22 in 122345)
			nLastDigit = nThisDigit
		Else
			Exit For
		Endif
	Next nPasswordPosition
	If nPasswordPosition = 6 And bAdjacentNumber Then
		nValidPasswordsCount += 1 'If the inner loop exits early, the password is not a valid one
		Print nThisPassword
	Endif
Next nThisPassword

Print : Print "Valid passwords: " & nValidPasswordsCount