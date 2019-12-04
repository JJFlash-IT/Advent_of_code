' Your puzzle input is 245318-765747

Dim As Integer nThisPassword, nPasswordPosition, nThisDigit, nLastDigit, nValidPasswordsCount, nAdjacentNumberCount
Dim sPasswordString As String * 6 = String(6, " ")
Dim As Boolean bAdjacentNumber

For nThisPassword = 245318 To 765747
	Mid(sPasswordString, 1) = Str(nThisPassword)
	bAdjacentNumber = False : nAdjacentNumberCount = 0
	nLastDigit = 0
	For nPasswordPosition = 0 To 5
		nThisDigit = sPasswordString[nPasswordPosition]
		If nThisDigit >= nLastDigit Then 'Going from left to right, the digits *never decrease* - only ever *increase* or *stay the same*
			If nThisDigit <> nLastDigit Then
				'the two adjacent matching digits MUST NOT BE part of a larger group of matching digits. - This is a OR condition, so I won't reset bAdjacentNumber!
				If nAdjacentNumberCount = 2 Then bAdjacentNumber = True
				nAdjacentNumberCount = 1
			Else
				nAdjacentNumberCount += 1
			Endif
			nLastDigit = nThisDigit
		Else
			Exit For
		Endif
	Next nPasswordPosition
	If nPasswordPosition = 6 Then 'If the inner loop exits early, the password is not a valid one
		If nAdjacentNumberCount = 2 Then bAdjacentNumber = True ' I *have* to check this again, in case the two adjacent matching digits come LAST in the password
		If bAdjacentNumber Then 
			nValidPasswordsCount += 1 
			Print nThisPassword
		Endif
	Endif
Next nThisPassword

Print : Print "Valid passwords: " & nValidPasswordsCount
