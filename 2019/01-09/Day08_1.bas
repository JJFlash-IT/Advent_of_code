Type strLayers
	PixelData(1 To 6, 1 To 25) As Integer 'The image you received is 25 pixels wide and 6 pixels tall.
End Type
Dim aPicLayers(Any) As strLayers

Dim As Integer nLayerCount, nZeroDigitCount, nMin0DigitCount = &H7FFFFFFF, nMinZeroLayer

'The input file is too long for a string variable (> 4K), and also has a LF at the end! Weird things ahead...
Dim sStringDigit As String * 1
Dim nInputDigit As Integer

Open Exepath + "\input08.txt" For Input As #1
	Do
		Redim Preserve aPicLayers(nLayerCount)
		nZeroDigitCount = 0
		For nThisDigit_Row As Integer = 1 To 6
			For nThisDigit_Col As Integer = 1 To 25
				Get #1, 0, sStringDigit
				nInputDigit = sStringDigit[0] - Asc("0")
				If nInputDigit < 0 Then Exit Do 'Weird way to check for the End Of File, I know...
				nZeroDigitCount += Cint(nInputDigit = 0) And 1
				aPicLayers(nLayerCount).PixelData(nThisDigit_Row, nThisDigit_Col) = nInputDigit
			Next nThisDigit_Col
		Next nThisDigit_Row		
		
		If nZeroDigitCount < nMin0DigitCount Then
			nMin0DigitCount = nZeroDigitCount
			nMinZeroLayer = nLayerCount
		End If
		
		nLayerCount += 1
	Loop
Close #1

Dim As Integer nOneCounter, nTwoCounter
Print nMinZeroLayer
With aPicLayers(nMinZeroLayer)
	For nThisDigit_Row As Integer = 1 To 6
		For nThisDigit_Col As Integer = 1 To 25
			Print Str(.PixelData(nThisDigit_Row, nThisDigit_Col));
			nOneCounter += Cint(.PixelData(nThisDigit_Row, nThisDigit_Col) = 1) And 1
			nTwoCounter += Cint(.PixelData(nThisDigit_Row, nThisDigit_Col) = 2) And 1
		Next nThisDigit_Col
		Print
	Next nThisDigit_Row
End With

Print : Print "Ones * Twos Result: " & nOneCounter * nTwoCounter
