Type strLayers
	PixelData(1 To 6, 1 To 25) As Integer 'The image you received is 25 pixels wide and 6 pixels tall.
End Type
Dim aPicLayers(Any) As strLayers

Dim As Integer nLayerCount

'The input file is too long for a string variable (> 4K), and also has a LF at the end! Weird things ahead...
Dim sStringDigit As String * 1
Dim nInputDigit As Integer

Open Exepath + "\input08.txt" For Input As #1
	Do
		Redim Preserve aPicLayers(nLayerCount)
		For nThisDigit_Row As Integer = 1 To 6
			For nThisDigit_Col As Integer = 1 To 25
				Get #1, 0, sStringDigit
				nInputDigit = sStringDigit[0] - Asc("0")
				If nInputDigit < 0 Then Exit Do 'Weird way to check for the End Of File, I know...
				aPicLayers(nLayerCount).PixelData(nThisDigit_Row, nThisDigit_Col) = nInputDigit
			Next nThisDigit_Col
		Next nThisDigit_Row		
		
		nLayerCount += 1		
	Loop
Close #1
nLayerCount -= 1 'Ugly hack, yes...

For nThisDigit_Row As Integer = 1 To 6
	For nThisDigit_Col As Integer = 1 To 25
		For nThisLayer As Integer = 0 To nLayerCount
			With aPicLayers(nThisLayer)
				Select Case .PixelData(nThisDigit_Row, nThisDigit_Col)
					'The digits indicate the color of the corresponding pixel: 0 is black, 1 is white, and 2 is transparent.
					Case 0 'Black
						Print " ";
						Exit For
					Case 1 'White
						Print "#";
						Exit For
				End Select
			End With
		Next nThisLayer 
	Next nThisDigit_Col
	Print
Next nThisDigit_Row
	



