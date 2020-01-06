Const nCardsNumber As Integer = 10
Dim Shared aCardDeck(0 To nCardsNumber - 1) As Integer
For nThisCard As Integer = 0 To nCardsNumber - 1
	aCardDeck(nThisCard) = nThisCard 'there are nCardsNumber cards in the deck numbered 0 through nCardsNumber-1 - they're still in *factory order*
Next nThisCard


'https://www.geeksforgeeks.org/program-for-array-rotation-continued-reversal-algorithm/
Sub Reverse(nFirstIndex As Integer, nSecondIndex As Integer)
	Do While nFirstIndex < nSecondIndex
		Swap aCardDeck(nFirstIndex), aCardDeck(nSecondIndex)
		nFirstIndex += 1 : nSecondIndex -= 1
	Loop
End Sub

Sub Rotate(nRotations As Integer)
	nRotations Mod= nCardsNumber
	If nRotations < 0 Then nRotations = nCardsNumber - nRotations '"Simulates" right rotation by finding complementary number of left rotations

		Reverse(0, nRotations - 1)
		Reverse(nRotations, nCardsNumber - 1)
		Reverse(0, nCardsNumber - 1)
	End If
End Sub