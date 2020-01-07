Const nCardsNumber As Integer = 10007
Dim Shared aCardDeck(0 To nCardsNumber - 1, 1) As Integer 'Two buffers, 0 and 1
Dim Shared As Integer nSource, nDest = 1
For nThisCard As Integer = 0 To nCardsNumber - 1
	aCardDeck(nThisCard, 0) = nThisCard 'there are nCardsNumber cards in the deck numbered 0 through nCardsNumber-1 - they're still in *factory order*
Next nThisCard

'https://www.geeksforgeeks.org/program-for-array-rotation-continued-reversal-algorithm/
Sub Reverse(nFirstIndex As Integer, nSecondIndex As Integer)
	Do While nFirstIndex < nSecondIndex
		Swap aCardDeck(nFirstIndex, nSource), aCardDeck(nSecondIndex, nSource)
		nFirstIndex += 1 : nSecondIndex -= 1
	Loop
End Sub

Sub Rotate(nRotations As Integer)
	If nRotations < 0 Then nRotations = nCardsNumber - Abs(nRotations) '"Simulates" right rotation by finding complementary number of left rotations

	Reverse(0, nRotations - 1)
	Reverse(nRotations, nCardsNumber - 1)
	Reverse(0, nCardsNumber - 1)
End Sub

Sub DealWithIncrement(nIncrement As Integer)
	Dim nCardPosition As Integer
	
	'Deal the top card into the leftmost position
	aCardDeck(0, nDest) = aCardDeck(0, nSource)
	'Then, move N positions to the right and deal the next card there
	'If you would move into a position past the end, wrap around
	For nThisCard As Integer = 1 To nCardsNumber - 1
		nCardPosition = (nCardPosition + nIncrement) Mod nCardsNumber
		aCardDeck(nCardPosition, nDest) = aCardDeck(nThisCard, nSource)
	Next nThisCard
	
	Swap nSource, nDest
End Sub

Dim sInputString As String
Dim nOperand As Integer
Open Exepath + "\input22.txt" For Input As #1
	Do
		Line Input #1, sInputString
		Print sInputString & " ... " ;
		Select Case Left(sInputString, 6)
			Case "deal i" 'Deal into new stack
				Reverse(0, nCardsNumber - 1)
			Case "deal w" 'Deal with increment
				nOperand = Valint(Mid(sInputString, Instrrev(sInputString, " ") + 1))
				DealWithIncrement(nOperand)
			Case Else      'Cut
				nOperand = Valint(Mid(sInputString, Instrrev(sInputString, " ") + 1))
				Rotate(nOperand)
		End Select
		Print "Done."
	Loop Until Eof(1)
Close #1
Print

Print "Finding position of card 2019:" ;
For nThisCard As Integer = 0 To nCardsNumber - 1
	If aCardDeck(nThisCard, nSource) = 2019 Then Print nThisCard: Exit For 'EUREKA!
Next nThisCard
Print