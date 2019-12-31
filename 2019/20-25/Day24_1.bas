'a scan of the entire area fits into a 5x5 grid
Dim aGrid(-1 To 5, -1 To 5, 1) As Integer 'Including padding... + two frames! (0, 1)
ReDim aBiodivRating(0) As Boolean

Dim sInputString As String
Dim As Integer nGridRow, nGridCol
Open Exepath + "\input24.txt" For Input As #1
	Do
		Line Input #1, sInputString
		For nGridCol = 0 To 4
			aGrid(nGridRow, nGridCol, 0) = Iif(Chr(sInputString[nGridCol]) = "#", 1, 0)
		Next nGridCol
		If Eof(1) Then Exit Do
		nGridRow += 1
	Loop
Close #1

Dim As Integer nSource, nDest = 1, nBugsCount, nBugMask
Do
	nBugMask = 0
	For nGridRow = 0 To 4
		For nGridCol = 0 To 4
			'Each tile is worth biodiversity points equal to increasing powers of two: 1, 2, 4, 8, 16, 32, and so on.
			'Add up the biodiversity points for tiles with bugs
			nBugMask += aGrid(nGridRow, nGridCol, nSource) Shl ((nGridRow * 5) + nGridCol)
			nBugsCount = 0
			'The bugs live and die based on the number of bugs in the four adjacent tiles
			nBugsCount += aGrid(nGridRow - 1, nGridCol    , nSource) + _
			              aGrid(nGridRow + 1, nGridCol    , nSource) + _
						  aGrid(nGridRow,     nGridCol - 1, nSource) + _
						  aGrid(nGridRow,     nGridCol + 1, nSource)
			Select Case aGrid(nGridRow, nGridCol, nSource)
				Case 0 'Empty space
					'An empty space becomes infested with a bug if exactly *one or two bugs* are adjacent to it
					Select Case nBugsCount
						Case 1, 2
							aGrid(nGridRow, nGridCol, nDest) = 1
						Case Else 'Otherwise, an empty space remains the same.
							aGrid(nGridRow, nGridCol, nDest) = 0
					End Select
				Case 1 'Bug
					'A bug dies (becoming an empty space) unless there is *exactly one bug* adjacent to it
					If nBugsCount <> 1 Then
						aGrid(nGridRow, nGridCol, nDest) = 0
					Else 'Otherwise, a bug remains the same.
						aGrid(nGridRow, nGridCol, nDest) = 1
					End If
			End Select
		Next nGridCol
	Next nGridRow
	If Ubound(aBiodivRating) < nBugMask Then Redim Preserve aBiodivRating(nBugMask)
	If aBiodivRating(nBugMask) Then Exit Do 'Found repeated frame!
	aBiodivRating(nBugMask) = True
	Swap nSource, nDest
Loop

Print "Biodiversity rating of first duplicate:" ; : Color 15 : Print nBugMask : Color 7