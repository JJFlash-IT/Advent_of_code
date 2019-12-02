Dim As Integer nModuleMass, nThisModuleFuel, nTotalFuel
Open Exepath + "\input01.txt" For Input As #1
	Do
		Input #1, nModuleMass
		Do
			nThisModuleFuel = (nModuleMass \ 3) - 2
			If nThisModuleFuel <= 0 Then Exit Do
			nTotalFuel += nThisModuleFuel
			nModuleMass = nThisModuleFuel
		Loop
	Loop Until Eof(1)
Close #1
Print "Total fuel needed: " & nTotalFuel