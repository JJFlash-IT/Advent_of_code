Dim As Integer nModuleMass, nThisModuleFuel, nTotalFuel
Open Exepath + "\input01.txt" For Input As #1
	Do
		Input #1, nModuleMass
		nThisModuleFuel = (nModuleMass \ 3) - 2
		nTotalFuel += nThisModuleFuel
	Loop Until Eof(1)
Close #1
Print "Total fuel needed: " & nTotalFuel