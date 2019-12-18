'TEST 1
'Const sInputFilename As String = "input12_test1.txt"
'Const nStepIterations As Integer = 10 : Const nStepInterval As Integer = 1

'TEST 2
'Const sInputFilename As String = "input12_test2.txt"
'Const nStepIterations As Integer = 100 : Const nStepInterval As Integer = 10

'REAL
Const sInputFilename As String = "input12.txt"
Const nStepIterations As Integer = 1000 : Const nStepInterval As Integer = 100

Type str3D
	As Integer X, Y, Z
End Type
Type strMoon
	As str3D Position, Velocity
End Type
Dim aMoons(1 To 4) As strMoon
Dim As Integer nPotentialEnergy, nKineticEnergy, nTotalEnergy

Open Exepath + "\" + sInputFilename For Input As #1
Scope
	Dim As String sInputString_X, sInputString_Y, sInputString_Z
	For nThisMoon As Integer = 1 To 4
		Input #1, sInputString_X, sInputString_Y, sInputString_Z
		With aMoons(nThisMoon).Position
			.X = Valint(Mid(sInputString_X, Instr(sInputString_X, "=") + 1))
			.Y = Valint(Mid(sInputString_Y, Instr(sInputString_Y, "=") + 1))
			.Z = Valint(Mid(sInputString_Z, Instr(sInputString_Z, "=") + 1))
		End With
	Next nThisMoon
End Scope
Close #1

For nTimeStep As Integer = 1 To nStepIterations
	'First update the velocity of every moon by applying gravity
	For nFirstMoon As Integer = 1 To 4-1
		For nSecondMoon As Integer = nFirstMoon+1 To 4
			aMoons(nFirstMoon).Velocity.X += Sgn(aMoons(nSecondMoon).Position.X - aMoons(nFirstMoon).Position.X)
			aMoons(nSecondMoon).Velocity.X += Sgn(aMoons(nFirstMoon).Position.X - aMoons(nSecondMoon).Position.X)
			aMoons(nFirstMoon).Velocity.Y += Sgn(aMoons(nSecondMoon).Position.Y - aMoons(nFirstMoon).Position.Y)
			aMoons(nSecondMoon).Velocity.Y += Sgn(aMoons(nFirstMoon).Position.Y - aMoons(nSecondMoon).Position.Y)
			aMoons(nFirstMoon).Velocity.Z += Sgn(aMoons(nSecondMoon).Position.Z - aMoons(nFirstMoon).Position.Z)
			aMoons(nSecondMoon).Velocity.Z += Sgn(aMoons(nFirstMoon).Position.Z - aMoons(nSecondMoon).Position.Z)
		Next nSecondMoon
	Next nFirstMoon
	'Then update the position of every moon by applying velocity
	For nThisMoon As Integer = 1 To 4
		With aMoons(nThisMoon)
			.Position.X += .Velocity.X
			.Position.Y += .Velocity.Y
			.Position.Z += .Velocity.Z
		End With
	Next nThisMoon
	If nTimeStep Mod nStepInterval = 0 Then
		Print "After " & nTimeStep & " step(s):"
		For nThisMoon As Integer = 1 To 4
			With aMoons(nThisMoon).Position
				Print nThisMoon & ": pos=<x=" ; .X ; ", y=" ; .Y ; ", z=" ; .Z ; ">, ";
			End With
			With aMoons(nThisMoon).Velocity
				Print "vel=<x=" ; .X ; ", y=" ; .Y ; ", z=" ; .Z ; ">"
			End With
		Next nThisMoon
		Print
	End If
Next nTimeStep

'calculate the total energy in the system.
For nThisMoon As Integer = 1 To 4
	With aMoons(nThisMoon).Position
		'A moon's *potential energy* is the sum of the absolute values of its x, y, and z position coordinates
		nPotentialEnergy = Abs(.X) + Abs(.Y) + Abs(.Z)
	End With
	With aMoons(nThisMoon).Velocity
		'A moon's *kinetic energy* is the sum of the absolute values of its velocity coordinates
		nKineticEnergy = Abs(.X) + Abs(.Y) + Abs(.Z)
	End With
	'The total energy for a single moon is its potential energy multiplied by its kinetic energy.
	nTotalEnergy += nPotentialEnergy * nKineticEnergy
Next nThisMoon
Print "Sum of total energy:" ; : Color 15 : Print nTotalEnergy : Color 7
