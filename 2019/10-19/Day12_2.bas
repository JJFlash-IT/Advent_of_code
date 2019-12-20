'Hint from Reddit:
'https://www.reddit.com/r/adventofcode/comments/e9jxh2/help_2019_day_12_part_2_what_am_i_not_seeing/far9cgu/
'Program to find the LCM of two numbers:
'https://www.geeksforgeeks.org/program-to-find-lcm-of-two-numbers/
'How to find the LCM of three numbers:
'https://www.quora.com/How-do-I-find-the-LCM-of-three-numbers/answer/Giorgos-Giapitzakis

'TEST 1
'Const sInputFilename As String = "input12_test1.txt"

'TEST 2
'Const sInputFilename As String = "input12_test2.txt"

'REAL
Const sInputFilename As String = "input12.txt"

Enum
	POS_X, POS_Y, POS_Z, VEL_X, VEL_Y, VEL_Z
End Enum

Type strMoon
	V(VEL_Z) As Integer
End Type
Dim Shared aMoons(1 To 4) As strMoon 'Io, Europa, Ganymede, and Callisto
Dim aLoopCount(0 To 2) As Ulongint 'Big numbers here! - How many iterations *for each dimension* so that a cycle repeats?
Dim nDimensionsCycled As Integer
Dim nLoopSteps As Ulongint 'This is going to be a BIIIIG number!

Open Exepath + "\" + sInputFilename For Input As #1
Scope
	Dim As String sInputString_X, sInputString_Y, sInputString_Z
	For nThisMoon As Integer = 1 To 4
		Input #1, sInputString_X, sInputString_Y, sInputString_Z
		With aMoons(nThisMoon)
			.V(POS_X) = Valint(Mid(sInputString_X, Instr(sInputString_X, "=") + 1))
			.V(POS_Y) = Valint(Mid(sInputString_Y, Instr(sInputString_Y, "=") + 1))
			.V(POS_Z) = Valint(Mid(sInputString_Z, Instr(sInputString_Z, "=") + 1))
		End With
	Next nThisMoon
End Scope
Close #1

'Make a copy of the initial states
Dim Shared aInitialMoons(1 To 4) As strMoon
aInitialMoons(1) = aMoons(1)
aInitialMoons(2) = aMoons(2)
aInitialMoons(3) = aMoons(3)
aInitialMoons(4) = aMoons(4)

Declare Function DimensionHasCycled(nDimension As Integer) As Boolean
Declare Function gcd(a As Ulongint, b As Ulongint) As Ulongint
Declare Function lcm(a As Ulongint, b As Ulongint) As Ulongint

Dim nTimeStep As Ulongint = 1
Do
	'First update the velocity of every moon by applying gravity
	For nFirstMoon As Integer = 1 To 4-1
		For nSecondMoon As Integer = nFirstMoon+1 To 4
			aMoons(nFirstMoon).V(VEL_X) += Sgn(aMoons(nSecondMoon).V(POS_X) - aMoons(nFirstMoon).V(POS_X))
			aMoons(nSecondMoon).V(VEL_X) += Sgn(aMoons(nFirstMoon).V(POS_X) - aMoons(nSecondMoon).V(POS_X))
			aMoons(nFirstMoon).V(VEL_Y) += Sgn(aMoons(nSecondMoon).V(POS_Y) - aMoons(nFirstMoon).V(POS_Y))
			aMoons(nSecondMoon).V(VEL_Y) += Sgn(aMoons(nFirstMoon).V(POS_Y) - aMoons(nSecondMoon).V(POS_Y))
			aMoons(nFirstMoon).V(VEL_Z) += Sgn(aMoons(nSecondMoon).V(POS_Z) - aMoons(nFirstMoon).V(POS_Z))
			aMoons(nSecondMoon).V(VEL_Z) += Sgn(aMoons(nFirstMoon).V(POS_Z) - aMoons(nSecondMoon).V(POS_Z))
		Next nSecondMoon
	Next nFirstMoon
	'Then update the position of every moon by applying velocity
	For nThisMoon As Integer = 1 To 4
		With aMoons(nThisMoon)
			.V(POS_X) += .V(VEL_X)
			.V(POS_Y) += .V(VEL_Y)
			.V(POS_Z) += .V(VEL_Z)
		End With
	Next nThisMoon
	For nThisDimension As Integer = 0 To 2 'This is because X = 0, Y = 1, Z = 2
		If aLoopCount(nThisDimension) = 0 Then
			If DimensionHasCycled(nThisDimension) Then
				aLoopCount(nThisDimension) = nTimeStep : nDimensionsCycled += 1
				Print "Dimension " & nThisDimension & " looped! " & aLoopCount(nThisDimension)
			Endif
		End If
	Next nThisDimension
	If nDimensionsCycled = 3 Then Exit Do 'All dimensions looped! Exit simulation!
	nTimeStep += 1
Loop

'Now, to see when *all three dimensions* will loop, the Least Common Multiple ( LCM ) of all three is calculated!
nLoopSteps = lcm(lcm(aLoopCount(0), aLoopCount(1)), aLoopCount(2))

Print : Print "Number of steps to repeat the universe: " & nLoopSteps

Function DimensionHasCycled(nDimension As Integer) As Boolean
	For nLocalThisMoon As Integer = 1 To 4
		'all of the moons' *positions* and *velocities* MUST exactly match the initial state (they started with velocities = 0)
		'nDimension+3 because: POS_X = 0, POS_Y = 1, POS_Z = 2, VEL_X = 3, VEL_Y = 4, VEL_Z = 5
		If aMoons(nLocalThisMoon).V(nDimension) <> aInitialMoons(nLocalThisMoon).V(nDimension) Or aMoons(nLocalThisMoon).V(nDimension+3) <> 0 Then Return False
	Next nLocalThisMoon 
	Return True
End Function

'Since overflows occurred, I set *all* the involved variables in these two functions to Ulongint !
Function gcd(a As Ulongint, b As Ulongint) As Ulongint
	If a = 0 Then Return b
	Return gcd(b Mod a, a)
End Function

Function lcm(a As Ulongint, b As Ulongint) As Ulongint
	Return ((a/gcd(a, b)) * b) 'Written this way instead of (a*b)/gcd(a, b) in order to avoid overflow problems
End Function
