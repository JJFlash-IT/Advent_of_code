'Decent explanation: https://github.com/emilysmiddleton/advent-of-code-2019/blob/master/src/day22/day22.md
'Python application of that: https://github.com/elemoine/adventofcode/blob/master/2019/day22/part2.py
'This is mainly a translation exercise (without me knowing Python!), I basically copied the solution :(

'Big Integer Library: https://github.com/stephanbrunker/big_integer
#Include "big_integer.bi"

Type strOperations
	Operation As String
	Number As Longint
End Type

Dim Shared aOperations(Any) As strOperations
Dim Shared nOperationNumber As Integer = -1

Function RealMod(nNumber1 As Bigint, nNumber2 As Longint) As Longint
	'As explained in: https://stackoverflow.com/questions/4467539/javascript-modulo-gives-a-negative-result-for-negative-numbers
	Dim nResult As Longint = nNumber1 Mod nNumber2
	If nResult < 0 Then nResult += nNumber2
	RealMod = nResult
End Function

Sub Combine(Byref a As Longint, Byref b As Longint, nDeckSize As Const Longint)
    ' dealintonewstack (-1 - x) mod m
    ' cut (x - n) mod m
    ' dealwithincrement (x * n) mod m
    '
    ' example: m = 10, x = 2
    ' - cut 6 : 2 - 6 mod 10 = -4 mod 10 = 6
    ' - deal with increment 7 = 6 * 7 mod 10 = 2
    ' - deal into new stack : -1 - 2 mod 10 = 7
    
	a = 1 : b = 0
	For nThisOperation As Integer = 0 To nOperationNumber
		Select Case aOperations(nThisOperation).Operation
			Case "reverse"
				' (-1 - x) = (-1 - (ax + b)) = -ax + (-b - 1)
				a = -a
				b = -b - 1
			Case "rotate"
				' (x - n) = ((ax + b) - n) = ax + b - n
				b -= aOperations(nThisOperation).Number
			Case "increment"
				' (x * n) = ((ax + b) * n) = n * a * x + n * b
				a *= aOperations(nThisOperation).Number
				b *= aOperations(nThisOperation).Number
		End Select
		a = RealMod(a, nDeckSize)
		b = RealMod(b, nDeckSize)
	Next nThisOperation
End Sub

'Find the logarithm of any base - copied STRAIGHT from the Freebasic documentation
Function LogBaseX (ByVal Number As Longint, ByVal BaseX As Longint) As Double
    LogBaseX = Log( Number ) / Log( BaseX )
End Function

Sub Repeat(Byref a As Longint, Byref b As Longint, nDeckSize As Const Longint, nRepetitions As Const Longint)
    ' we need to repeat n times
    '
    ' to repeat 2 times: a2, b2 = (a**2) % m, (b * (a + 1)) % m
    ' to repeat 4 times: a4, b4 = (a2**2) % m, (b2 * (a2 + 1)) % m
    ' to repeat 8 times: a8, b8 = (a4**2) % m, (b4 * (a4 + 1)) % m
    ' ...
    ' we repeat 2**logn times, 2**logn times what's left, etc.
	
	Dim As Bigint a1 = 1, b1
	Dim As Bigint a2, b2
	Dim n2 As Longint = nRepetitions
	Dim nLogn As Longint
	
	Do While n2 > 0
		a2 = a : b2 = b
		nLogn = Int(LogBaseX(n2, 2))
		For nThisIter As Integer = 1 To nLogn
			b2 = RealMod(b2 * (a2 + 1), nDeckSize)
			a2 = RealMod(a2^2, nDeckSize)
		Next nThisIter
		b1 = RealMod(a1 * b2 + b1, nDeckSize)
		a1 = RealMod(a1 * a2, nDeckSize)
		n2 -= 2 ^ nLogn
	Loop
	If n2 = 0 Then Print , "n2 is zero!" Else Print , "n2 is NOT zero!"
	
	a = a1 : b = b1
End Sub

Function Inverse(a As Longint, b As Longint) As Longint
'Original Python:
'	def inverse(a, b):
'		m = b
'		prevx, x = 1, 0
'		prevy, y = 0, 1
'		while b:
'			q = a // b
'			x, prevx = prevx - q * x, x            [ x = prevx - q * x     prevx = x ]   
'			y, prevy = prevy - q * y, y            [ y = prevy - q * y     prevy = y ]
'			a, b = b, a % b                        [ a = b                 b = a % b ]
'		return prevx % m

	Dim nDeckSize As Longint = b
	Dim As Longint nPrevX = 1, x = 0
	Dim As Longint nPrevY = 0, y = 1
	Dim q As Longint
	Dim As Longint nPythonExpr1, nPythonExpr2
	Do While b
		q = a \ b

		'in Python, the expressions on the right-hand side are all evaluated first before any of the assignments take place
		'https://stackoverflow.com/a/11502290
		nPythonExpr1 = nPrevX - q * x
		nPythonExpr2 = x
		x = nPythonExpr1 : nPrevX = nPythonExpr2
		
		nPythonExpr1 = nPrevY - q * y
		nPythonExpr2 = y
		y = nPythonExpr1 : nPrevY = nPythonExpr2
		
		nPythonExpr1 = b
		nPythonExpr2 = RealMod(a, b)
		a = nPythonExpr1 : b = nPythonExpr2
	Loop
	Inverse = RealMod(nPrevX, nDeckSize)
End Function

Dim nTempString As String
Open Exepath + "\input22.txt" For Input As #1
	Do
		nOperationNumber += 1
		Redim Preserve aOperations(nOperationNumber)
		Line Input #1, nTempString
		With aOperations(nOperationNumber)
			Select Case Left(nTempString, 6)
				Case "deal i" 'Deal into new stack (reverse)
					.Operation = "reverse"
					'.Number stays at zero
				Case "deal w" 'Deal with increment
					.Operation = "increment"
					.Number = Vallng(Mid(nTempString, Len("deal with increment ")))
				Case Else     'Cut (rotate)
					.Operation = "rotate"
					.Number = Vallng(Mid(nTempString, Len("cut ")))
			End Select
		End With
	Loop Until Eof(1)
Close #1

Dim As Longint nDeckSize, nCardPosition, nShuffles
Dim As Longint a, b, i
'TEST using Part 1 solution... my answer: 2514
nDeckSize = 10007 : nCardPosition = 2514 : nShuffles = 1
Combine(a, b, nDeckSize)
Print "After combine: " & a ; b
Repeat(a, b, nDeckSize, nShuffles)
Print "After repeat: " & a ; b
i = Inverse(a, nDeckSize)
Print "Inverse: " & i
Print "This *must* be equal to 2019: " & RealMod((nCardPosition - b) * i, nDeckSize)
Print

'REAL
nDeckSize = 119315717514047 : nCardPosition = 2020 : nShuffles = 101741582076661
Dim nBigSolution As Bigint

Combine(a, b, nDeckSize)
Print "After combine: " & a ; b
Repeat(a, b, nDeckSize, nShuffles)
Print "After repeat: " & a ; b
i = Inverse(a, nDeckSize)
Print "Inverse: " & i
Print "(nCardPosition - b): " & (nCardPosition - b)
nBigSolution = CBig((nCardPosition - b)) * CBig(i)
Print "(nCardPosition - b) * i: " & nBigSolution
Print : Print "And the solution is..." ; : Color 15 : Print RealMod(nBigSolution, nDeckSize) : Color 7
Print
