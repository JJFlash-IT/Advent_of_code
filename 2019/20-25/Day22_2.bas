'Decent explanation: https://github.com/emilysmiddleton/advent-of-code-2019/blob/master/src/day22/day22.md
'Python application of that: https://github.com/elemoine/adventofcode/blob/master/2019/day22/part2.py
'This is mainly a translation exercise (without me knowing Python!), I basically copied the solution :(

Type strOperations
	Operation As String
	Number As Integer
End Type

Dim Shared aOperations(Any) As strOperations
Dim nOperationNumber As Integer = -1

Function RealMod(nNumber1 As Longint, nNumber2 As Longint) As Longint
	'As explained in: https://stackoverflow.com/questions/4467539/javascript-modulo-gives-a-negative-result-for-negative-numbers
	RealMod = nNumber1 Mod nNumber2
	If RealMod < 0 Then RealMod += nNumber2
End Function

Sub Combine(Byref a As Longint, Byref b As Longint, nDeckSize As Const Ulongint)
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
			Case "cut"
				' (x - n) = ((ax + b) - n) = ax + b - n
				b -= aOperations(nThisOperation).Number
			Case "increment"
				' (x * n) = ((ax + b) * n) = n * a * x + n * b
				a *= aOperations(nThisOperation).Number
				b *= aOperations(nThisOperation).Number
		End Select
	Next nThisOperation
	
	a = RealMod(a, nDeckSize)
	b = RealMod(b, nDeckSize)
End Sub

'Find the logarithm of any base - copied STRAIGHT from the Freebasic documentation
Function LogBaseX (ByVal Number As Double, ByVal BaseX As Double) As Double
    LogBaseX = Log( Number ) / Log( BaseX )
End Function

Sub Repeat(Byref a As Longint, Byref b As Longint, nDeckSize As Const Ulongint, nRepetitions As Const Ulongint)
    ' we need to repeat n times
    '
    ' to repeat 2 times: a2, b2 = (a**2) % m, (b * (a + 1)) % m
    ' to repeat 4 times: a4, b4 = (a2**2) % m, (b2 * (a2 + 1)) % m
    ' to repeat 8 times: a8, b8 = (a4**2) % m, (b4 * (a4 + 1)) % m
    ' ...
    ' we repeat 2**logn times, 2**logn times what's left, etc.
	
	Dim As Longint a1 = 1, b1
	Dim As Longint a2, b2
	Dim n2 As Longint = nRepetitions
	Dim nLogn As Longint

/'
    while n2 > 0:
        a2, b2 = a, b
        logn = math.floor(math.log(n2, 2))
        for _ in range(logn):
            a2, b2 = (a2**2) % m, (b2 * (a2 + 1)) % m
        a1, b1 = (a1 * a2) % m, (a1 * b2 + b1) % m
        n2 -= 2**logn
    assert n2 == 0
    return a1, b1
'/
	
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

Function Inverse(a As Longint, b As Ulongint) As Longint
/'
def inverse(a, b):
    m = b
    prevx, x = 1, 0
    prevy, y = 0, 1
    while b:
        q = a // b
        x, prevx = prevx - q * x, x
        y, prevy = prevy - q * y, y
        a, b = b, a % b
    return prevx % m
'/
	Dim nDeckSize As Ulongint = b
	Dim As Longint nPrevX = 1, x = 0
	Dim As Longint nPrevY = 0, y = 1
	Dim q As Longint
	Do While b
		q = a \ b
		nPrevX = x : x = nPrevX - q * x
		nPrevY = y : y = nPrevY - q * y
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
					.Number = Valint(Mid(nTempString, Len("deal with increment ")))
				Case Else     'Cut (rotate)
					.Operation = "rotate"
					.Number = Valint(Mid(nTempString, Len("cut ")))
			End Select
		End With
	Loop Until Eof(1)
Close #1