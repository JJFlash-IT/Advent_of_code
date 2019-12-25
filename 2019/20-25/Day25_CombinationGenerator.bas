'https://www.geeksforgeeks.org/print-all-possible-combinations-of-r-elements-in-a-given-array-of-size-n/  (method 2)

Dim aDay25Items(0 To 5) As String => {"coin", "whirled peas", "fixed point", "prime number", "antenna", "weather machine" }

Declare Sub combinationUtil(arr() As String, n As Integer, r As Integer, index As Integer, DataA() As String, i As Integer)
  
' The main function that prints all 
' combinations of size r in arr[]  
' of size n. This function mainly  
' uses combinationUtil()  
Sub printCombination(arr() As String, n As Integer, r As Integer)  
    'A temporary array to store  
    'all combination one by one  
    Dim dataA(r-1) As String  
  
    ' Print all combination using  
    ' temprary array 'data[]'  
    combinationUtil(arr(), n, r, 0, DataA(), 0)    
End Sub

'arr[] ---> Input Array  
'n ---> Size of input array  
'r ---> Size of a combination to be printed  
'index ---> Current index in data[]  
'data[] ---> Temporary array to store current combination  
'i ---> index of current element in arr[]

Sub combinationUtil(arr() As String, n As Integer, r As Integer, index As Integer, DataA() As String, i As Integer)
    ' Current cobination is ready, print it  
    if (index = r) Then
		For J As Integer = 0 To r-1
			Print DataA(J) & ", " ;
		Next J
		Print
		Return
    Endif
  
    ' When no more elements are there to put in data[]  
    if (i >= n) Then return
  
    ' current is included, put next at next location  
    dataA(index) = arr(i)  
    combinationUtil(arr(), n, r, index + 1, dataA(), i + 1)
  
    ' current is excluded, replace it with next (Note that  
    ' i+1 is passed, but index is not changed)  
    combinationUtil(arr(), n, r, index, dataA(), i+1)  
End Sub

printCombination(aDay25Items(), 6, 4)

'Winning combo: 
'- whirled peas
'- fixed point
'- prime number
'- antenna