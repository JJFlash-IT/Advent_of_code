function toNumber as LONG (sNumberString as STRING * 5) STATIC
    toNumber = 0
    for wIndex as WORD = 1 to len(sNumberString)
        toNumber = toNumber * 10
        toNumber = toNumber + (peek(@sNumberString + wIndex) - $30 )
    next wIndex
end function

Dim sBuffer as STRING * 5
Dim lThisCalorieSum as LONG
Dim lGreatestCalorieSum(3) as LONG
lThisCalorieSum = 0
lGreatestCalorieSum(0) = 0
lGreatestCalorieSum(1) = 0
lGreatestCalorieSum(2) = 0

poke 646, 1 'white text
open 2,8,2,"input01.txt,p,r"
    do
        input #2, sBuffer
        if sBuffer = "" then
            For bSumSlot as BYTE = 0 to 2
                if lThisCalorieSum > lGreatestCalorieSum(bSumSlot) then
                    if bSumSlot < 2 then lGreatestCalorieSum(2) = lGreatestCalorieSum(1)
                    if bSumSlot < 1 then lGreatestCalorieSum(1) = lGreatestCalorieSum(0)
                    lGreatestCalorieSum(bSumSlot) = lThisCalorieSum
                    exit for
                end if
            next bSumSlot
            lThisCalorieSum = 0
        else
            lThisCalorieSum = lThisCalorieSum + toNumber(sBuffer)
        end if
    loop until st() = 64 'EOF
    print lGreatestCalorieSum(0)
    print lGreatestCalorieSum(1)
    print lGreatestCalorieSum(2)
    print "sum of the three most-calories elves: " : print lGreatestCalorieSum(0) + lGreatestCalorieSum(1) + lGreatestCalorieSum(2)
close 2
