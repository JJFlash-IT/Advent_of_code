function toNumber as LONG (sNumberString as STRING * 5) STATIC
    toNumber = 0
    for wIndex as WORD = 1 to len(sNumberString)
        toNumber = toNumber * 10
        toNumber = toNumber + (peek(@sNumberString + wIndex) - $30 )
    next wIndex
end function

Dim sBuffer as STRING * 5
Dim lThisCalorieSum as LONG
Dim lGreatestCalorieSum as LONG
lThisCalorieSum = 0 : lGreatestCalorieSum = 0

poke 646, 1 'white text
open 2,8,2,"input01.txt,p,r"
    do
        input #2, sBuffer
        if sBuffer = "" then
            if lThisCalorieSum > lGreatestCalorieSum then
                lGreatestCalorieSum = lThisCalorieSum
            end if
            lThisCalorieSum = 0
        else
            lThisCalorieSum = lThisCalorieSum + toNumber(sBuffer)
        end if
    loop until st() = 64 'EOF
    print "the most calories on a single elf are: " : print lGreatestCalorieSum
close 2
