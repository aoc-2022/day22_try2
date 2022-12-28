module day22_try2.Utils

let rec gcd (a:int) (b:int) : int =
    if b = 0 then a
    else gcd b (a % b)