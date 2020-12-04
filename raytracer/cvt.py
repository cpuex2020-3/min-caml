import sys


# cf) https://www.geeksforgeeks.org/python-program-to-represent-floating-number-as-hexadecimal-by-ieee-754-standard/

def float_bin(my_number, places=3):
    my_whole, my_dec = str(my_number).split('.')
    my_whole = int(my_whole)
    res = (str(bin(my_whole))+'.').replace('0b', '')

    for x in range(places):
        my_dec = str('0.')+str(my_dec)
        temp = '%1.20f' % (float(my_dec)*2)
        my_whole, my_dec = temp.split('.')
        res += my_whole
    return res


def IEEE754(n):
    # identifying whether the number
    # is positive or negative
    sign = 0
    if n < 0:
        sign = 1
        n = n * (-1)
    p = 30
    # convert float to binary
    dec = float_bin(n, places=p)

    dot_place = dec.find('.')
    one_place = dec.find('1')
    # finding the mantissa
    if one_place > dot_place:
        dec = dec.replace('.', '')
        one_place -= 1
        dot_place -= 1
    elif one_place < dot_place:
        dec = dec.replace('.', '')
        dot_place -= 1
    mantissa = dec[one_place+1:]

    # calculating the exponent(E)
    exponent = dot_place - one_place
    exponent_bits = exponent + 127

    # converting the exponent from
    # decimal to binary
    exponent_bits = bin(exponent_bits).replace('0b', '')

    mantissa = mantissa[0:23]

    # the IEEE754 notation in binary
    final = str(sign) + exponent_bits.zfill(8) + mantissa
    return final


with open(sys.argv[1]) as f:
    b = 0xffffffff
    for line in f:
        ints = map(lambda n: float(n) if '.' in n else int(n), line.split())
        for i in ints:
            if type(i) is int:
                if i < 0:
                    i = int(bin(-i ^ b), 2) + 1
                s = '{:032b}'.format(i)
            else:
                s = IEEE754(abs(i))
                if i < 0:
                    s = '1' + s[1:]
            for i in [0, 8, 16, 24]:
                print(int(s[i:i+8], 2))
