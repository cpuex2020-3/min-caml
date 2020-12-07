import sys
import struct
import binascii

with open(sys.argv[1]) as f:
    b = 0xffffffff
    for line in f:
        ints = map(lambda n: float(n) if '.' in n else int(n), line.split())
        for i in ints:
            if type(i) is int:
                if i < 0:
                    i = int(bin(-i ^ b), 2) + 1
                s = '{:032b}'.format(i)
                for i in [0, 8, 16, 24]:
                    print(int(s[i:i+8], 2))
            else:
                s = str(binascii.hexlify(struct.pack('f', i)), 'utf-8')
                for i in [6, 4, 2, 0]:
                    print(int(s[i:i+2], 16))
