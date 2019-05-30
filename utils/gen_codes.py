
    # Copyright 2019 Joel Svensson	svenssonjoel@yahoo.se

    # This program is free software: you can redistribute it and/or modify
    # it under the terms of the GNU General Public License as published by
    # the Free Software Foundation, either version 3 of the License, or
    # (at your option) any later version.

    # This program is distributed in the hope that it will be useful,
    # but WITHOUT ANY WARRANTY; without even the implied warranty of
    # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    # GNU General Public License for more details.

    # You should have received a copy of the GNU General Public License
    # along with this program.  If not, see <http://www.gnu.org/licenses/>.


import huffman
import collections

symchars = "abcdefghijklmnopqrstuvwxyz"
numchars = "0123456789"
funchars = "+-*/=<>."
lispnames = ["lambda", "if", '\'', "list", "quote", "closure", "define", "let",
             "cons", "car", "cdr","(", "((",  ")", "))", ")))", "))))", "nil", "TOGGLE_COMPRESS"]

def total_bits(c) :
    sum = 0;
    for code in c:
        sum += len(code[1])
    return sum    

def minimum_total_bits_codes() :
    min_total_num_bits = 1000000
    min_codes = []
    r = [1, 10, 100, 1000, 10000, 100000]
    space = [ (x,y,z,v) for x in r for y in r
                          for z in r for v in r]
    for point in space :
        
        numchars_w  = [(x, point[0] / len(numchars)) for x in numchars]
        symchars_w  = [(x, point[1] / len(symchars)) for x in symchars]
        funchars_w  = [(x, point[2] / len(funchars)) for x in funchars]
        lispnames_w = [(x, point[3] / len(lispnames)) for x in lispnames]
        all_w = symchars_w + lispnames_w + funchars_w + numchars_w
        codes = huffman.codebook(all_w).items()
        size = total_bits(codes)
        # print("smallest: % d current: % d\n" % (min_total_num_bits, size)  )
        if size < min_total_num_bits :
            min_total_num_bits = size
            min_codes = codes

    return (min_total_num_bits, min_codes)


def make_c_array() :
    codes = minimum_total_bits_codes()
    num_codes = len(codes[1])

    print("Total number of bits %d\n" % codes[0])
    
    code_map = ''

    first = True;
    
    for code in codes[1] :
        if (first) :
            code_map = '{ \"' + code[0] + '\", \"' + code[1] + '\" }' + code_map
            first = False
        else :
            code_map = '{ \"' + code[0] + '\", \"' + code[1] + '\" },\n' + code_map
          
    
    c_str = 'char *codes[' + str(num_codes) + '][2] = {' + code_map + '};\n'

    return c_str

    
    
    
