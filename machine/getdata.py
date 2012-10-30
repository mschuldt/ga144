#! /usr/bin/python

import sys

dic = {}

def main():
  filename = sys.argv[1]
  f = open(filename)
  line = f.readline()
  if len(sys.argv) > 2 and sys.argv[2] == "-h":
    print_all = False
  else:
    print_all = True

  while line:
    tokens = line.lstrip().split(" ")
    if len(tokens) > 0:
      #print tokens[0]
      if tokens[0] == "(define-fun":
        var = tokens[1]
        line = f.readline()
        tokens = line.lstrip().split(" ")
        val = tokens[0]
        to = val.find(")")
        
        begin = var.find("_") + 1
        end = var.find("_", begin)
        if end == -1:
          end = len(var)
        if end-begin < 2:
          var = var[:begin] + "0" + var[begin:]

        if val[1] == "b":
          if var[0] == "h":
            hole = int(val[2:to],2)
            if hole == 0:
              inst = "2*"
            elif hole == 1:
              inst = "2/"
            elif hole == 2:
              inst = "-"
            elif hole == 3:
              inst = "+"
            elif hole == 4:
              inst = "and"
            elif hole == 5:
              inst = "or"
            elif hole == 6:
              inst = "drop"
            elif hole == 7:
              inst = "dup"
            elif hole == 8:
              inst = "@+"
            elif hole == 9:
              inst = "@"
            elif hole == 10:
              inst = "@b"
            elif hole == 11:
              inst = "!+"
            elif hole == 12:
              inst = "!"
            elif hole == 13:
              inst = "!b"
            elif hole == 14:
              inst = "a!"
            elif hole == 15:
              inst = "b!"
            elif hole == 16:
              inst = "up"
            elif hole == 17:
              inst = "down"
            elif hole == 18:
              inst = "left"
            elif hole == 19:
              inst = "right"
            elif hole == 20:
              inst = "nop"
            elif hole == 21:
              inst = "1"
            print var + " = " + inst
          elif print_all:
            print var + " = " + str(int(val[2:to],2))
        elif print_all:
          print var + " = 0" + val[1:to]
    line = f.readline()

if __name__ == "__main__":
  main()
