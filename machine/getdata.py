#! /usr/bin/python

import sys

#choices = ['2*', '2/', '-', '+', 'and', 'or', 'drop', 'dup', '@+', '@', '@b', '!+', '!', '!b', 'a!', 'b!', 'a', '+*', 'pop', 'push', 'over', 'up', 'down', 'left', 'right', 'nop', '0', '1', '63', '128']

choices = ['@p', '@+', '@b', '@', '!p', '!+', '!b', '!', '+*', '2*', '2/', '-', '+', 'and', 'or', 'drop', 'dup', 'pop', 'over', 'a', 'nop', 'push', 'b!', 'a!', 'lshift', 'rshift']

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
          if (var[0] == "h" or var.find("spec") != -1 or var.find("cand") != -1) and var.find("lit") == -1 :
            hole = int(val[2:to],2)
            print var + " = " + choices[hole]
          elif (var[0] == "h" or var.find("spec") != -1 or var.find("cand") != -1):
            print var + " = " + str(int(val[2:to],2))
          elif print_all:
            print var + " = " + str(int(val[2:to],2))
        elif print_all:
          print var + " = 0" + val[1:to]
    line = f.readline()

if __name__ == "__main__":
  main()
