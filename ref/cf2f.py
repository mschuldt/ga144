#!/usr/bin/python

# based on: https://github.com/mangpo/chlorophyll/blob/master/cforth_tools/cf2f.py
# Modified to output in .aforth format

'''\
simple colorforth to forth translator

see Charley Shattuck's post for the general idea:
http://www.strangegizmo.com/forth/ColorForth/msg00209.html
in addition, we will use [compile] in front of cyan words

also, we will use angle brackets, e.g. < macro >, around words defined in the
kernel to mark those with no tag, otherwise words outside of definitions
are assumed to be yellow (executed).

markup_ functions are helpers, to "mark up" the bare words with
indicators as to their "color", i.e. syntax, in those cases where positional
or other clues are insufficient.
'''
import sys, os, struct, re
try:
 from cfword import *
except:  # this should work with pytest
 sys.path.append('.')
 import cfword
 from cfword import *
# http://colorforth.com/parsed.html
# => https://mschuldt.github.io/www.colorforth.com/parsed.html

MARKUP = {  # tagname patterns to trigger markup_
    'highlevel': [
        ['^compilemacro$', 'macro'],  # must go before compile
        ['^compile', 'compile'],
        ['^execute', 'execute'],
        ['^definition$', 'definition'],  # set state to 'compile'
        ['^text', 'text'],
        ['^variable$', 'variable'],
        ['.*word$', 'numeric'],  # this check must come *after* compile and execute
        ['^formatting$', 'formatting'],  # "blue words": cr, br, indent, etc.
        ['^commented', 'commented'],  # new "commented number" tag 0xf
        ['^feedback', 'feedback'],  # new "compiler feedback number" tag 0xd
    ],
    'binary': [
        ['^extension$', 'bareword'],
    ]
}

SIGNBIT = (1 << 26)
MAXSHORT = SIGNBIT - 1  # 27 bits max colorforth "short" integer
INT = 0xffffffff
DEFAULT = ['execute']  # default action, reset at start of each block
# bit 27 is the sign bit, so the above is the largest positive number

block_num = 0
def cf2f(infile = sys.stdin, output = sys.stdout):
 global block_num
 blocks = getbinary(infile)
 for index in range(18, len(blocks)):
  block_num = index
  DEFAULT[:] = ['execute']  # reset default action
  print >>output, '( block %d )' % index
  block = getwords(blocks[index])
  debug('unpacking block %d' % index, 2)
  unpack_all(block)
  output_text(block, output)

def output_text(block, output):
 for i in range(len(block)):
  if block[i][0] == ':' and not nocr(block, i - 1):
   output.softspace = False
   print >>output
  if block[i][1] != 'formatting':
   print >>output, block[i][0],
  if block[i][0] in ['cr', 'br', 'indent'] and block[i][1] == 'formatting':
   output.softspace = False
   print >>output
   if block[i][0] == 'br':  # double newline
    print >>output
   elif block[i][0] == 'indent':
    print '    ',  # 4 spaces plus one added by Python
 print >>output  # end of block
 print >>output

def nocr(block, index):
 return padblock(block)[index][0] == '-cr'

def is_empty(block):
 if len(block):
  if type(block[0]) is list:
   empty = [['[0x0]', None]] * len(block)
  else:
   empty = [0] * len(block)
  return block == empty
 else:
  return True

def unpack_number(block, index):
 'unpack one- or two-word number and return updated index'
 tag = TAGS[block[index] & 0xf]
 if tag.endswith(('short', 'long')):
  hex = bool(block[index] & 0x10)
  debug('unpacking number %s, tag=%s, hex=%s' % (block[index], tag, hex), 2)
  block[index] >>= 5
  if tag.endswith('long'):
   if block[index] != 0 or not block[index + 1:]:
    block[index] = [unpack_binary(TAGS.index(tag) | (hex << 4)), None]
    return index + 1, True
   else:
    block.pop(index)  # puts value in current slot
  if hex:
   block[index] = ['0x%x' % (sign_extend(block[index], tag) & INT), tag]
  else:
   block[index] = ['%d' % block[index], tag]
  return index + 1, True
 else:
  return index, False

def sign_extend(number, tag):
 if tag.endswith('short') and number > MAXSHORT:
  number |= -SIGNBIT
 return number

def unpack_all_binary(block, index = 0):
 'go through raw numbers and unpack as [0xn] or untagged strings'
 while not is_empty(block[index:]):
  unpacked = unpack(block[index])
  if unpacked[1] != 'extension':
   unpacked = [unpack_binary(block[index]), None]
  block[index] = unpacked
  index += 1
 block[index:] = []
 markup(block, 'binary')

def unpack_all(rawblock, index = 0):
 'go through raw numbers and unpack into number and text strings'
 block = list(rawblock)  # copy in case we find it's not high-level forth
 while not is_empty(block[index:]):
  if padblock(block)[index - 1][1] == 'variable':
   block[index] = [unpack_binary(block[index]), None]
   index += 1
  else:
   index, processed = unpack_number(block, index)  # try as number first
   if not processed:
    block[index] = unpack(block[index])
    index += 1
   if block[index - 1][1] == None:
    debug('marking binary at %s' % block[:index + 1], 2)
    return unpack_all_binary(rawblock, index = 0)
 block[index:] = []  # trim trailing zeros
 if not connect_extensions(block):
  return unpack_all_binary(rawblock, index = 0)
 markup(block, 'highlevel')
 rawblock[:] = block

word_type_host = False

reversed_words = [ "node", "org", "," ]#, "/b", "/a", "/io", "/p", "/stack" ]

def markup(block, blocktype = 'highlevel', index = 0):
 '''add syntactic cues for text-mode rendition of colorforth

    each markup_ function will return an index and offset;
    the index is where the next markup_ function will act, and
    the offset is the number of words added beyond the index.
    a special offset of None will indicate to the markup loop
    to skip any further processing.'''
 global word_type_host
 word_type_host = False
 while block[index:]:
  item, word, tag, adjust = block[index], block[index][0], block[index][1], 0
  debug('marking up: %s' % item, 2)
  if word == "host":
   word_type_host = True
  elif word == "target":
   word_type_host = False
  elif not block_num %2 and  word in reversed_words and tag == 'executeword':
   block[index-1], block[index] = block[index], block[index-1]

  for pattern in MARKUP[blocktype]:
   if re.compile(pattern[0]).match(tag or 'NO_MATCH'):
    debug('marking up as %s' % pattern[1], 2)
    index, offset = eval('markup_' + pattern[1])(block, index)
    if offset == None:
     break
    else:
     adjust += offset
  debug('adjusting index %d by %d' % (index, adjust + 1), 2)
  index += adjust + 1

def markup_formatting(block, index):
 'signify a formatting word to differentiate format cr from kernel cr'
 #block.insert(index, ['|', 'markup'])
 #return index + 1, None
 return index, None

def markup_feedback(block, index):
 'signify a "compiler feedback" number'
 debug('compiler feedback number: %s' % block[index], 2)
 block.insert(index, ['(', 'markup'])
 block.insert(index + 2, [')', 'markup'])
 return index + 2, None

def markup_commented(block, index):
 'signify a "commented" number rather than one compiled or executed'
 block.insert(index, ['(', 'markup'])
 block.insert(index + 2, [')', 'markup'])
 return index + 2, None

def markup_definition(block, index):
 'put colon before defined word'
 block.insert(index, ['::' if word_type_host else ':', 'markup'])
 DEFAULT[-1] = 'compile'
 return index + 1, None  # skip over colon

def markup_variable(block, index):
 debug('marking up variable %s' % block[index][0], 2)
 block.insert(index, [':var', 'markup'])
 index += 1
 if block[index + 1:] and block[index + 1][0] == '[0x0]':
  block.pop(index + 1)
  return index, None
 else:
  return index + 1, None  # skip "binary" number following

def markup_macro(block, index):
 debug('marking up macro %s' % block[index][0], 2)
 block.insert(index, ['[compile]', 'markup'])
 return index + 1, None

def markup_textallcaps(block, index):
 assert False
 word, marked_up = block[index][0], block[index][0].upper()
 if marked_up in [word.lower(), word.capitalize()]:
  debug('explicitly marking %s as allcaps' % block[index], 2)
  block.insert(index, ['^^', 'markup'])
  index += 1
 block[index][0] = marked_up
 return index  # note this should NOT return tuple

def markup_textcapitalized(block, index):
 assert False
 word, marked_up = block[index][0], block[index][0].capitalize()
 debug('as is, upper, capital: %s' % [word, word.upper(), marked_up], 2)
 if marked_up in [word.lower(), word.upper()]:
  debug('explicitly marking %s as capitalized' % block[index], 2)
  block.insert(index, ['^', 'markup'])
  index += 1
 block[index][0] = marked_up
 return index  # note this should NOT return tuple

def markup_text(block, index):
 debug('marking up text %s' % block[index][0], 2)
 block.insert(index, ['(', 'markup'])
 index += 1
 while block[index:] and block[index][1].startswith('text'):
  if block[index][1] == 'textallcaps':
   index = markup_textallcaps(block, index)
  elif block[index][1] == 'textcapitalized':
   index = markup_textcapitalized(block, index)
  index += 1
 block[index - 1][0] += ')'
 return index - 1, None  # point to last commented word

def markup_execute(block, index):
 if DEFAULT[-1] == 'compile':
  debug('switching default to "execute" at %s' % end(block, index), 2)
  block.insert(index, ['', 'markup'])
  DEFAULT[-1] = 'execute'
  index += 1
  return index, 0
 elif padblock(block)[index + 1][1] == 'definition':
  block.insert(index + 1, ['', 'markup'])
  DEFAULT[-1] = 'compile'
  return index, 1
 else:
  return index, 0

def end(block, index):
 'end of current part of block, for debugging'
 return block[max(0, index - 9):index + 1]

def markup_compile(block, index):
 if DEFAULT[-1] == 'execute':
  debug('switching default to "compile" at %s' % end(block, index), 2)
  block.insert(index, ['', 'markup'])
  DEFAULT[-1] = 'compile'
  index += 1
 if block[index][0] == ';':
  debug('switching default to "execute" at %s' % end(block, index), 2)
  DEFAULT[-1] = 'execute'
 return index, 0

def padblock(block):
 'pad so that block[i-1] or block[i+1] will always see [[None, None]] at end'
 return block + [[None, None]]

def connect_extensions(block, index = 0, ok = True):
 '''join extension[s] to previous word

    due to binary value following a variable declaration (magenta word),
    a variable name cannot have an extension. otherwise how could
    colorforth know whether it were an extension or the value?'''
 debug('connecting extensions in block: %s' % block, 2)
 while block[index:]:
  tag = block[index][1]
  if tag == 'extension':
   debug('found bad tag at: %s' % end(block, index), 2)
   ok = False
   break
  elif tag is None or tag.endswith(('variable', 'short', 'long')):
   index += 1  # none of these can have extensions
   continue
  while padblock(block)[index + 1][1] == 'extension':
   block[index][0] += block.pop(index + 1)[0]
  index += 1
 return ok

def is_decimal(string):
 'determine if string represents a decimal number'
 return re.compile('^-?[0-9]+$').match(string)

def markup_numeric(block, index):
 'mark a word that looks like a number'
 if is_decimal(block[index][0]) and block[index][1].endswith('word'):
  debug('marking up "%s" as numeric-looking word' % block[index], 2)
  block.insert(index, ['$', 'markup'])
  return index + 1, None
 else:
  debug('skipping markup of "%s" to number' % block[index], 2)
  return index, 0

def markup_bareword(block, index):
 block.insert(index, ['<', 'markup'])
 index += 1
 while block[index:] and block[index][1] == 'extension':
  index += 1
 block.insert(index, ['>', 'markup'])
 return index + 1, None

def getbinary(filename, data = ''):
 'concatenate block data from files or stdin'
 if type(filename) == str:
  input = open(filename, 'rb')
 else:
  input = filename
 data += input.read()
 input.close()
 if not data:
  data = sys.stdin.read()
 blocks = [data[n:n + 1024] for n in range(0, len(data), 1024)]
 return blocks

def getwords(block):
 words = list(struct.unpack('<%dl' % (len(block) / 4), block))
 return words

if __name__ == '__main__':
 print "( -*- mode: aforth -*- )"
 cf2f( *sys.argv[1:] )
