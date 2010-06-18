# Running time on the x10 sampleFile.txt:
#   real    0m3.794s
#   user    0m3.599s
#   sys     0m0.176s

import re
import sys

f = open(sys.argv[1],'r')
fileString=f.read()
f.close

finalCount={}
for word in re.split('\W+', fileString.lower()):
  finalCount[word] = finalCount.get(word,0)+1

f = open(sys.argv[2],'w')
for n in sorted([(count,word) for (word,count) in finalCount.items()]):
  f.write(str(n) + "\n")
f.close()
