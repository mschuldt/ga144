#!/bin/bash

FILE=ga

echo "#!/bin/bash" > $FILE
echo "# !!! auto generated by make-ga-script.sh !!!" >> $FILE
echo "emacs --quick --script $(pwd)/src/ga-main.el" '--wd $(pwd) $@' >> $FILE
