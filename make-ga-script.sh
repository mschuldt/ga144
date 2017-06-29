#!/bin/bash

echo "#!/bin/bash" > ga
echo "emacs --quick --script $(pwd)/src/ga-main.el" '--wd $(pwd) $@' >> ga
