echo /usr/local/bin/gcc ${1}.S -o ${1}${2} -g -L../nucleus/checkout/bin/linux.i686 -lnu_sd -Wl,-rpath ../nucleus/checkout/bin/linux.i686 -g -Wl,-T ../nucleus/checkout/scripts/linker/elf32_i386.x;
/usr/local/bin/gcc ${1}.S -o ${1}${2} -g -L../nucleus/checkout/bin/linux.i686 -lnu_sd -Wl,-rpath ../nucleus/checkout/bin/linux.i686 -g -Wl,-T ../nucleus/checkout/scripts/linker/elf32_i386.x
 
