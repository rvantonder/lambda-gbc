# run 0x20 steps with verbose option
./driver.native -k 0x20 --di --v --nr

# debug interpreter 5 steps, don't render
./driver.native -k 0x5 --di --nr

# render at 0xbb34
./driver.native -k 0xbb34 -v

# lots of output, no render
./driver.native -v --di --nr

# hex dump and disassemble options
./driver.native --hd --disas 0x100

# Unimplemented improve
#./driver.native --disas=0x204 -f tests/cpu_instrs/individual/01-special.gb
