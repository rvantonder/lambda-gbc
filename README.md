## Design

```
driver.ml # initialize image/parse bootrom options
   |
runner.ml # start the interpreter loop and debugger
   |________________
  /                 \
input_loop         interpreter_loop
|                  |
|                   ` processes all debug actions will enter blocking lwt mode when debug stepping.
parse send commands to interpreter recv stream
```

## Boot

`DMG_ROM.bin` is the original boot rom that would get loaded at `0x0000`, run
the Nintendo logo check routine, and set PC to `0x100` to start running the
game cartridge. Note that the bootrom only sets up the initial state. Once it
is set up, the values in `0x0000-0x100` are flushed out and replaced by the
cartridge.

`DMG_ROM.bin` has the boot sequence, but not the logo data. Setting PC to 0 and
executing will cause the ROM to lock up here: `JR NZ,$fe; $00e9;if not a match,
lock up here`. That's because the boot rom is separate from the cartridge, and
it compares logo data in the cartridge to the DMG rom values.

To pass the check, I manually copied the logo data from somewhere to make
`DMG_ROM_WITH_SCREEN.bin` which passes the test. This does everything as before
like `DMG_ROM.bin`, but displays the nintendo screen, as it comes down in an
animation. Basically I stole enough of the nintendo logo from a different
cartridge and just embedded it into the rom image.

To play with the boot instructions: run `DMG_ROM_WITH_SCREEN.bin`. Ignore the
pop-up. Then set PC to 0. Then `f9` to run. Or step.

Ref:

http://bgb.bircd.org/pandocs.htm#powerupsequence


## Initial state

If you load tetris in no$gmb, you'll see that our emulator register state is
very close to the initial state (breakpoint at 0x101) in the emulator. That's
good news. The 'cheat' is to just set this initial register state and skip
doing the actual boot rom.

Source:

http://marc.rawer.de/Gameboy/Docs/GBCPUman.pdf Page 17
http://bgb.bircd.org/pandocs.htm#powerupsequence

Test initial state with something like:

```
./driver.native --di --nr -v --file /tmp/q
                 ` at every step
                              ` use dummy file, not the default SCREEN one (execution corrupts starting values)
                          `-dump state

```


## Debugging with tetris rom

Load the tetris rom and set BP at 0x100 and 0x101.


## Options

Run with the boot file (initial state is 0 for registers and memory):

`./driver.native --bootrom`

Set the speed per frame:

`./driver.native --speed 1.0`

Dump the state after each instruction

`./driver.native --di`

Run k steps

`./driver.native -k 0xbb34`

Hex dump and disassemble at offset

`./driver.native --hd --disas 0x100`


# TODO

Get it to work fully on bootrom, with updating Nintendo logo.

Debug mode:

`./driver.native --bootrom --speed 1.0 --di`

Then switch to tetris.



# Thoughts

Added sleep/wake threads above the two threads that get joined. I can pause/resume once. But after that,
no idea.

BUT! can i create a sleep/resume pair and then send the thread to the frame_loop?



# Left off:

running `./driver.native --bootrom --speed 1.0 ` and testing debugger.

Debugger commands: (print insn), (print regs), pause, resume, helpde
