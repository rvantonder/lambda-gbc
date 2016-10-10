# IMPORTANT!!!

To RENDER:

Zoom out all the way, and zoom in ONCE.

Code:

`step_frame` on empty requests in loop for FAST

For debugging:



`step_insn` in loop for no req


possible optimization:

make `rrender` work every 10k steps


# Left off:

No debug:
./driver.native --bootrom --speed 0.0

Renders with `step_frame` in non_blocking mode (let rec loop function).
Need to make it render at the right time. Problem is: while rendering,
execution continues, and i can't see the screen moving. any attempt
to slow down or pause execution stops the rendering too: it's in the
same thread.

Solution: correctly model gpu and rendering. Rendering depends on every clock

http://imrannazar.com/GameBoy-Emulation-in-JavaScript:-GPU-Timings

http://www.codeslinger.co.uk/pages/projects/gameboy/lcd.html "ctrl-f sync" "ctrl-f subtract"

The 69905 number comes from here: http://www.codeslinger.co.uk/pages/projects/gameboy/beginning.html



LWT_LOG="debug" ./driver.native --bootrom --speed 0.0


## Hacks

a FRAME step is a RENDERING optimization. if we do `./driver.native --botrom --speed 0.0`, things
will work, but we don't want to render every time.


Hack for now: render every 10k steps. For some reason the boot rom is only about 47k steps,
so the 69k steps isn't getting hit. we will sync this up with the step_frame once
the gpu works correctly.

## Debugger commands:

`(bp 0x86)`

`(print insn)`

`(print regs)`

`(print (mem 0x0))`

`(step insn)`

`(step frame)`

`render`

`pause`

`resume`

`help`

Return: repeat last command.

## Debug SCY, cool example

```
(bp 0x86)
resume
(print (mem 0xFF42))
-> 64
resume
(print (mem 0xFF42))
-> 63
```

## Small fixes for one day

use bap word for breakpoints/pc. not int (see debugger_types.mli)

debugger should nto have 'dump' methods. should return the value
and i should make a separate 'printer' class.

## blah

by default, the interpreter continues with `next_insn` and not `next_frame`,
because with `next_frame`, on resume, the bp gets skipped (even though, in the
ideal implementation, it shouldn't)

In other words, this should work, but doesnt:

```
(bp 0x4)
(step frame)
-> BP triggered!, entering blocking mode (already in blocking mode)
```

Instead it just continues.


Whenever (step frame) is called we will batch-process an entire frame before
listening to any event again. That's why. Even though the bp triggered event is sent,
we are not aborting step frame because it was triggered. correct behavior would do this.

To get this, we would need to listen for evnts after each insn step inside a
frame. Not going to bother. frames are there so we can execute big pieces with a
given syncrhony. only.


## Logging

Only logging under section "ev_*" will be printed, and those that map to debug.

`"ev_* -> debug"`

`LWT_LOG="ev_* -> debug" ./driver.native --bootrom --speed 0.0`

All debug messages:

`LWT_LOG="debug" ./driver.native --bootrom --speed 0.0`

same as

`LWT_LOG="* -> debug" ./driver.native --bootrom --speed 0.0`




## The bug


Bil can be nested:

```
Sep 18 22:14:43: ev_int_dbg_eval: {
Sep 18 22:14:43: ev_int_dbg_eval:   if (~FZ) {
Sep 18 22:14:43: ev_int_dbg_eval:     jmp (0xC:16 + (extend:16[0xFB:8]))
Sep 18 22:14:43: ev_int_dbg_eval:   }
Sep 18 22:14:43: ev_int_dbg_eval: }
```

interpreter #eval is called recursively on structures like the above. First evaluates if, then jmp. So, when
we have something that sends events like 'pause', it may send it multiple times before returning.

this could be fixd by putting it in 'step_insn', which is at least atomic.

But the bigger issue is, should we be using mailbox already, or the queue for lwt?

because the second part of this bug is that two events are added, and then we enter infniloop











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
