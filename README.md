# multi-test
### _ava fox_

an unpolished trivial-gamekit experiment

does no error checking, and is very rough around the edges

## Building

- `git clone https://github.com/compufox/mini-test ~/common-lisp`
- `git clone https://github.com/compufox/trivial-gamekit-ui ~/common-lisp`
- `git clone https://github.com/compufox/trivial-gamekit-colors ~/common-lisp`
- ensure you have the borodust quicklisp distribution installed in your lisp image
- `cd ~/common-lisp/mini-test; make`

## Running

After opening once instance of the game (either in a repl or opening the built executable),
click the button that says "host".

Open another instance of the game and click "Join"

in each window you should now see a blue square following your cursor, and a red square that matches
where the cursor's position in the other window

when youre done, hit quit.

## License

GPLv3

