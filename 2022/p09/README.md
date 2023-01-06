# Building

Install `apio`:

```
pip install -U apio
```

Compile our verilog:

```
stack run clash -- Aoc --verilog
```

We'll copy all our verilog into the apio directory which lets us then do everything all at once (assuming the board is plugged in):

```
cp ../verilog/Aoc.topEntity/*v && apio build -v && apio upload
```

The `-v` can be omitted, but it's useful to see the statistics about our build printed, such as LC & RAM usage or max clock speed.

# Interacting

Install picocom:

```
sudo apt install picocom
```

To interactively send and receive data, start picocom like this:

```
picocom -b ${BAUD_RATE} --omap crlf --imap lfcrlf ${SERIAL_DEVICE}
```

Where for the Alchitry Cu, the typically device name is `/dev/ttyUSB0` or `/dev/ttyUSB1`.
The `omap` and `imap` options convert typical unix line breaks (also used in our FPGA code) into typical serial protocol line breaks.

In this mode, you can use Ctrl+D, you can send an EOT symbol.

To send a file from stdin, use the following command:

```
cat ${INPUT_FILE_NAME} | picocom -b ${BAUD_RATE} -qrx 1000 ${SERIAL_DEVICE}
```

This opens the channel for 1s (since picocom doesn't know when we're done) and then writes back to stdout (which can be piped elsewhere).
Note that we may need to put this symbol `` at the end of the file manually, and the device may need to be reset after each run due to the file's final newline.
This is because we need something that indicates the end of the file to the onboard logic, and we typically go with the EOT char.
