# Building

## Permissions

In order to program the Alchitry Cu, you'll need permissions to the correct USB device.
To interact with it over the serial connection, you'll need permission to interact with the correct serial device.

You can do this each time the device is plugged in by using `chmod`:

```bash
# Give all users read and write access to the FPGA's USB
sudo chmod 666 /dev/bus/usb/${bus_number}/${device_number}

# Give all users read and write access to the FPGA's virtual serial port over USB
sudo chmod 666 /dev/ttyUSB${number}
```

However, these effects are purely temporary.
You'll also need to locate the correct USB bus and device number (`lsusb` can help with this).

To grant permanent access for USB, you can add a udev rule, so that when the Alchitry Cu is plugged in, it already has read and write access for all users.
In NixOS, something like this would go in your configuration file:

```nix
  services.udev.extraRules = ''
    # FT2232 onboard Alchitry Cu FPGA
    ATTRS{idVendor}=="0403", ATTRS{idProduct}=="6010", MODE="0666", ENV{ID_MM_DEVICE_IGNORE}="1", ENV{ID_MM_PORT_IGNORE}="1"
    '';
```

In this example, broader permissions are set when the vendor and product id of the USB device match the Alchitry Cu's debug chip.

For permanent access to the serial ports, you likely just want to add the user to the `dialout` group.
In NixOS you can add "dialout" to `users.users.${you}.extraGroups` (a list of groups) or use `sudo usermod -a -G dialout $USERNAME` in other distros.

## Dependencies

Install `apio`:

```
pip install -U apio
```

## Build

Compile our verilog:

```
stack run clash -- Aoc --verilog
```

We'll copy all our verilog into the apio directory which lets us then do everything all at once (assuming the board is plugged in):

```
cd apio
cp ../verilog/Aoc.topEntity/*v . && apio build -v && apio upload
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
