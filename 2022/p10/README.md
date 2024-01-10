## Dependencies

Install [nix](https://nixos.org/download).
Nix automatically supplies all other dependencies for all other purposes.

## Create the Binary

All build steps to get the final FPGA binary are handled by nix, simply run the following from the root of the project.

```bash
nix-build
```

## Program the FPGA

### Permissions

In order to program the Alchitry Cu, you'll need permissions to the correct USB device.
You can do this each time the device is plugged in by using `chmod`:

```bash
# Give all users read and write access to the FPGA's USB
sudo chmod 666 /dev/bus/usb/${bus_number}/${device_number}
```

However, these effects are purely temporary.
You'll also need to locate the correct USB bus and device number (`nix-shell --run lsusb` can help with this; look for something like "Ltd FT2232C/D/H Dual UART/FIFO IC").

To grant permanent access for USB, you can add a udev rule, so that when the Alchitry Cu is plugged in, it already has read and write access for all users.
In NixOS, something like this would go in your configuration file:

```nix
  services.udev.extraRules = ''
    # FT2232 onboard Alchitry Cu FPGA
    ATTRS{idVendor}=="0403", ATTRS{idProduct}=="6010", MODE="0666", ENV{ID_MM_DEVICE_IGNORE}="1", ENV{ID_MM_PORT_IGNORE}="1"
    '';
```

In this example, broader permissions are set when the vendor and product id of the USB device match the Alchitry Cu's debug chip.

### Program

Once permissions are granted, use nix to run the programming command in an environment where our dependencies are made automatically available:

```
nix-shell --run "iceprog result"
```

## Interacting

### Permissions

To interact with the FPGA over the serial connection, you'll need permission to interact with the correct serial device.

You can do this each time the device is plugged in by using `chmod`:

```bash
# Give all users read and write access to the FPGA's virtual serial port over USB
sudo chmod 666 /dev/ttyUSB${number}
```

However, these effects are purely temporary.
For permanent access to the serial ports, you likely just want to add the user to the `dialout` group.
In NixOS you can add "dialout" to `users.users.${you}.extraGroups` (a list of groups) or use `sudo usermod -a -G dialout $USERNAME` in other distros.

### Full Interactivity

To interactively send and receive data, start picocom like this:

```
nix-shell --run "picocom -b ${BAUD_RATE} --omap crlf --imap lfcrlf ${SERIAL_DEVICE}"
```

Where for the Alchitry Cu, the typically device name is `/dev/ttyUSB0` or `/dev/ttyUSB1`.
The `omap` and `imap` options convert typical unix line breaks (also used in our FPGA code) into typical serial protocol line breaks.

In this mode, you can use Ctrl+D, you can send an EOT symbol.

### Sending Files

To send a file from stdin, use the following command:

```
nix-shell --run "cat ${INPUT_FILE_NAME} | picocom -b ${BAUD_RATE} -qrx 1000 ${SERIAL_DEVICE}"
```

This opens the channel for 1s (since picocom doesn't know when we're done) and then writes back to stdout (which can be piped elsewhere).
Note that we may need to put this symbol `` at the end of the file manually, and the device may need to be reset after each run due to the file's final newline.
This is because we need something that indicates the end of the file to the onboard logic, and we typically go with the EOT char.
