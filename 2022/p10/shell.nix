{ pkgs ? import <nixpkgs> {} }:
  pkgs.mkShell {
    nativeBuildInputs =
      [ # Used for the current synthesis
        # TODO: It seems like this requires an impure install of its own tools
        # Don't forget to setup USB permissions for programming
        pkgs.apio

        # haskell stuff
        pkgs.stack
        pkgs.haskell.compiler.ghc8107

        # Low level alternative to apio (WIP)
        # Don't forget to setup USB permissions for programming
        pkgs.icestorm
        pkgs.yosys
        pkgs.nextpnr

        # Interact with the FPGA over UART serial
        # Don't forget to add "dialout" to your user's groups!
        pkgs.picocom

        # Commands like lsusb for trouleshooting usb devices
        pkgs.usbutils
      ];
  }
