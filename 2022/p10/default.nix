let
  # Pin nixpkgs for full determinism
  pkgs =
    import (
      builtins.fetchTarball {
        name = "nixos-23.11";
        url = "https://github.com/nixos/nixpkgs/archive/04220ed6763637e5899980f98d5c8424b1079353.tar.gz";
      }
    ) {};

  # clash-cores is in a subdirectory, so we fetch first (which
  # returns a string of its location) and then that string can
  # be manipulated later to get the subdirectory.
  clash-compiler-src-1-6-6 =
    builtins.fetchTarball
      "https://github.com/clash-lang/clash-compiler/archive/5a214f06d4e1604fe83ad67161450d3e95a9479b.tar.gz";

  aoc = 
    pkgs.haskell.packages.ghc8107.developPackage {
      root = ./.;
      source-overrides = {
        clash-cores =
          "${clash-compiler-src-1-6-6}/clash-cores";

        # Tests fail for GHC > 8.10.7
        clash-prelude = "1.6.6";

        clash-lib = "1.6.6";

        clash-prelude-hedgehog = "1.6.6";

        clash-ghc = "1.6.6";

        clash-utils =
          builtins.fetchTarball
            "https://github.com/iamfromspace/clash-utils/archive/0fa598841af87aa4c4af934bb3ee26a1d3bb9c0c.tar.gz";

        # clash-wavedrom does not work with singletons-3.x
        # (but does not include the appropriate constraint)
        clash-wavedrom =
          builtins.fetchTarball
            # My branch that fixes tests with aeson-2.1
            "https://github.com/iamfromspace/clash-wavedrom/archive/7aabc5f9f173896c439ba6c977065cb227f536ba.tar.gz";

        # For clash-wavedrom
        # singletons-2.7 requires GHC 8.10.x
        singletons = "2.7";

        # For singletons 2.7
        th-desugar = "1.11";

        # For clash-utils
        hashable = "1.4.2.0";
      };
    };
in
  aoc
