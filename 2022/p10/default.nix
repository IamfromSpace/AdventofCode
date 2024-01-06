# clash-wavedrom does not work with singletons-3.x (but does not include the appropriate constraint)
# singletons-2.7 requires GHC 8.10.x

let
  # Pin nixpkgs for full determinism
  pkgs =
    import (
      builtins.fetchTarball {
        # This one gets us pretty far, but digits is broken
        # name = "nixos-21.11";
        # url = "https://github.com/nixos/nixpkgs/archive/a7ecde854aee5c4c7cd6177f54a99d2c1ff28a31.tar.gz";

        # name = "nixos-22.11";
        # url = "https://github.com/nixos/nixpkgs/archive/ea4c80b39be4c09702b0cb3b42eab59e2ba4f24b.tar.gz";

        # name = "nixos-23.05";
        # url = "https://github.com/nixos/nixpkgs/archive/7790e078f8979a9fcd543f9a47427eeaba38f268.tar.gz";

        name = "nixos-23.11";
        url = "https://github.com/nixos/nixpkgs/archive/04220ed6763637e5899980f98d5c8424b1079353.tar.gz";
      }
    ) {};

  # clash-cores is in a subdirectory, so we fetch first (which
  # returns a string of its location) and then that string can
  # be manipulated later to get the subdirectory.
  clash-compiler-src-from-stack-yaml =
    builtins.fetchTarball
      "https://github.com/clash-lang/clash-compiler/archive/e9242c36b11f2e2a287622fc76af207be64b36e2.tar.gz";

  clash-compiler-src-1-8-1 =
    builtins.fetchTarball
      "https://github.com/clash-lang/clash-compiler/archive/3f5dc67c0e526e43a4dd88eb3902e39ed512c166.tar.gz";

  clash-compiler-src-1-6-3 =
    builtins.fetchTarball
      "https://github.com/clash-lang/clash-compiler/archive/91bec8ae577ad45cb4e268f12c331893e88276eb.tar.gz";

  clash-compiler-src-1-6-4 =
    builtins.fetchTarball
      "https://github.com/clash-lang/clash-compiler/archive/f7d56f1f091324f8ed709c8e3a655ac7145fddc9.tar.gz";

  clash-compiler-src-1-6-5 =
    builtins.fetchTarball
      "https://github.com/clash-lang/clash-compiler/archive/231031d00c809306208348269c0b954d9bb35d6d.tar.gz";

  clash-compiler-src-1-6-6 =
    builtins.fetchTarball
      "https://github.com/clash-lang/clash-compiler/archive/5a214f06d4e1604fe83ad67161450d3e95a9479b.tar.gz";

  aoc = 
    pkgs.haskell.packages.ghc8107.developPackage {
      root = ./.;
      source-overrides = {
        clash-cores =
          "${clash-compiler-src-1-6-6}/clash-cores";

        clash-prelude = "1.6.6";
          # "${clash-compiler-src-1-6-5}/clash-prelude";

        clash-lib = "1.6.6";
          # "${clash-compiler-src-1-6-5}/clash-lib";

        clash-prelude-hedgehog = "1.6.6";
          # "${clash-compiler-src-1-6-5}/clash-prelude-hedgehog";

        clash-ghc = "1.6.6";
          # "${clash-compiler-src-1-6-5}/clash-ghc";

        # WE ARE HERE: tests don't pass
        # They're unrelated to the part we use
        # Notably, they _do_ pass with LTS-18.28
        clash-utils =
          builtins.fetchTarball
            "https://github.com/iamfromspace/clash-utils/archive/0fa598841af87aa4c4af934bb3ee26a1d3bb9c0c.tar.gz";

        clash-wavedrom =
          builtins.fetchTarball
            # My branch that fixes tests with aeson-2.1
            "https://github.com/iamfromspace/clash-wavedrom/archive/7aabc5f9f173896c439ba6c977065cb227f536ba.tar.gz";

        # For clash-wavedrom
        singletons = "2.7";

        # For singletons 2.7
        th-desugar = "1.11";

        # For clash-utils test to pass
        # hashable = "1.3.5.0";

        # An experiment to test clash-utils bounds
        # This one fails:
        # hashable = "1.4.3.0";

        # For clash-utils test to pass
        # This one works!
        hashable = "1.4.2.0";

        # For hashable-1.3.0.0
        # WIP: Shoot!  Infinite recursion
        # random = "1.2.0";

        # For hashable-1.3.5.0
        # OneTuple = "0.3.1";

        # This one seems to work with 21.11 default packages
        # tasty-hedgehog =
        #   builtins.fetchTarball
        #     # 1.2.0.0
        #     "https://github.com/qfpl/tasty-hedgehog/archive/729617f82699be189954825920d6f30985e1cfa7.tar.gz";

        # doctest-parallel =
        #   builtins.fetchTarball
        #     # 0.2.6
        #     "https://github.com/martijnbastiaan/doctest-parallel/archive/b54c3f3dec08def41dea476f090da8c07961a20f.tar.gz";
        # This one seems to work with 21.11 default packages
        # tasty =
        #   (builtins.fetchTarball
        #     # 1.4.2.1
        #     "https://github.com/UnkindPartition/tasty/archive/7379d3101f165a582aa3341084fbeaae9828875e.tar.gz") + "/core";
        # tasty =
        #   (builtins.fetchTarball
        #     # 1.3.1
        #     "https://github.com/UnkindPartition/tasty/archive/0039c17a0fe251803fcb8208554d080718311fe5.tar.gz") + "/core";
        # tasty = 
        #   (builtins.fetchTarball
        #     # 1.2.3
        #     "https://github.com/UnkindPartition/tasty/archive/1254246a728d244e1cbb0a56ea7bbccb203b0cc6.tar.gz") + "/core";
        # This pattern doesn't work here (but should be able to be used elsewhere)
        # singletons =
        #   pkgs.haskellPackages.callHackage
        #     "singletons"
        #     "2.7"
        #     {};
      };
    };
in
  aoc
