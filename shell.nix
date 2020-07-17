let
  pkgs = import <nixpkgs> {
    overlays = [(import ./overlays.nix)];
  };
  python-custom = (pkgs.python3.withPackages (
        pythonPackages: with pythonPackages; [
          matplotlib
          numpy
          scipy
          pandas
          pillow
          scikitlearn
        ]));
in
  with pkgs;
  mkShell {
    name = "mlShell";
    buildInputs = [
      clojure
      clj-kondo
      openjdk
      python-custom
    ];
    shellHooks = ''
      export PYTHON_PATH=${python-custom}
      export PYTHON_VERSION=${python3.version}
    '';
  }
