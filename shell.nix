let pkgs = import <nixpkgs> {};
in pkgs.mkShell {
  propagatedBuildInputs = with pkgs; [
    # the actual haskell toolstack
    stack

    # Rust Compiler
    cargo

    # python runtime
    pkgs.python3Full
    pkgs.python3Packages.multiprocess
  ];
}
