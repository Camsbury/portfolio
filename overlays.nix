self: super: {
  clj-kondo = super.callPackage (import ./kondo.nix) {};
}
