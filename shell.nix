let pkgs = (import ./nix/nixpkgs.nix).nixpkgs {}; in
let easyPureScript = import ./nix/pkgs/easy-purescript.nix { inherit pkgs; }; in

let spago2nix = import ./nix/pkgs/spago2nix.nix {
  inherit pkgs;
  inherit (pkgs) dhall-json;
}; in

pkgs.mkShell {
  buildInputs = [
    easyPureScript.inputs.purs
    easyPureScript.inputs.spago
    pkgs.nodejs
    spago2nix
  ];
}
