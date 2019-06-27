let mkEasyPureScript = pkgs: import ./nix/pkgs/easy-purescript.nix {
  inherit pkgs;
}; in

let mkSpago2nix = pkgs: import ./nix/pkgs/spago2nix.nix {
  inherit pkgs;
  inherit (pkgs) dhall-json;
}; in

{ pkgs ? (import ./nix/nixpkgs.nix).nixpkgs {}
, easyPureScript ? mkEasyPureScript pkgs
, spago2nix ? mkSpago2nix pkgs
}:

let spagoPackages = import ./spago-packages.nix { inherit pkgs; }; in

{
  inherit pkgs;

  pureswift = pkgs.stdenv.mkDerivation {
    name = "pureswift";
    src = pkgs.stdenv.lib.sourceByRegex ./. [
      "^packages.dhall$"
      "^spago.dhall$"
      "^((src|test)(\/[^\/]+)*)(\.purs)?$"
    ];
    buildInputs = [
      easyPureScript.inputs.purs
      spago2nix
    ];
    buildPhase = ''
      ${spagoPackages.installSpagoStyle}
      ${spagoPackages.buildSpagoStyle} "src/**/*.purs" "test/**/*.purs"
      purs bundle './output/*/*.js' --module PSW --main PSW --output psw
    '';
    # doCheck = true;
    checkInputs = [
      pkgs.nodejs
    ];
    checkPhase = ''
      node -e 'require("./output/Test.Main").main()'
    '';
    installPhase = ''
      mkdir -p $out/bin
      echo "#!/usr/bin/env node" | cat - psw > tmp && mv tmp psw
      chmod +x psw
      mv psw $out/bin
    '';
  };
}
