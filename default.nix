{ pkgs ? import <nixpkgs> {} }:
pkgs.stdenv.mkDerivation rec {
  pname = "unroller";
  version = "0.1.0";

  src = ./.;

  buildInputs = [
    pkgs.llvmPackages_14.libllvm
    #pkgs.llvm_14
    pkgs.gnumake
    pkgs.llvmPackages_14.libclang
    pkgs.llvmPackages_14.clang
  ];

  buildPhase = ''
    make
  '';

  installPhase = ''
    mkdir -p $out/bin 
    mv LoopConvert $out/bin 
  '';
}
