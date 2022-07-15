{pkgs ? import <nixpkgs> {}}:
let
  sbt8 = pkgs.sbt.override { jre = pkgs.jdk8; };
in
pkgs.mkShell
  {
    nativeBuildInputs = with pkgs; [sbt8 openjdk8 metals];
  }
