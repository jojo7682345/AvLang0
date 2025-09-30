{
	description = "flake description";

	inputs = {
		nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
		avbuilder = {
			type = "github";
			owner = "jojo7682345";
			repo = "avBuilder";
			inputs.nixpkgs.follows = "nixpkgs";
			
		};
	};

	outputs = { self, nixpkgs, ... } @inputs: let
		system = "x86_64-linux";
		pkgs = import nixpkgs { inherit system; };
		avbuilder = inputs.avbuilder.packages.${system}.avbuilder;
	in {
		devShells.${system}.default = pkgs.mkShell {
			nativeBuildInputs = with pkgs; [
				libgcc
				gdb
				avbuilder
			];
			buildInputs = with pkgs; [
			];

			shellHook = ''
  				export CC=gcc
  				export AVBUILDER_HOME=${avbuilder}/share/avBuilder
  				export CXX=g++
			'';
		};
	};
}
