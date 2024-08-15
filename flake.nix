{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    devenv = {
      url = "github:cachix/devenv";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, devenv, flake-utils, ... } @inputs:
		flake-utils.lib.eachDefaultSystem(system:
			let pkgs = nixpkgs.legacyPackages.${system}; in
      {
				devShells.default = devenv.lib.mkShell {
          inherit inputs pkgs;

          modules = [{
            languages.zig.enable = true;
            # packages = with pkgs; [
            #
            # ];
          }];
				};
			}
		);
}
# vim: sw=2 ts=2

