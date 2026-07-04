.PHONY: home home-build home-switch os os-build os-switch darwin darwin-build darwin-switch

home: home-build

home-build:
	nh home build --no-nom .

home-switch:
	nh home switch --no-nom .

os: os-build

os-build:
	nh os build --no-nom .

os-switch:
	nh os switch --no-nom .

darwin: darwin-build

darwin-build:
	darwin-rebuild build --flake .

darwin-switch:
	darwin-rebuild switch --flake .

update:
	nix flake update
