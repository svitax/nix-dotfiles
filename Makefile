.PHONY: home home-build home-switch os os-build os-switch

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

update:
	nix flake update
