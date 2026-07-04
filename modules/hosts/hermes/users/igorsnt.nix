# Host-specific user binding for igorsnt on hermes (work macbook)
# This file exists to show where host-specific user overrides go.
# For hermes, the user module (modules/users/igorsnt/) handles everything.
# If igorsnt appeared on another darwin host with different settings,
# those overrides would go here.
{
  ...
}:
{
  # Currently empty — all igorsnt config is in modules/users/igorsnt/
  # If hermes needed host-specific user tweaks (e.g. extra packages only
  # on this machine), they'd go here as:
  #
  # flake.modules.darwin.igorsnt-hermes = { ... }: {
  #   home-manager.users.igorsnt.home.packages = [ ... ];
  # };
}
