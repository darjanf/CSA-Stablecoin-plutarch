# Plutarch Template

# Set up nix config 
Put the following lines in your nix configuration file (usually located at /etc/nix/nix.conf)
extra-experimental-features = nix-command flakes ca-derivations
extra-trusted-substituters = https://cache.iog.io https://cache.nixos.org/ https://public-plutonomicon.cachix.org https://cache.zw3rk
extra-trusted-public-keys = cache.iog.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= public-plutonomicon.cachix.org-1:3AKJMhCLn32gri1drGuaZmFrmnue+KkKrhhubQk/CWc=

# Installation 
After setting up nix config, restart your computer or VM. 
Then run:
    nix develop

# Running Hoogle
Hoogle is a Haskell API search engine. To run Hoogle locally for the Plutarch project, run the following command inside the development environment:

```
[nix develop:~/plutarch-template]$ hoogle server --local --port=8085
```

This will start a local Hoogle server at http://localhost:8085/.

# License
See the [LICENSE](LICENSE) file for license rights and limitations (MIT).