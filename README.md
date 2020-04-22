# Color Word Analyzer

This is web app and CLI for analyzing color words in a text. Watch it in action at http://colors.jonreeve.com. Still highly experimental, and not currently ready for public use.

## Running the Project Gutenberg Analysis

(Requires pg-text-7.db, from corpus-db.) 

1. Install Nix, if you don't have it already, with: 

```sh
curl https://nixos.org/nix/install | sh
```

2. Compile the CLI, with: 

``` sh
nix-shell --run "cabal build"
```

3. Run the python script, with: 

```sh
python3 pg-analysis.py
```

## Technical details

This program is written in Haskell, using 

 - Scotty (a web framework)
 - Lucid (an HTML DSL)
 - Clay (a CSS DSL) 
 - PlotlyHS, a Haskell interface to the [Plotly](https://plotly.com/) data visualization library. 
   - I'm actually using [my own fork](https://github.com/JonathanReeve/plotlyhs) of PlotlyHS, which adds a couple of new features.
 
The web interface is the main way of using this program, but it can also be used via CLI or just by using it as a Haskell library. 
