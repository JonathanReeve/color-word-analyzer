# Color Word Analyzer

This is web app for analyzing color words in a text. Watch it in action at http://colors.jonreeve.com.

## Running

Install Nix, then: 

``` sh
nix-shell --run "cabal run"
```

## Technical details

This program is written in Haskell, using 

 - Scotty (a web framework)
 - Lucid (an HTML DSL)
 - Clay (a CSS DSL) 
 - PlotlyHS, a Haskell interface to the [Plotly](https://plotly.com/) data visualization library. 
   - I'm actually using [my own fork](https://github.com/JonathanReeve/plotlyhs) of PlotlyHS, which adds a couple of new features.
 
The web interface is the main way of using this program, but it can also be used via CLI or just by using it as a Haskell library. 
