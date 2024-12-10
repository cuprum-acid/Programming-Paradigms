# Jupyter setup for Racket development:

First, install Jupyter Notebook or Jupyter Lab (both would work for our purposes). In the guide below we will assume Jupyter Lab.

1. You need to have a Python environment installed with `pip`.
2. Using `pip` proceed to install Jupyter Lab (or Jupyter Notebook): `pip install jupyterlab` .
3. Verify that Jupyter Lab is installed by running `jupyter lab` in the command line.

Now that we have Jupyter installed, we proceed to install the Jupyter kernel for Racket.

1. You need to have Racket installed with the Raco package manager. Verify that you can run `raco` before you proceed.
2. Install the package for iracket: `raco pkg install iracket` .
3. Register the iracket kernel in Jupyter: `raco iracket install` .

And that's it! You can now use Racket as a language in your Jupyter Notebooks.
