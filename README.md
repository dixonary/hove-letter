# hove-letter

An implementation of the brilliant game *Love Letter*, written in Haskell.

![](/image-top.png)

## Installation

#### From Source
1. Install Haskell Stack onto your system through the package manager or the Haskell Stack website.
2. Grab the source code:
    ```sh
    git clone https://github.com/dixonary/hove-letter
    ```
3. Enter the directory:
    ```sh
    cd hove-letter
    ```
4. Build and run the code:
    ```sh
    stack build && stack exec love-letter
    ```


#### From Linux Release
0. Make sure you have LibGMP installed, against which Haskell binaries are dynamically linked. Chances are good that you do.
1. Download the binary from the Releases page here on GitHub.
2. Run the binary directly:
    ```sh
    ./love-letter
    ```


## Caveats

There are 3 steps to writing good Haskell code:
1. Make it work.
2. Make it simple.
3. Make it readable.

The code here is very much *stage-one code*. There aren't a million comments.

The AI is also exceptionally stupid, in that it will always play its held card (unless that is an illegal move). Making the AI better is left to the reader.