# diceware-auto


## Synopsis

Randomly generate Diceware passphrases


## Description

Against everyone's better judgement, this is an automated Diceware passphrase generator. This software simulates rolling 5 six-sided dice and chooses the corresponding word in a Diceware word list. Words selected this way can comprise a passphrase that's not hard to memorize and yet very difficult to discover with brute-force.

As of 2019-Jan it's recommended to use 96 bits of entropy for your most sensitive passphrases. Using a list of 7776 words, this translates into 8 words in length which is the default value for this program. Bits of entropy will be calculated and displayed during execution. For more information on password strength, see [this Wikipedia page](https://en.wikipedia.org/wiki/Password_strength)

This software is obviously less secure than rolling five 6-sided dice and looking up the words in a paper word list. But it's quite a bit more convenient. It does make some small attempt to be safer by using the cryptonite library to generate random values for die rolls.

WORD LISTS

Three stock word lists have been compiled into this software and can be retrieved to stdout with the `--dump` option. The possible choices for the -L|--list switch are:

  improved  A combination of the original Diceware list plus something called the Beale list minus non-letters, abbreviations, and more. See [this page](http://www.webplaces.org/passwords/diceware-criticism.htm) for info. This is the default list.
  diceware  The original Diceware word list. See the [Diceware Passphrase Home Page](http://world.std.com/~reinhold/diceware.html)
  eff       [The EFF's New Wordlists for Random Passphrases](https://www.eff.org/deeplinks/2016/07/new-wordlists-random-passphrases)

Any other value will be interpreted as a path to a word list file of your choosing. These can be found at some of the links above. Handy if you need a language other than English.


## Getting source

Source code is available from github at the [diceware-auto](https://github.com/dino-/diceware-auto) project page.

Build with stack

    $ stack build
    $ stack exec diceware-auto


## Contact

Dino Morelli <dino@ui3.info>
