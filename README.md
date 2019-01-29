# diceware-auto


## Synopsis

Randomly generate Diceware passphrases


## Description

Against everyone's better judgement, this is an automated Diceware passphrase generator. This software simulates rolling 5 six-sided dice and chooses the corresponding word in a Diceware word list. Words selected this way can comprise a passphrase that's not hard to memorize and yet very difficult to discover with brute-force.

As of 2019-Jan it's recommended to use 96 bits of entropy for your most sensitive passphrases. For Diceware this translates into 8 words in length which is the default value for this program. Bits of entropy will be calculated and displayed during execution. For more information on password strength, see [this Wikipedia page](https://en.wikipedia.org/wiki/Password_strength)

This software is obviously less secure than rolling five 6-sided dice and looking up the words in a paper word list. But it's quite a bit more convenient. It does make some small attempt to be safer by using the cryptonite library to generate random values for die rolls.

For more information on the Diceware system of passphrase generation, please see the [Diceware Passphrase Home Page](http://world.std.com/~reinhold/diceware.html)  Here you can also find links to word list files in languages other than English. The standard English Diceware word list has been compiled into this software and can be printed to stdout if desired. Other word list files are supported as well via an option.


## Getting source

Source code is available from github at the [diceware-auto](https://github.com/dino-/diceware-auto) project page.

Build with stack

    $ stack build
    $ stack exec diceware-auto


## Contact

Dino Morelli <dino@ui3.info>
