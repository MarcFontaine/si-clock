# An interface to the Silicon Labs Si5351 clock chip

![Si5351 Board](https://i.imgur.com/5DI1b1l.jpg)

The Si5351 is Silicon Labs I2C-programmable any-frequency CMOS clock generator and VCXO.
It can generate any frequency from ca 8kHz to 160MHz.
The Si5351 uses a programmable rational clock divider.

## Why yet an other SI5351 library ?

First of all it is hackable in Haskell.
And second, almost all of the other open source Si5351 libraries
use a fixed denominator for the clock divider.
In other words, 20 of the 58 bits, that set the clock divider, are hard-coded in
library.
As the IC always uses a combination of two rational divider stages,
a total of 2*20 bits = 40 bits, that the carefully designed hardware provides,
are lost by the software design.
That means "40 bits less resolution and more jitter".

Instead of a fixed denominator, this library uses
[continued fractions](https://en.wikipedia.org/wiki/Continued_fraction)
to compute the (theoretically best)
numerator-denominator pair for the clock divider.
(TO DO investigate if this makes any difference in practice.)

## Examples
The library contains examples for:

* Frequency synthesis
* Hopping frequencies
* CW generation (Morse code)
* Transmitting JT65
* RTTY / FSK

## [Haddock documentation](http://hackage.haskell.org/package/si-clock)
