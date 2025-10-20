
# LispBM REPL with I2S (Audio) in Chibios for stm32f407-discovery boards

The purpose of this example is to get people who want to try/use 
LispBM on an STM32 based system and do some audio experimentation!
Note that this is a place to start! Not a plug and play audio generator.
Quite a bit of stuff is left for you to figure out!

The example is targeting ChibiOS 21.11.3.
The dev-board used during development is the stm32f407-disc1 but
it can probably quite easily be ported to other stm32f4 boards.
The example uses USB-CDC to provide a REPL. If your board does not
have this kind of USB wired up, you need to rewrite the example to
use an UART for interaction.

The STM32F407G-DISC1 board has a built-in cs43L22 DAC hooked up to I2S3
(PC7, PC10, PC12, PA4). This example does not use that DAC. This
example is meant for the CS4344 DAC connected to I2S2 using as follows:

MCLK -> PC6
LRCK -> PB9
SCLK -> PB10
SDIN -> PB15

If you are using the PMOD I2S2 then connect the upper set of pins of the J1 connector
as follows:

 | PMOD I2S2 J1 pin | STM32F4 pin |
 |------------------|-------------|
 | 1                | PC6         |
 | 2                | PB9         |
 | 3                | PB10        |
 | 4                | PB15        |
 | 5                | GND         |
 | 6                | 3V          | 


The example provides an extension for playing simple sine wave to
the I2S dac:

1. `(i2s-tone freq duration)`

For example `(i2s-tone 440 5000)` plays an A for 5 seconds.  

The example also shows how to set up image-storage on flash. When up
and running you interact with the image via the extensions:

1. `(image-clear)` - clear the image and reboot the system
2. `(image-save)`  - Save the environment into the image.
3. `(reset)` - resets the board in an image-safe way.

Saving the image requires that there is a main function. The main function
is launched automatically upon reboot.

Note that LBM images are not portable across changes to the FW!
So if you have an image stored on the flash, but you uploaded an updated firmware,
then LBM will (possibly/likely) not be able to load the image. Run the
`erase-image.sh` script to clear the image and be able to boot again.

# Requirements:

1. ChibiOS 21.11.3 located at `../../../ChibiOS_21.11.3` relative to this example (or modify the Makefile)
2. arm-none-eabi-gcc compiler suite. 
3. an I2S DAC CS4344

# Building

run:

```
make
```

# Flashing onto discovery board

run:

```
./flash.sh
```

