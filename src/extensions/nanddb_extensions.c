/*
    Copyright 2026 Joel Svensson        svenssonjoel@yahoo.se

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/


#include "nanddb_extensions.h"

/* Database for NAND flash

   NAND flash properties:
   - Large eraase size (Blocks).
   - Writes should be done in page-sized chunks (Pages).
   - Blocks and Pages are not the same size.
   - Expected to come with "bad blocks" from factory.
   - Blocks are expected to turn bad over the lifetime of the memory.
   - Depends on ECC in order to reach adverticed number
     of erase cycles.
   - Almost guaranteed to come with its own ECC implementation.

   Desired DB capabilites:
   - Storage of (key . value) pairs.
     - These will have a 32bit key (typically a 28bit lispbm symbol)
     - Values: I am not sure yet what values to support.
       Alternatives:
         - Value is always a byte array (flat value for example).
         - support for unboxed LBM values?
   - Storing of time series. (timestamp . value) pairs.
   - Possible key format
      [4 bit semmantics | 28 bit value ]
        - Fits perfect for 28bit symbols
        - Timeseries entries will be identified by a bit being set in
          the semmantics area.
        - Should timeseries have an "overwrite if full policy?"

   Storage sizes:
   - How big things should we allow to be stored in the DB,
     - total size of entry (including any headers, crcs and so on AT MOST = 1 page)?
     - Small entries will be accumulated in a ram buffer.

   Protocol for RAM buffer handling:
   - Flushed to FLASH when full.
   - What if there is M bytes left in buffer and I want to write M+N bytes.
     - Maybe: calculate the CRC and write the buffer to flash.
              Then in new buffer put the larger value.
              This leaves an unused hole in flash that will be
              compacted upon GC.
   - The last 16bits of a page is a crc. The crc is
     the last thing written.

   Protocol for knowing complete writes:
   - Crc is present for block and correct.

   Protocol for deletion:
   - Tombstone entries.

   Protocol for Bad block management.
   - I dont know yet.

   Protocol for Garbage collection:
   - I dont know yet.
   - Garbage collection takes time and should be done in
     an incremental way. (expose a step function).
     - Read operations should be allowed during GC (perhaps?).

   Protocol for lookup:
   - Search from newest entry towards oldest.
   - How do we know in what order to look at blocks?
     Some kind of sequence numbering is needed.

   Protocol for Wear leveling
   - Sequence number

   Robustness against crash/loss of power:
   - Data in ram-buffer is lost.
   - Partial write of ram-buffer makes a corrupt entry in the flash
     that is not correctly terminated with a CRC.
      - This should likely trigger a GC that would repair the state
        but loose the partial data.
        Can we detect this state upon boot and start a repair.

 */


void lbm_nanddb_extensions_init(void) {

}
