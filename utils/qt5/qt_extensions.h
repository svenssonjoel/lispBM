/*
  Copyright 2026 Joel Svensson  svenssonjoel@yahoo.se

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

#ifndef QT_EXTENSIONS_H_
#define QT_EXTENSIONS_H_

class QLbmContainerWidget;

// Register the root widget area that LBM extensions will operate on.
// Must be called before lbm_qt_extensions_init().
void lbm_qt_extensions_set_widget(QLbmContainerWidget *widget);

// Register all qt-* extensions with the LispBM runtime.
// Call after lbm_init() and before lbm_run_eval().
//
// Layout / structure:
//   (qt-root)                                        -> handle
//   (qt-widget-add-container  ch layout)             -> handle  layout: 'vbox 'hbox 'grid
//   (qt-widget-add-stretch    ch ['horizontal])      -> handle
//   (qt-widget-remove         ch)                    -> t
//   (qt-widget-set-max-width  ch w)                  -> t
//   (qt-widget-set-min-width  ch w)                  -> t
//
// Display:
//   (qt-widget-add-display    ch w h)                -> handle
//   (qt-set-display           handle)                -> t
//
// Input widgets:
//   (qt-widget-add-button     ch label)              -> handle
//   (qt-set-button-label      handle label)          -> t
//   (qt-widget-add-checkbox   ch label)              -> handle
//   (qt-widget-add-radio      ch label)              -> handle
//   (qt-widget-add-spinbox-i  ch min max)            -> handle
//   (qt-widget-add-spinbox-f  ch min max step)       -> handle
//   (qt-widget-add-textfield  ch [placeholder])      -> handle
//
// Value access (checkbox, radio, spinbox-i/f, textfield):
//   (qt-widget-get-value      handle)                -> t/nil | int | float | byte-array
//   (qt-widget-set-value      handle val)            -> t
//
// Trailing attribute args accepted by all add-* calls:
//   '(max-width N)  '(min-width N)  '(max-height N)  '(min-height N)
//   '(pos-x N)      '(pos-y N)   (grid placement; ignored for non-grid containers)
//
// Events sent to the LispBM event handler:
//   (button-pressed   handle)
//   (checkbox-changed handle t/nil)
//   (radio-changed    handle)
//   (spinbox-changed  handle value)
//   (textfield-commit handle text)
void lbm_qt_extensions_init(void);

#endif
