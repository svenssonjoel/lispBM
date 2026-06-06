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
// Extensions registered:
//   (qt-add-display w h)                   -> handle
//   (qt-set-display handle)                -> t
//   (qt-add-button label)                  -> handle
//   (qt-set-button-label handle label)     -> t
//   (qt-add-container layout-sym)          -> handle  (layout-sym: 'vbox 'hbox 'grid)
//   (qt-widget-add-display  ch w h)        -> handle  (add display to container ch)
//   (qt-widget-add-button   ch label)      -> handle  (add button to container ch)
//   (qt-widget-add-container ch layout)    -> handle  (add container to container ch)
void lbm_qt_extensions_init(void);

#endif
